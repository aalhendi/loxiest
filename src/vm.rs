use std::hint::unreachable_unchecked;

use crate::{
    ALLOCATE,
    chunk::OpCode,
    compiler::compile,
    memory::{free_objects, reallocate},
    object::{
        NativeFn, Obj, ObjBoundMethod2, ObjClass2, ObjClosure, ObjInstance2, ObjNative, ObjString,
        ObjType, ObjUpvalue, native_clock2,
    },
    table::Table,
    value::Value,
};

const FRAMES_MAX: usize = 64;
const STACK_MAX: usize = FRAMES_MAX * (u8::MAX as usize + 1);

pub enum InterpretResult {
    CompileError,
    RuntimeError,
}

#[repr(C)]
pub struct CallFrame {
    pub closure: *mut ObjClosure,
    pub ip: *mut u8,
    pub slots: *mut Value,
}

#[repr(C)]
pub struct VM {
    pub frames: [CallFrame; FRAMES_MAX],
    pub frame_count: i32,

    pub stack: [Value; STACK_MAX],
    pub stack_top: *mut Value,
    pub globals: Table,
    pub strings: Table,
    pub init_string: *mut ObjString,
    pub open_upvalues: *mut ObjUpvalue, // Intrusive linked list

    pub bytes_allocated: usize,
    pub next_gc: usize,
    pub objects: *mut Obj, // Intrusive linked list head
    pub gray_count: i32,
    pub gray_capacity: i32,
    // assume full responsibility for this array, including allocation failure.
    // If we can’t create or grow the gray stack, then we can’t finish the garbage collection.
    // this is bad news for the VM, but fortunately rare since the gray stack tends to be pretty small
    // TODO(aalhendi): do something more graceful
    pub gray_stack: *mut *mut Obj,
}

/// Enum to control the execution flow of the interpreter loop.
enum LoopControl {
    Continue,
    Stop,
}

impl VM {
    fn reset_stack(&mut self) {
        self.stack_top = self.stack.as_mut_ptr();
        self.frame_count = 0;
        self.open_upvalues = std::ptr::null_mut();
    }

    pub fn init(&mut self) {
        self.reset_stack();
        self.objects = std::ptr::null_mut();
        self.bytes_allocated = 0;
        // TODO(aalhendi): Tune next_gc and dynamic array initial capacity
        self.next_gc = 1024 * 1024; // arbitrary - goal is to not trigger the first few GCs too quickly but also to not wait too long.

        self.gray_count = 0;
        self.gray_capacity = 0;
        self.gray_stack = std::ptr::null_mut();

        self.globals.init();
        self.strings.init();

        // GC now reads vm.initString. field initialized from result of copyString().
        // But copying string allocs memory, which can trigger a GC.
        // If collector ran at just the wrong time, it would read vm.initString before it had been initialized.
        // So, first we zero the field out & clear ptr before freeObjects() in free();
        self.init_string = std::ptr::null_mut();
        self.init_string = Obj::copy_string("init".as_bytes(), 4);

        self.define_native("clock", native_clock2);
    }

    pub fn free(&mut self) {
        self.globals.free();
        self.strings.free();
        self.init_string = std::ptr::null_mut();
        free_objects();
    }

    #[inline(always)]
    fn get_frame(&mut self) -> *mut CallFrame {
        unsafe { self.frames.get_unchecked_mut(self.frame_count as usize - 1) }
    }

    pub fn run(&mut self) -> Result<(), InterpretResult> {
        // Store frame pointer in local variable to avoid repeated memory loads
        let mut frame = self.get_frame();
        // Keep IP in local variable to avoid memory traffic - this is critical for performance
        let mut ip = unsafe { (*frame).ip };

        loop {
            #[cfg(feature = "debug-trace-execution")]
            {
                print!("          ");
                let mut slot = self.stack.as_mut_ptr();
                while slot < self.stack_top {
                    let slot_val = unsafe { *slot };
                    print!("[ {slot_val} ]");
                    slot = slot.wrapping_add(1);
                }
                println!(); // newline
                let offset = unsafe {
                    let chunk_code_ptr = (*(*(*frame).closure).function).chunk.code;
                    ip.offset_from(chunk_code_ptr) as i32
                };
                unsafe {
                    (*(*(*frame).closure).function)
                        .chunk
                        .disassemble_instruction(offset);
                }
            }

            let instruction = OpCode::from(self.read_byte(&mut ip));
            let result = match instruction {
                OpCode::Print => self.op_print(),
                OpCode::Jump => self.op_jump(&mut ip),
                OpCode::JumpIfFalse => self.op_jump_if_false(&mut ip),
                OpCode::Loop => self.op_loop(&mut ip),
                OpCode::Call => self.op_call(&mut ip, &mut frame),
                OpCode::Invoke => self.op_invoke(&mut ip, &mut frame),
                OpCode::SuperInvoke => self.op_super_invoke(&mut ip, &mut frame),
                OpCode::Closure => self.op_closure(&mut ip, frame),
                OpCode::CloseUpvalue => self.op_close_upvalue(),
                OpCode::Return => self.op_return(&mut frame, &mut ip),
                OpCode::Constant => self.op_constant(&mut ip, frame),
                OpCode::Negate => self.op_negate(&ip, frame),
                OpCode::Add => self.op_add(&ip, frame),
                OpCode::Subtract => self.op_binary(&ip, frame, |a, b| Value::number_val(a - b)),
                OpCode::Multiply => self.op_binary(&ip, frame, |a, b| Value::number_val(a * b)),
                OpCode::Divide => self.op_binary(&ip, frame, |a, b| Value::number_val(a / b)),
                OpCode::Not => self.op_not(),
                OpCode::Nil => self.op_nil(),
                OpCode::True => self.op_true(),
                OpCode::False => self.op_false(),
                OpCode::Pop => self.op_pop(),
                OpCode::GetLocal => self.op_get_local(&mut ip, frame),
                OpCode::SetLocal => self.op_set_local(&mut ip, frame),
                OpCode::GetGlobal => self.op_get_global(&mut ip, frame),
                OpCode::DefineGlobal => self.op_define_global(&mut ip, frame),
                OpCode::SetGlobal => self.op_set_global(&mut ip, frame),
                OpCode::GetUpvalue => self.op_get_upvalue(&mut ip, frame),
                OpCode::SetUpvalue => self.op_set_upvalue(&mut ip, frame),
                OpCode::GetProperty => self.op_get_property(&mut ip, frame),
                OpCode::SetProperty => self.op_set_property(&mut ip, frame),
                OpCode::GetSuper => self.op_get_super(&mut ip, frame),
                OpCode::Equal => self.op_equal(),
                OpCode::Greater => self.op_binary(&ip, frame, |a, b| Value::bool_val(a > b)),
                OpCode::Less => self.op_binary(&ip, frame, |a, b| Value::bool_val(a < b)),
                OpCode::Class => self.op_class(&mut ip, frame),
                OpCode::Inherit => self.op_inherit(&ip, frame),
                OpCode::Method => self.op_method(&mut ip, frame),
            };

            match result {
                Ok(LoopControl::Continue) => continue,
                Ok(LoopControl::Stop) => return Ok(()),
                Err(e) => {
                    // Sync IP back to frame before exiting on error
                    unsafe { (*frame).ip = ip };
                    return Err(e);
                }
            }
        }
    }

    pub fn interpret(&mut self, source: &'static str) -> Result<(), InterpretResult> {
        let function = compile(source);
        if function.is_null() {
            return Err(InterpretResult::CompileError);
        }

        self.push(Value::obj_val(function));
        let closure = ObjClosure::new(function);
        self.pop();
        self.push(Value::obj_val(closure));
        self.call(closure, 0);

        self.run()
    }

    // --- Start of instruction helpers ---

    #[inline(always)]
    fn read_byte(&self, ip: &mut *mut u8) -> u8 {
        unsafe {
            let byte = **ip;
            *ip = ip.offset(1);
            byte
        }
    }

    #[inline(always)]
    fn read_short(&self, ip: &mut *mut u8) -> u16 {
        unsafe {
            let short = {
                let bytes = std::slice::from_raw_parts(*ip, 2);
                ((bytes[0] as u16) << 8) | (bytes[1] as u16)
            };
            *ip = ip.offset(2);
            short
        }
    }

    #[inline(always)]
    fn read_constant(&self, ip: &mut *mut u8, frame: *mut CallFrame) -> Value {
        let constant_index = self.read_byte(ip) as usize;
        unsafe {
            *(*(*(*frame).closure).function)
                .chunk
                .constants
                .values
                .wrapping_add(constant_index)
        }
    }

    #[inline(always)]
    fn read_string(&self, ip: &mut *mut u8, frame: *mut CallFrame) -> *mut ObjString {
        self.read_constant(ip, frame).as_string()
    }

    fn op_print(&mut self) -> Result<LoopControl, InterpretResult> {
        println!("{}", self.pop());
        Ok(LoopControl::Continue)
    }

    fn op_jump(&mut self, ip: &mut *mut u8) -> Result<LoopControl, InterpretResult> {
        let offset = self.read_short(ip);
        *ip = ip.wrapping_offset(offset as isize);
        Ok(LoopControl::Continue)
    }

    fn op_jump_if_false(&mut self, ip: &mut *mut u8) -> Result<LoopControl, InterpretResult> {
        let offset = self.read_short(ip);
        if self.peek(0).is_falsey() {
            *ip = ip.wrapping_offset(offset as isize);
        }
        Ok(LoopControl::Continue)
    }

    fn op_loop(&mut self, ip: &mut *mut u8) -> Result<LoopControl, InterpretResult> {
        let offset = self.read_short(ip);
        *ip = ip.wrapping_offset(-(offset as isize));
        Ok(LoopControl::Continue)
    }

    fn op_call(
        &mut self,
        ip: &mut *mut u8,
        frame: &mut *mut CallFrame,
    ) -> Result<LoopControl, InterpretResult> {
        let arg_count = self.read_byte(ip);
        let value = self.peek(arg_count as i32);
        unsafe { (**frame).ip = *ip };
        if !self.call_value(value, arg_count as i32) {
            return Err(InterpretResult::RuntimeError);
        }
        *frame = self.get_frame();
        *ip = unsafe { (**frame).ip };
        Ok(LoopControl::Continue)
    }

    fn op_invoke(
        &mut self,
        ip: &mut *mut u8,
        frame: *mut *mut CallFrame,
    ) -> Result<LoopControl, InterpretResult> {
        let method = self.read_string(ip, unsafe { *frame });
        let arg_count = self.read_byte(ip) as i32;
        unsafe { (**frame).ip = *ip };
        if !self.invoke(method, arg_count) {
            return Err(InterpretResult::RuntimeError);
        }
        unsafe { *frame = self.get_frame() };
        *ip = unsafe { (**frame).ip };
        Ok(LoopControl::Continue)
    }

    fn op_super_invoke(
        &mut self,
        ip: &mut *mut u8,
        frame: *mut *mut CallFrame,
    ) -> Result<LoopControl, InterpretResult> {
        let method = self.read_string(ip, unsafe { *frame });
        let arg_count = self.read_byte(ip) as i32;
        let superclass = self.pop().as_class();
        unsafe { (**frame).ip = *ip };
        if !self.invoke_from_class(superclass, method, arg_count) {
            return Err(InterpretResult::RuntimeError);
        }
        unsafe { *frame = self.get_frame() };
        *ip = unsafe { (**frame).ip };
        Ok(LoopControl::Continue)
    }

    fn op_closure(
        &mut self,
        ip: &mut *mut u8,
        frame: *mut CallFrame,
    ) -> Result<LoopControl, InterpretResult> {
        let function = self.read_constant(ip, frame).as_function();
        unsafe { (*frame).ip = *ip }; // Closure creation may trigger GC
        let closure = ObjClosure::new(function);
        self.push(Value::obj_val(closure));
        for i in 0..unsafe { (*closure).upvalue_count } {
            let is_local = self.read_byte(ip);
            let index = self.read_byte(ip);
            if is_local != 0 {
                unsafe {
                    let local = (*frame).slots.wrapping_offset(index as isize);
                    *(*closure).upvalues.wrapping_add(i as usize) = self.capture_upvalue(local);
                }
            } else {
                unsafe {
                    *(*closure).upvalues.wrapping_add(i as usize) =
                        *(*(*frame).closure).upvalues.wrapping_offset(index as isize);
                }
            }
        }
        Ok(LoopControl::Continue)
    }

    fn op_close_upvalue(&mut self) -> Result<LoopControl, InterpretResult> {
        self.close_upvalues(self.stack_top.wrapping_sub(1));
        self.pop();
        Ok(LoopControl::Continue)
    }

    fn op_return(
        &mut self,
        frame: &mut *mut CallFrame,
        ip: &mut *mut u8,
    ) -> Result<LoopControl, InterpretResult> {
        let result = self.pop();
        self.close_upvalues(unsafe { (**frame).slots });
        self.frame_count -= 1;
        if self.frame_count == 0 {
            self.pop();
            return Ok(LoopControl::Stop);
        }
        self.stack_top = unsafe { (**frame).slots };
        *frame = self.get_frame();
        *ip = unsafe { (**frame).ip };
        self.push(result);
        Ok(LoopControl::Continue)
    }

    fn op_constant(
        &mut self,
        ip: &mut *mut u8,
        frame: *mut CallFrame,
    ) -> Result<LoopControl, InterpretResult> {
        let constant = self.read_constant(ip, frame);
        self.push(constant);
        Ok(LoopControl::Continue)
    }

    fn op_negate(
        &mut self,
        ip: &*mut u8,
        frame: *mut CallFrame,
    ) -> Result<LoopControl, InterpretResult> {
        if !self.peek(0).is_number() {
            unsafe { (*frame).ip = *ip };
            self.runtime_error("Operand must be a number.");
            return Err(InterpretResult::RuntimeError);
        }
        let v = Value::number_val(-self.pop().as_number());
        self.push(v);
        Ok(LoopControl::Continue)
    }

    fn op_nil(&mut self) -> Result<LoopControl, InterpretResult> {
        self.push(Value::nil_val());
        Ok(LoopControl::Continue)
    }

    fn op_true(&mut self) -> Result<LoopControl, InterpretResult> {
        self.push(Value::bool_val(true));
        Ok(LoopControl::Continue)
    }

    fn op_false(&mut self) -> Result<LoopControl, InterpretResult> {
        self.push(Value::bool_val(false));
        Ok(LoopControl::Continue)
    }

    fn op_pop(&mut self) -> Result<LoopControl, InterpretResult> {
        self.pop();
        Ok(LoopControl::Continue)
    }

    fn op_get_local(
        &mut self,
        ip: &mut *mut u8,
        frame: *mut CallFrame,
    ) -> Result<LoopControl, InterpretResult> {
        let slot = self.read_byte(ip);
        let val = unsafe { *(*frame).slots.wrapping_offset(slot as isize) };
        self.push(val);
        Ok(LoopControl::Continue)
    }

    fn op_set_local(
        &mut self,
        ip: &mut *mut u8,
        frame: *mut CallFrame,
    ) -> Result<LoopControl, InterpretResult> {
        let slot = self.read_byte(ip);
        unsafe {
            *(*frame).slots.wrapping_offset(slot as isize) = self.peek(0);
        }
        Ok(LoopControl::Continue)
    }

    fn op_get_global(
        &mut self,
        ip: &mut *mut u8,
        frame: *mut CallFrame,
    ) -> Result<LoopControl, InterpretResult> {
        let name = self.read_string(ip, frame);
        if let Some(value) = self.globals.get(name) {
            self.push(value);
        } else {
            unsafe {
                (*frame).ip = *ip;
                let name_deref = &*name;
                self.runtime_error(&format!("Undefined variable '{name_deref}'."));
                return Err(InterpretResult::RuntimeError);
            }
        }
        Ok(LoopControl::Continue)
    }

    fn op_define_global(
        &mut self,
        ip: &mut *mut u8,
        frame: *mut CallFrame,
    ) -> Result<LoopControl, InterpretResult> {
        let name = self.read_string(ip, frame);
        self.globals.set(name, self.peek(0));
        self.pop();
        Ok(LoopControl::Continue)
    }

    fn op_set_global(
        &mut self,
        ip: &mut *mut u8,
        frame: *mut CallFrame,
    ) -> Result<LoopControl, InterpretResult> {
        let name = self.read_string(ip, frame);
        if self.globals.set(name, self.peek(0)) {
            self.globals.delete(name);
            unsafe {
                (*frame).ip = *ip;
                let name_deref = &*name;
                self.runtime_error(&format!("Undefined variable '{name_deref}'."));
                return Err(InterpretResult::RuntimeError);
            }
        }
        Ok(LoopControl::Continue)
    }

    fn op_get_upvalue(
        &mut self,
        ip: &mut *mut u8,
        frame: *mut CallFrame,
    ) -> Result<LoopControl, InterpretResult> {
        let slot = self.read_byte(ip);
        let v =
            unsafe { *(*(*(*(*frame).closure).upvalues.wrapping_offset(slot as isize))).location };
        self.push(v);
        Ok(LoopControl::Continue)
    }

    fn op_set_upvalue(
        &mut self,
        ip: &mut *mut u8,
        frame: *mut CallFrame,
    ) -> Result<LoopControl, InterpretResult> {
        let slot = self.read_byte(ip);
        unsafe {
            *(*(*(*(*frame).closure).upvalues.wrapping_offset(slot as isize))).location =
                self.peek(0);
        }
        Ok(LoopControl::Continue)
    }

    fn op_set_property(
        &mut self,
        ip: &mut *mut u8,
        frame: *mut CallFrame,
    ) -> Result<LoopControl, InterpretResult> {
        if !self.peek(1).is_instance() {
            unsafe { (*frame).ip = *ip };
            self.runtime_error("Only instances have fields.");
            return Err(InterpretResult::RuntimeError);
        }

        let instance = self.peek(1).as_instance();
        unsafe {
            (*instance)
                .fields
                .set(self.read_string(ip, frame), self.peek(0));
            let value = self.pop();
            self.pop();
            self.push(value);
        }
        Ok(LoopControl::Continue)
    }

    fn op_get_property(
        &mut self,
        ip: &mut *mut u8,
        frame: *mut CallFrame,
    ) -> Result<LoopControl, InterpretResult> {
        if !self.peek(0).is_instance() {
            unsafe { (*frame).ip = *ip };
            self.runtime_error("Only instances have properties.");
            return Err(InterpretResult::RuntimeError);
        }
        let instance = self.peek(0).as_instance();
        let name = self.read_string(ip, frame);
        if let Some(value) = unsafe { (*instance).fields.get(name) } {
            self.pop(); // instance
            self.push(value);
        } else {
            unsafe { (*frame).ip = *ip };
            if !self.bind_method(unsafe { (*instance).class }, name) {
                return Err(InterpretResult::RuntimeError);
            }
        }
        Ok(LoopControl::Continue)
    }

    fn op_get_super(
        &mut self,
        ip: &mut *mut u8,
        frame: *mut CallFrame,
    ) -> Result<LoopControl, InterpretResult> {
        let name = self.read_string(ip, frame);
        let superclass = self.pop().as_class();
        unsafe { (*frame).ip = *ip };
        if !self.bind_method(superclass, name) {
            return Err(InterpretResult::RuntimeError);
        }
        Ok(LoopControl::Continue)
    }

    fn op_equal(&mut self) -> Result<LoopControl, InterpretResult> {
        let b = self.pop();
        let a = self.pop();
        self.push(Value::bool_val(a == b));
        Ok(LoopControl::Continue)
    }

    fn op_add(
        &mut self,
        ip: &*mut u8,
        frame: *mut CallFrame,
    ) -> Result<LoopControl, InterpretResult> {
        if self.peek(0).is_string() && self.peek(1).is_string() {
            unsafe { (*frame).ip = *ip }; // Concatenation may trigger GC
            self.concatenate();
        } else if self.peek(0).is_number() && self.peek(1).is_number() {
            let b = self.pop().as_number();
            let a = self.pop().as_number();
            self.push(Value::number_val(a + b));
        } else {
            unsafe { (*frame).ip = *ip };
            self.runtime_error("Operands must be two numbers or two strings.");
            return Err(InterpretResult::RuntimeError);
        }
        Ok(LoopControl::Continue)
    }

    #[inline(always)]
    fn op_binary<F>(
        &mut self,
        ip: &*mut u8,
        frame: *mut CallFrame,
        op: F,
    ) -> Result<LoopControl, InterpretResult>
    where
        F: FnOnce(f64, f64) -> Value,
    {
        if !self.peek(0).is_number() || !self.peek(1).is_number() {
            unsafe { (*frame).ip = *ip };
            self.runtime_error("Operands must be numbers.");
            return Err(InterpretResult::RuntimeError);
        }
        let b = self.pop().as_number();
        let a = self.pop().as_number();
        self.push(op(a, b));
        Ok(LoopControl::Continue)
    }

    fn op_not(&mut self) -> Result<LoopControl, InterpretResult> {
        let v = Value::bool_val(self.pop().is_falsey());
        self.push(v);
        Ok(LoopControl::Continue)
    }

    fn op_class(
        &mut self,
        ip: &mut *mut u8,
        frame: *mut CallFrame,
    ) -> Result<LoopControl, InterpretResult> {
        unsafe { (*frame).ip = *ip }; // Class creation may trigger GC
        let class_name = self.read_string(ip, frame);
        let class = ObjClass2::new(class_name);
        self.push(Value::obj_val(class));
        Ok(LoopControl::Continue)
    }

    fn op_inherit(
        &mut self,
        ip: &*mut u8,
        frame: *mut CallFrame,
    ) -> Result<LoopControl, InterpretResult> {
        let superclass = self.peek(1);
        if !superclass.is_class() {
            unsafe { (*frame).ip = *ip };
            self.runtime_error("Superclass must be a class.");
            return Err(InterpretResult::RuntimeError);
        }
        let subclass = self.peek(0).as_class();
        unsafe {
            Table::add_all(
                &mut (*superclass.as_class()).methods,
                &mut (*subclass).methods,
            );
        }
        self.pop(); // Subclass
        Ok(LoopControl::Continue)
    }

    fn op_method(
        &mut self,
        ip: &mut *mut u8,
        frame: *mut CallFrame,
    ) -> Result<LoopControl, InterpretResult> {
        let name = self.read_string(ip, frame);
        self.define_method(name);
        Ok(LoopControl::Continue)
    }

    // --- End of instruction helpers ---

    fn concatenate(&mut self) {
        let b = self.peek(0).as_string();
        let a = self.peek(1).as_string();

        let length = unsafe { (*a).length + (*b).length };
        let chars = ALLOCATE!(u8, length as usize);
        unsafe {
            std::ptr::copy_nonoverlapping((*a).chars, chars, (*a).length as usize);
            std::ptr::copy_nonoverlapping(
                (*b).chars,
                chars.add((*a).length as usize),
                (*b).length as usize,
            );
        }

        let result = ObjString::take_string(chars, length);
        self.pop();
        self.pop();
        self.push(Value::obj_val(result));
    }

    fn runtime_error(&mut self, message: &str) {
        eprintln!("{message}");

        for i in (0..self.frame_count).rev() {
            let frame = unsafe { self.frames.get_unchecked(i as usize) };
            let function = unsafe { (*frame.closure).function };
            let instruction = unsafe { frame.ip as usize - (*function).chunk.code as usize - 1 };
            let line = unsafe { *((*function).chunk.lines.wrapping_add(instruction)) };
            let name = unsafe {
                if (*function).name.is_null() {
                    "script".to_owned()
                } else {
                    format!("{}()", (*(*function).name))
                }
            };
            eprintln!("[line {line}] in {name}");
        }

        self.reset_stack();
    }

    fn define_native(&mut self, name: &str, function: NativeFn) {
        self.push(Value::obj_val(Obj::copy_string(
            name.as_bytes(),
            name.len() as i32,
        )));
        self.push(Value::obj_val(ObjNative::new(function)));
        self.globals.set(self.stack[0].as_string(), self.stack[1]);
        self.pop();
        self.pop();
    }

    pub fn push(&mut self, value: Value) {
        unsafe { *self.stack_top = value };
        self.stack_top = self.stack_top.wrapping_add(1);
    }

    pub fn pop(&mut self) -> Value {
        self.stack_top = self.stack_top.wrapping_sub(1);
        unsafe { *self.stack_top }
    }

    fn peek(&self, distance: i32) -> Value {
        unsafe { *self.stack_top.offset(-1 - distance as isize) }
    }

    fn call(&mut self, closure: *mut ObjClosure, arg_count: i32) -> bool {
        let arity = unsafe { (*(*closure).function).arity };
        if arg_count != arity {
            self.runtime_error(&format!("Expected {arity} arguments but got {arg_count}."));
            return false;
        }

        if self.frame_count == FRAMES_MAX as i32 {
            self.runtime_error("Stack overflow.");
            return false;
        }

        let frame = unsafe { self.frames.get_unchecked_mut(self.frame_count as usize) };
        self.frame_count += 1;
        frame.closure = closure;
        frame.ip = unsafe { (*(*closure).function).chunk.code };
        frame.slots = unsafe { self.stack_top.offset(-(arg_count as isize) - 1) };
        true
    }

    fn call_value(&mut self, callee: Value, arg_count: i32) -> bool {
        if callee.is_obj() {
            match callee.obj_type() {
                ObjType::Closure => return self.call(callee.as_closure(), arg_count),
                // NOTE: Bare ObjFunction calls are not implemented. ObjFunctions are always wrapped in ObjClosures.
                ObjType::Function => unsafe { unreachable_unchecked() },
                ObjType::Native => {
                    let native = callee.as_native();
                    let args = unsafe {
                        std::slice::from_raw_parts_mut(self.stack_top, arg_count as usize)
                    };
                    let result = native(arg_count, args);
                    self.stack_top = self.stack_top.wrapping_sub(arg_count as usize + 1);
                    self.push(result);
                    return true;
                }
                ObjType::Class => {
                    let class = callee.as_class();
                    unsafe {
                        *self.stack_top.wrapping_sub(arg_count as usize + 1) =
                            Value::obj_val(ObjInstance2::new(class));
                        if let Some(initializer) = (*class).methods.get(self.init_string) {
                            return self.call(initializer.as_closure(), arg_count);
                        } else if arg_count != 0 {
                            self.runtime_error(&format!(
                                "Expected 0 arguments but got {arg_count}."
                            ));
                            return false;
                        }
                    }
                    return true;
                }
                ObjType::BoundMethod => {
                    let bound = callee.as_bound_method();
                    //-argCount skips past args and - 1 adjusts for stackTop pointing just past last used stack slot.
                    unsafe {
                        *self.stack_top.offset(-(arg_count as isize) - 1) = (*bound).reciever;
                    }
                    return self.call(unsafe { (*bound).method }, arg_count);
                }
                ObjType::String | ObjType::Upvalue | ObjType::Instance => { /* Non-Callable Object Type */
                }
            }
        }

        self.runtime_error("Can only call functions and classes.");
        false
    }

    fn invoke_from_class(
        &mut self,
        class: *mut ObjClass2,
        name: *mut ObjString,
        arg_count: i32,
    ) -> bool {
        unsafe {
            if let Some(method) = (*class).methods.get(name) {
                self.call(method.as_closure(), arg_count)
            } else {
                self.runtime_error(&format!("Undefined property '{}'.", *name));
                false
            }
        }
    }

    fn invoke(&mut self, name: *mut ObjString, arg_count: i32) -> bool {
        let receiver = self.peek(arg_count);
        if !receiver.is_instance() {
            self.runtime_error("Only instances have methods.");
            return false;
        }
        let instance = receiver.as_instance();
        unsafe {
            if let Some(value) = (*instance).fields.get(name) {
                *self.stack_top.wrapping_sub(arg_count as usize + 1) = value;
                self.call_value(value, arg_count)
            } else {
                self.invoke_from_class((*instance).class, name, arg_count)
            }
        }
    }

    fn bind_method(&mut self, class: *mut ObjClass2, name: *mut ObjString) -> bool {
        if let Some(method) = unsafe { (*class).methods.get(name) } {
            let bound = ObjBoundMethod2::new(self.peek(0), method.as_closure());
            self.pop();
            self.push(Value::obj_val(bound));
            true
        } else {
            unsafe {
                self.runtime_error(&format!("Undefined property '{}'.", *name));
            }
            false
        }
    }

    fn capture_upvalue(&mut self, local: *mut Value) -> *mut ObjUpvalue {
        let mut prev_upvalue = std::ptr::null_mut();
        let mut upvalue = self.open_upvalues;
        while !upvalue.is_null() && unsafe { (*upvalue).location > local } {
            prev_upvalue = upvalue;
            upvalue = unsafe { (*upvalue).next };
        }

        if !upvalue.is_null() && unsafe { (*upvalue).location == local } {
            return upvalue;
        }

        let created_upvalue = ObjUpvalue::new(local);
        unsafe { (*created_upvalue).next = upvalue };

        if prev_upvalue.is_null() {
            self.open_upvalues = created_upvalue;
        } else {
            unsafe { (*prev_upvalue).next = created_upvalue };
        }

        created_upvalue
    }

    fn close_upvalues(&mut self, last: *mut Value) {
        while !self.open_upvalues.is_null() && unsafe { (*self.open_upvalues).location >= last } {
            let upvalue = self.open_upvalues;
            unsafe {
                (*upvalue).closed = *(*upvalue).location;
                (*upvalue).location = &mut (*upvalue).closed;
                self.open_upvalues = (*upvalue).next;
            }
        }
    }

    fn define_method(&mut self, name: *mut ObjString) {
        let method = self.peek(0);
        let class = self.peek(1).as_class();
        unsafe {
            (*class).methods.set(name, method);
        }
        self.pop();
    }
}
