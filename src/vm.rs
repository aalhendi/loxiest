use std::{
    cell::RefCell,
    collections::{hash_map::Entry, HashMap},
    hint::unreachable_unchecked,
    mem::MaybeUninit,
    ops::Deref,
    rc::Rc,
};

use crate::{
    chunk::{Chunk, OpCode},
    compiler::{compile, Parser},
    memory::{free_objects, reallocate},
    object::{
        native_clock2, NativeFn2, Obj, ObjBoundMethod2, ObjClass2, ObjClosure2, ObjFunction,
        ObjInstance2, ObjNative2, ObjString, ObjType, ObjUpvalue2,
    },
    table::Table,
    value::Value,
    ALLOCATE,
};

macro_rules! binary_op {
    ($vm:expr, $value_type:expr, $op:tt) => {{
        if !$crate::value::Value::is_number(&$vm.peek(0)) || !$crate::value::Value::is_number(&$vm.peek(1)) {
            $vm.runtime_error("Operands must be numbers.");
            return Err($crate::vm::InterpretResult::RuntimeError);
        }
        let b = $crate::value::Value::as_number(&$vm.pop());
        let a = $crate::value::Value::as_number(&$vm.pop());
        let result = $value_type(a $op b);
        $vm.push(result);
    }};
}

const FRAMES_MAX: usize = 64;
const STACK_MAX: usize = FRAMES_MAX * (u8::MAX as usize + 1);

pub enum InterpretResult {
    CompileError,
    RuntimeError,
}

pub struct CallFrame {
    pub closure: *mut ObjClosure2,
    pub ip: *mut u8,
    pub slots: *mut Value,
}

pub struct VM {
    frames: [CallFrame; FRAMES_MAX],
    frame_count: usize,

    pub stack: [Value; STACK_MAX],
    pub stack_top: *mut Value,
    pub globals: Table,
    pub strings: Table,
    pub init_string: *mut ObjString,
    pub open_upvalues: *mut ObjUpvalue2, // Intrusive linked list

    pub bytes_allocated: usize,
    pub next_gc: usize,
    pub objects: *mut Obj, // Intrusive linked list head
    pub gray_count: usize,
    pub gray_capacity: usize,
    // assume full responsibility for this array, including allocation failure.
    // If we can’t create or grow the gray stack, then we can’t finish the garbage collection.
    // this is bad news for the VM, but fortunately rare since the gray stack tends to be pretty small
    // TODO(aalhendi): do something more graceful
    pub gray_stack: *mut *mut Obj,
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
        &mut self.frames[self.frame_count - 1]
    }

    pub fn run(&mut self) -> Result<(), InterpretResult> {
        let mut frame = self.get_frame();

        macro_rules! READ_BYTE {
            ($self:expr) => {{
                #[allow(unused_unsafe)]
                unsafe {
                    let byte = *(*frame).ip;
                    (*frame).ip = (*frame).ip.offset(1);
                    byte
                }
            }};
        }

        macro_rules! READ_SHORT {
            ($self:expr) => {{
                unsafe {
                    let short = {
                        let bytes = std::slice::from_raw_parts((*frame).ip, 2);
                        ((bytes[0] as u16) << 8) | (bytes[1] as u16)
                    };
                    (*frame).ip = (*frame).ip.offset(2);
                    short
                }
            }};
        }

        macro_rules! READ_CONSTANT {
            ($self:expr) => {{
                let constant_index = READ_BYTE!($self) as usize;
                #[allow(unused_unsafe)]
                unsafe {
                    *(*(*(*frame).closure).function)
                        .chunk
                        .constants
                        .values
                        .wrapping_add(constant_index)
                }
            }};
        }

        macro_rules! READ_STRING {
            ($self:expr) => {{
                READ_CONSTANT!($self).as_string()
            }};
        }

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
                    let ip_ptr = (*frame).ip;
                    ip_ptr.offset_from(chunk_code_ptr) as usize
                };
                unsafe {
                    (*(*(*frame).closure).function)
                        .chunk
                        .disassemble_instruction(offset);
                }
            }

            let instruction = OpCode::from(READ_BYTE!(self));
            match instruction {
                OpCode::Print => {
                    println!("{}", self.pop());
                }
                OpCode::Jump => {
                    let offset = READ_SHORT!(self);
                    unsafe {
                        (*frame).ip = (*frame).ip.wrapping_add(offset as usize);
                    }
                }
                OpCode::JumpIfFalse => {
                    let offset = READ_SHORT!(self);
                    if self.peek(0).is_falsey() {
                        unsafe {
                            (*frame).ip = (*frame).ip.wrapping_add(offset as usize);
                        }
                    }
                }
                OpCode::Loop => {
                    let offset = READ_SHORT!(self);
                    unsafe {
                        (*frame).ip = (*frame).ip.wrapping_sub(offset as usize);
                    }
                }
                OpCode::Call => {
                    let arg_count = READ_BYTE!(self);
                    let value = self.peek(arg_count as isize);
                    if !self.call_value(value, arg_count) {
                        return Err(InterpretResult::RuntimeError);
                    }
                    frame = self.get_frame();
                }
                OpCode::Invoke => {
                    let method = READ_STRING!(self);
                    let arg_count = READ_BYTE!(self);
                    if !self.invoke(method, arg_count) {
                        return Err(InterpretResult::RuntimeError);
                    }
                    frame = self.get_frame();
                }
                OpCode::SuperInvoke => {
                    let method = READ_STRING!(self);
                    let arg_count = READ_BYTE!(self);
                    let superclass = self.pop().as_class();
                    if !self.invoke_from_class(superclass, method, arg_count) {
                        return Err(InterpretResult::RuntimeError);
                    }
                    frame = self.get_frame();
                }
                OpCode::Closure => {
                    let function = READ_CONSTANT!(self).as_function();
                    let closure = ObjClosure2::new(function);
                    self.push(Value::obj_val(closure));
                    for i in 0..unsafe { (*closure).upvalue_count } {
                        let is_local = READ_BYTE!(self);
                        let index = READ_BYTE!(self);
                        if is_local != 0 {
                            unsafe {
                                let local = (*frame).slots.wrapping_add(index as usize);
                                *(*closure).upvalues.wrapping_add(i as usize) =
                                    self.capture_upvalue(local);
                            }
                        } else {
                            unsafe {
                                *(*closure).upvalues.wrapping_add(i as usize) =
                                    *(*(*frame).closure).upvalues.wrapping_add(index as usize);
                            }
                        }
                    }
                }
                OpCode::CloseUpvalue => {
                    self.close_upvalues(self.stack_top.wrapping_sub(1));
                    self.pop();
                }
                OpCode::Return => {
                    let result = self.pop();
                    self.close_upvalues(unsafe { (*frame).slots });
                    self.frame_count -= 1;
                    if self.frame_count == 0 {
                        self.pop();
                        return Ok(());
                    }

                    self.stack_top = unsafe { (*frame).slots };
                    frame = self.get_frame();
                    self.push(result);
                }
                OpCode::Constant => {
                    let constant = READ_CONSTANT!(self);
                    self.push(constant);
                }
                OpCode::Negate => {
                    if !self.peek(0).is_number() {
                        self.runtime_error("Operand must be a number.");
                        return Err(InterpretResult::RuntimeError);
                    }
                    let v = Value::number_val(-self.pop().as_number());
                    self.push(v);
                }
                OpCode::Nil => self.push(Value::nil_val()),
                OpCode::True => self.push(Value::bool_val(true)),
                OpCode::False => self.push(Value::bool_val(false)),
                OpCode::Pop => {
                    self.pop();
                }
                OpCode::GetLocal => {
                    let slot = READ_BYTE!(self);
                    let val = unsafe { *(*frame).slots.wrapping_add(slot as usize) };
                    self.push(val);
                }
                OpCode::SetLocal => {
                    let slot = READ_BYTE!(self);
                    unsafe {
                        *(*frame).slots.wrapping_add(slot as usize) = self.peek(0);
                    }
                }
                OpCode::GetGlobal => {
                    let name = READ_STRING!(self);
                    if let Some(value) = self.globals.get(name) {
                        self.push(value);
                    } else {
                        unsafe {
                            let name_deref = &*name;
                            self.runtime_error(&format!("Undefined variable '{name_deref}'."));
                            return Err(InterpretResult::RuntimeError);
                        }
                    }
                }
                OpCode::DefineGlobal => {
                    let name = READ_STRING!(self);
                    self.globals.set(name, self.peek(0));
                    self.pop();
                }
                OpCode::SetGlobal => {
                    let name = READ_STRING!(self);

                    if self.globals.set(name, self.peek(0)) {
                        self.globals.delete(name);
                        unsafe {
                            let name_deref = &*name;
                            self.runtime_error(&format!("Undefined variable '{name_deref}'."));
                            return Err(InterpretResult::RuntimeError);
                        }
                    }
                }
                OpCode::GetUpvalue => {
                    let slot = READ_BYTE!(self) as usize;
                    let v = unsafe {
                        *(*(*(*(*frame).closure).upvalues.wrapping_add(slot))).location
                    };
                    self.push(v);
                }
                OpCode::SetUpvalue => {
                    let slot = READ_BYTE!(self) as usize;
                    unsafe {
                        *(*(*(*(*frame).closure).upvalues.wrapping_add(slot))).location =
                            self.peek(0);
                    }
                }
                OpCode::SetProperty => {
                    if !self.peek(1).is_instance() {
                        self.runtime_error("Only instances have fields.");
                        return Err(InterpretResult::RuntimeError);
                    }

                    let instance = self.peek(1).as_instance();
                    unsafe {
                        (*instance).fields.set(READ_STRING!(self), self.peek(0));
                        let value = self.pop();
                        self.pop();
                        self.push(value);
                    }
                }
                OpCode::GetProperty => {
                    if !self.peek(0).is_instance() {
                        self.runtime_error("Only instances have properties.");
                        return Err(InterpretResult::RuntimeError);
                    }
                    let instance = self.peek(0).as_instance();
                    let name = READ_STRING!(self);
                    if let Some(value) = unsafe { (*instance).fields.get(name) } {
                        self.pop(); // instance
                        self.push(value);
                    } else if !self.bind_method(unsafe { (*instance).class }, name) {
                        return Err(InterpretResult::RuntimeError);
                    }
                }
                OpCode::GetSuper => {
                    let name = READ_STRING!(self);
                    let superclass = self.pop().as_class();

                    if !self.bind_method(superclass, name) {
                        return Err(InterpretResult::RuntimeError);
                    }
                }
                OpCode::Equal => {
                    let b = self.pop();
                    let a = self.pop();
                    self.push(Value::bool_val(a == b));
                }
                OpCode::Greater => binary_op!(self, Value::bool_val, >),
                OpCode::Less => binary_op!(self, Value::bool_val, <),
                OpCode::Add => {
                    if self.peek(0).is_string() && self.peek(1).is_string() {
                        self.concatenate();
                    } else if self.peek(0).is_number() && self.peek(1).is_number() {
                        let b = self.pop().as_number();
                        let a = self.pop().as_number();
                        let v = Value::number_val(a + b);
                        self.push(v);
                    } else {
                        self.runtime_error("Operands must be two numbers or two strings.");
                        return Err(InterpretResult::RuntimeError);
                    }
                }
                OpCode::Subtract => binary_op!(self, Value::number_val, -),
                OpCode::Multiply => binary_op!(self, Value::number_val, *),
                OpCode::Divide => binary_op!(self, Value::number_val, /),
                OpCode::Not => {
                    let v = Value::bool_val(self.pop().is_falsey());
                    self.push(v)
                }
                OpCode::Class => {
                    let v = Value::obj_val(ObjClass2::new(READ_STRING!(self)));
                    self.push(v);
                }
                OpCode::Inherit => {
                    let superclass = self.peek(1);
                    if !superclass.is_class() {
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
                }
                OpCode::Method => {
                    let name = READ_STRING!(self);
                    self.define_method(name);
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
        let closure = ObjClosure2::new(function);
        self.pop();
        self.push(Value::obj_val(closure));
        self.call(closure, 0);

        self.run()
    }

    fn concatenate(&mut self) {
        let b = self.peek(0).as_string();
        let a = self.peek(1).as_string();

        let length = unsafe { (*a).length + (*b).length };
        let chars = ALLOCATE!(u8, length as usize);
        // PERF(aalhendi): ghetto memcpy, not sure about perf
        unsafe {
            for idx in 0..(*a).length {
                (*chars.offset(idx)) = *(*a).chars.offset(idx);
            }

            for idx in 0..(*b).length {
                (*chars.offset((*a).length + idx)) = *(*b).chars.offset(idx);
            }
        }

        let result = ObjString::take_string(chars, length as usize);
        self.pop();
        self.pop();
        self.push(Value::obj_val(result));
    }

    fn runtime_error(&mut self, message: &str) {
        eprintln!("{message}");

        for i in (0..=self.frame_count - 1).rev() {
            let frame = &self.frames[i];
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
            eprintln!("[line {line}] in {name}")
        }

        self.reset_stack();
    }

    fn define_native(&mut self, name: &str, function: NativeFn2) {
        self.push(Value::obj_val(Obj::copy_string(
            name.as_bytes(),
            name.as_bytes().len(),
        )));
        self.push(Value::obj_val(ObjNative2::new(function)));
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

    fn peek(&self, distance: isize) -> Value {
        unsafe { *self.stack_top.offset(-1 - distance) }
    }

    fn call(&mut self, closure: *mut ObjClosure2, arg_count: u8) -> bool {
        let arity = unsafe { (*(*closure).function).arity } as u8;
        if arg_count != arity {
            self.runtime_error(&format!("Expected {arity} arguments but got {arg_count}."));
            return false;
        }

        if self.frame_count == FRAMES_MAX {
            self.runtime_error("Stack overflow.");
            return false;
        }

        let frame = &mut self.frames[self.frame_count];
        self.frame_count += 1;
        frame.closure = closure;
        frame.ip = unsafe { (*(*closure).function).chunk.code };
        frame.slots = unsafe { self.stack_top.offset(-(arg_count as isize) - 1) };
        true
    }

    fn call_value(&mut self, callee: Value, arg_count: u8) -> bool {
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
                    let result = native(arg_count as usize, args);
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
        arg_count: u8,
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

    fn invoke(&mut self, name: *mut ObjString, arg_count: u8) -> bool {
        let receiver = self.peek(arg_count as isize);
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

    fn capture_upvalue(&mut self, local: *mut Value) -> *mut ObjUpvalue2 {
        let mut prev_upvalue = std::ptr::null_mut();
        let mut upvalue = self.open_upvalues;
        while !upvalue.is_null() && unsafe { (*upvalue).location > local } {
            prev_upvalue = upvalue;
            upvalue = unsafe { (*upvalue).next };
        }

        if !upvalue.is_null() && unsafe { (*upvalue).location == local } {
            return upvalue;
        }

        let created_upvalue = ObjUpvalue2::new(local);
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
