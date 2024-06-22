use std::{
    cell::RefCell,
    collections::{hash_map::Entry, HashMap},
    mem::MaybeUninit,
    ops::Deref,
    rc::Rc,
};

use crate::{
    chunk::{Chunk2, OpCode},
    compiler::{Compiler, FunctionType},
    compiler2::{compile, Parser},
    memory::{free_objects, reallocate},
    object::{
        native_clock, NativeFn, Obj, ObjBoundMethod, ObjClass, ObjClosure, ObjInstance, ObjNative,
        ObjUpvalue,
    },
    object2::{
        native_clock2, NativeFn2, Obj2, ObjClass2, ObjClosure2, ObjFunction, ObjInstance2,
        ObjNative2, ObjString, ObjType, ObjUpvalue2,
    },
    table::Table,
    value::{Value, Value2},
    ALLOCATE,
};

macro_rules! frame_mut {
    ($self:ident) => {
        $self.frames.last_mut().unwrap()
    };
}

macro_rules! binary_op {
    ($vm:expr, $value_type:expr, $op:tt) => {{
        if !$crate::value::Value2::is_number(&$vm.peek(0)) || !$crate::value::Value2::is_number(&$vm.peek(1)) {
            $vm.runtime_error("Operands must be numbers.");
            return Err($crate::vm::InterpretResult::RuntimeError);
        }
        let b = $crate::value::Value2::as_number(&$vm.pop());
        let a = $crate::value::Value2::as_number(&$vm.pop());
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
    pub closure: Rc<ObjClosure>, // Ptr to fuction being called. Used to look up constants and other stuff.
    pub ip: usize, // Caller stores its own IP index (as opposed to storing its own ptr in C)
    pub slots: usize, // index into VM's stack. Points to first slot the function can use.
}

impl CallFrame {
    pub fn new(closure: Rc<ObjClosure>, ip: usize, slots: usize) -> Self {
        Self { closure, ip, slots }
    }
}

pub struct VM {
    stack: Vec<Value>, // No need to impl a stack data structure... Vec does it all
    globals: HashMap<Rc<str>, Value>,
    frames: Vec<CallFrame>,
    open_upvalues: Vec<Rc<RefCell<ObjUpvalue>>>,
}

impl VM {
    pub fn new() -> Self {
        let mut vm = Self {
            stack: Vec::with_capacity(STACK_MAX),
            globals: HashMap::new(),
            frames: Vec::with_capacity(FRAMES_MAX), // TODO(aalhendi): fixed size array?
            open_upvalues: Vec::new(),
        };

        vm.define_native("clock".into(), native_clock);

        vm
    }

    fn define_native(&mut self, name: Rc<str>, function: NativeFn) {
        // C version pushes 2 values to the stack. String: name and Obj: func
        // it sets these values in the globals table then pops the 2 values off
        // the stack. That is done for GC purposes to potentially trigger a collection.
        // Since we are going with an RC approach this can all be summerized in a globals.insert
        self.globals.insert(
            name,
            Value::Obj(Obj::Native(ObjNative::new(function)).into()),
        );
    }

    pub fn free(&mut self) {}

    pub fn interpret(&mut self, source: String) -> Result<(), InterpretResult> {
        let mut compiler = Compiler::new(source, FunctionType::Script);
        if let Some(function) = compiler.compile() {
            let closure = Rc::new(ObjClosure::new(function));
            self.stack
                .push(Value::Obj(Obj::Closure(closure.clone()).into()));
            match self.call(&closure, 0) {
                Ok(_) => {
                    let result = self.run();
                    self.stack.pop();
                    result
                }
                Err(e) => Err(e),
            }
        } else {
            // NOTE(aalhendi): is this rly needed?
            compiler.current_chunk().free();
            Err(InterpretResult::CompileError)
        }
    }

    fn run(&mut self) -> Result<(), InterpretResult> {
        loop {
            #[cfg(feature = "debug-trace-execution")]
            {
                print!("          ");
                for slot in &self.stack {
                    print!("[ {slot} ]");
                }
                println!(); // newline
                let frame = frame_mut!(self);
                let chunk = &frame.closure.function.chunk;
                chunk.disassemble_instruction(frame.ip);
            }

            let instruction = OpCode::from(self.read_byte());
            match instruction {
                OpCode::Constant => self.op_constant(),
                OpCode::Negate => self.op_negate()?,
                OpCode::Add => self.add_values()?,
                OpCode::Subtract => self.op_binary(|a, b| Value::Number(a - b))?,
                OpCode::Multiply => self.op_binary(|a, b| Value::Number(a * b))?,
                OpCode::Divide => self.op_binary(|a, b| Value::Number(a / b))?,
                OpCode::Greater => self.op_binary(|a, b| Value::Boolean(a > b))?,
                OpCode::Less => self.op_binary(|a, b| Value::Boolean(a < b))?,
                OpCode::False => self.stack.push(Value::Boolean(false)),
                OpCode::True => self.stack.push(Value::Boolean(true)),
                OpCode::Nil => self.stack.push(Value::Nil),
                OpCode::Not => self.op_not(),
                OpCode::Equal => self.op_equal(),
                OpCode::Print => println!("{v}", v = self.stack.pop().unwrap()),
                OpCode::Pop => self.op_pop(),
                OpCode::DefineGlobal => self.op_define_global(),
                OpCode::GetGlobal => self.op_get_global()?,
                OpCode::SetGlobal => self.op_set_global()?,
                OpCode::GetLocal => self.op_get_local(),
                OpCode::SetLocal => self.op_set_local(),
                OpCode::JumpIfFalse => self.op_jump_if_false(),
                OpCode::Jump => self.op_jump(),
                OpCode::Loop => self.op_loop(),
                OpCode::Call => self.op_call()?,
                OpCode::Closure => self.op_closure(),
                OpCode::GetUpvalue => self.op_get_upvalue(),
                OpCode::SetUpvalue => self.op_set_upvalue(),
                OpCode::CloseUpvalue => self.op_close_upvalue(),
                OpCode::Class => self.op_class(),
                OpCode::GetProperty => self.op_get_property()?,
                OpCode::SetProperty => self.op_set_property()?,
                OpCode::Method => self.op_method(),
                OpCode::Invoke => self.op_invoke()?,
                OpCode::Inherit => self.op_inherit()?,
                OpCode::GetSuper => self.op_get_super()?,
                OpCode::SuperInvoke => self.op_super_invoke()?,
                OpCode::Return => {
                    let result = self.stack.pop().unwrap();
                    let prev_frame = self.frames.pop().unwrap();
                    let slot = prev_frame.slots;
                    self.close_upvalues(slot);
                    if self.frames.is_empty() {
                        self.stack.pop();
                        return Ok(());
                    }
                    // Pop the function and its params from the stack
                    self.stack.truncate(slot);
                    self.stack.push(result);
                }
            }
        }
    }

    // --- Ops Start

    fn op_constant(&mut self) {
        let constant = self.read_constant().clone();
        self.stack.push(constant);
    }

    fn op_negate(&mut self) -> Result<(), InterpretResult> {
        match self.peek_top(0).clone() {
            Value::Number(_) => {
                let value = self.stack.pop().unwrap();
                self.stack.push(-value);
                Ok(())
            }
            _ => self.runtime_error("Operand must be a number."),
        }
    }

    /// Adds the top two values on the stack.
    ///
    /// This function supports adding two numeric values or concatenating two strings.
    /// In the case of numeric values, it expects both operands to be `Value::Number`
    /// and pushes the result of their addition back onto the stack.
    /// For string values, it expects both operands to be `Value::Obj` containing `Obj::String`,
    /// concatenates them via new allocation, and pushes the result back onto the stack as a new `Obj::String`.
    ///
    /// # Errors
    ///
    /// Returns an `Err(InterpretResult::RuntimeError)` if:
    /// - The top two values on the stack are not both numbers or both strings.
    /// - The stack is underflowed (does not contain at least two values). [Should not happen]
    fn add_values(&mut self) -> Result<(), InterpretResult> {
        let right = self.stack.pop().unwrap();
        let left = self.stack.pop().unwrap();

        match (&left, &right) {
            (Value::Number(left_num), Value::Number(right_num)) => {
                self.stack.push(Value::Number(left_num + right_num));
            }
            (Value::Obj(left_obj), Value::Obj(right_obj)) => {
                if let (Obj::String(left_str), Obj::String(right_str)) =
                    (left_obj.deref(), right_obj.deref())
                {
                    let concatenated = format!("{}{}", left_str, right_str);
                    self.stack
                        .push(Value::Obj(Rc::new(Obj::String(concatenated.into()))));
                } else {
                    return self.runtime_error("Operands must be two numbers or two strings.");
                }
            }
            _ => return self.runtime_error("Operands must be two numbers or two strings."),
        }
        Ok(())
    }

    fn op_binary(&mut self, op_closure: fn(f64, f64) -> Value) -> Result<(), InterpretResult> {
        let b = self.peek_top(0);
        let a = self.peek_top(1);
        match (a, b) {
            (Value::Number(a), Value::Number(b)) => {
                let res = op_closure(*a, *b);
                self.stack.pop(); //b
                self.stack.pop(); //a
                self.stack.push(res);
                Ok(())
            }
            _ => self.runtime_error("Operands must be numbers."),
        }
    }

    fn op_not(&mut self) {
        let last = self.stack.pop().unwrap();
        self.stack.push(Value::Boolean(last.is_falsey()));
    }

    fn op_equal(&mut self) {
        // TODO(aalhendi): Unwrap unchecked everywhere
        let b = self.stack.pop().unwrap();
        let a = self.stack.pop().unwrap();
        self.stack.push(Value::Boolean(a == b));
    }

    fn op_pop(&mut self) {
        self.stack.pop();
    }

    fn op_define_global(&mut self) {
        let name = self.read_string();
        self.globals.insert(name, self.stack.pop().unwrap());
    }

    fn op_get_global(&mut self) -> Result<(), InterpretResult> {
        let name = self.read_string();
        match self.globals.get(&name) {
            Some(v) => {
                self.stack.push(v.clone());
                Ok(())
            }
            None => self.runtime_error(&format!("Undefined variable '{name}'.")),
        }
    }

    fn op_set_global(&mut self) -> Result<(), InterpretResult> {
        let name = self.read_string();
        let value = self.peek_top(0).clone();
        if let Entry::Occupied(mut e) = self.globals.entry(name.clone()) {
            e.insert(value);
            Ok(())
        } else {
            self.runtime_error(&format!("Undefined variable '{name}'."))
        }
    }

    fn op_get_local(&mut self) {
        let slot = self.read_byte() as usize;
        let slot_offset = self.frame().slots;
        self.stack.push(self.stack[slot + slot_offset].clone());
    }

    fn op_set_local(&mut self) {
        let slot = self.read_byte() as usize;
        let slot_offset = self.frame().slots;
        self.stack[slot + slot_offset] = self.peek_top(0).clone();
    }

    fn op_jump_if_false(&mut self) {
        let offset = self.read_short() as usize;
        if self.peek_top(0).is_falsey() {
            frame_mut!(self).ip += offset;
        }
    }

    fn op_jump(&mut self) {
        let offset = self.read_short() as usize;
        frame_mut!(self).ip += offset;
    }

    fn op_loop(&mut self) {
        let offset = self.read_short() as usize;
        frame_mut!(self).ip -= offset;
    }

    fn op_call(&mut self) -> Result<(), InterpretResult> {
        let arg_count = self.read_byte() as usize;
        let func_to_call = self.peek_top(arg_count).clone();
        self.call_value(&func_to_call, arg_count)
    }

    fn op_closure(&mut self) {
        let function = self.read_constant().as_closure().function.clone();
        let upvalue_count = function.upvalue_count;
        let mut closure = ObjClosure::new(function);
        for _ in 0..upvalue_count {
            let is_local = self.read_byte() == 1;
            let index = self.read_byte() as usize;
            let captured = if is_local {
                let idx = self.frame().slots + index;
                self.capture_upvalue(idx)
            } else {
                self.frame().closure.upvalues[index].clone()
            };
            closure.upvalues.push(captured);
        }
        self.stack
            .push(Value::Obj(Obj::Closure(Rc::new(closure)).into()));
    }

    fn op_get_upvalue(&mut self) {
        let slot = self.read_byte() as usize;
        let upvalue = self.frame().closure.upvalues[slot].borrow().clone();
        let value = match &upvalue.closed {
            Some(value) => value.clone(),
            None => self.stack[upvalue.location].clone(),
        };
        self.stack.push(value);
    }

    fn op_set_upvalue(&mut self) {
        let slot = self.read_byte() as usize;
        let value = self.peek_top(0).clone();
        let upvalue = self.frame().closure.upvalues[slot].borrow().clone();
        if upvalue.closed.is_none() {
            self.stack[upvalue.location] = value;
        } else {
            self.frame().closure.upvalues[slot].borrow_mut().closed = Some(value);
        }
    }

    fn op_close_upvalue(&mut self) {
        self.close_upvalues(self.stack.len() - 1);
        self.stack.pop();
    }

    fn op_class(&mut self) {
        let class_name = self.read_string();
        let value = Value::Obj(Obj::Class(Rc::new(RefCell::new(ObjClass::new(class_name)))).into());
        self.stack.push(value);
    }

    fn op_get_property(&mut self) -> Result<(), InterpretResult> {
        let instance_rc = if let Some(i) = self.peek_top(0).as_instance_maybe() {
            i.clone()
        } else {
            return self.runtime_error("Only instances have properties.");
        };
        let name = self.read_string();

        let instance = instance_rc.borrow();
        if let Some(value) = instance.fields.get(&name) {
            self.stack.pop(); // pop the instance
            self.stack.push(value.clone());
            Ok(())
        } else {
            self.bind_method(instance.klass.clone(), name)
        }
    }

    fn op_set_property(&mut self) -> Result<(), InterpretResult> {
        let instance_rc = if let Some(i) = self.peek_top(1).as_instance_maybe() {
            i.clone()
        } else {
            return self.runtime_error("Only instances have fields.");
        };

        let name = self.read_string();
        let value = self.peek_top(0).clone();
        {
            let mut instance = instance_rc.borrow_mut();
            instance.fields.insert(name, value);
        }

        // PERF(aalhendi): is `.remove(len-2)` faster? no pop/push and no clone
        let value = self.stack.pop().unwrap().clone();
        self.stack.pop(); // pop instance
        self.stack.push(value);
        Ok(())
    }

    fn op_method(&mut self) {
        let name = self.read_string();
        // Define method
        let method = self.peek_top(0);
        let klass = self.peek_top(1).as_class();
        klass.borrow_mut().methods.insert(name, method.clone());
        self.stack.pop();
    }

    fn op_invoke(&mut self) -> Result<(), InterpretResult> {
        let method = self.read_string();
        let arg_count = self.read_byte() as usize;
        self.invoke(&method, arg_count)
    }

    fn op_inherit(&mut self) -> Result<(), InterpretResult> {
        let superclass = if let Some(v) = self.peek_top(1).as_class_maybe() {
            v
        } else {
            return self.runtime_error("Superclass must be a class.");
        };
        let subclass = self.peek_top(0).as_class();

        // copy-down inheritance. works here because Lox classes are /closed/
        let super_methods = superclass.borrow().methods.clone();
        subclass.borrow_mut().methods.extend(super_methods);
        self.stack.pop(); // subclass
        Ok(())
    }

    fn op_get_super(&mut self) -> Result<(), InterpretResult> {
        let name = self.read_string();
        let superclass = self.stack.pop().unwrap().as_class().clone();
        self.bind_method(superclass, name)
    }

    fn op_super_invoke(&mut self) -> Result<(), InterpretResult> {
        let method = self.read_string();
        let arg_count = self.read_byte() as usize;
        let superclass = self.stack.pop().unwrap().as_class().clone();
        self.invoke_from_class(superclass, &method, arg_count)
    }

    // --- Ops End

    fn read_string(&mut self) -> Rc<str> {
        self.read_constant().as_string().clone()
    }

    fn peek_top(&self, distance: usize) -> &Value {
        let len = self.stack.len();
        &self.stack[len - 1 - distance]
    }

    fn call(&mut self, closure: &Rc<ObjClosure>, arg_count: usize) -> Result<(), InterpretResult> {
        if arg_count != closure.function.arity {
            return self.runtime_error(&format!(
                "Expected {arity} arguments but got {arg_count}.",
                arity = closure.function.arity
            ));
        }
        if self.frames.len() == FRAMES_MAX {
            return self.runtime_error("Stack overflow.");
        }
        let slots = self.stack.len() - arg_count - 1;
        let frame = CallFrame::new(closure.clone(), 0, slots);
        self.frames.push(frame);
        Ok(())
    }

    fn call_value(&mut self, callee: &Value, arg_count: usize) -> Result<(), InterpretResult> {
        match callee {
            Value::Obj(o) => match o.deref() {
                Obj::String(_) | Obj::Instance(_) => {
                    self.runtime_error("Can only call functions and classes.")
                }
                Obj::Native(f) => {
                    // From the stack top - arg count to the stack top
                    let slice = self.stack.len() - arg_count..self.stack.len();
                    let result = (f.function)(arg_count, &self.stack[slice]);
                    self.stack.truncate(self.stack.len() - (arg_count + 1));
                    self.stack.push(result);
                    Ok(())
                }
                // NOTE(aalhendi): All functions are closures
                Obj::Closure(c) => self.call(c, arg_count),
                Obj::Class(c) => {
                    let idx = self.stack.len() - arg_count - 1;
                    self.stack[idx] = Value::Obj(
                        Obj::Instance(Rc::new(RefCell::new(ObjInstance::new(c.clone())))).into(),
                    );
                    if let Some(initializer) = c.borrow().methods.get("init") {
                        let init = initializer.as_closure();
                        self.call(init, arg_count)
                    } else if arg_count != 0 {
                        return self
                            .runtime_error(&format!("Expected 0 arguments but got {arg_count}."));
                    } else {
                        Ok(())
                    }
                }
                Obj::BoundMethod(m) => {
                    let idx = self.stack.len() - arg_count - 1;
                    self.stack[idx] = m.receiver.clone();
                    self.call(&m.method, arg_count)
                }
            },
            _ => self.runtime_error("Can only call functions and classes."),
        }
    }

    fn invoke_from_class(
        &mut self,
        klass: Rc<RefCell<ObjClass>>,
        name: &str,
        arg_count: usize,
    ) -> Result<(), InterpretResult> {
        if let Some(method) = klass.borrow().methods.get(name) {
            let closure = method.as_closure();
            self.call(closure, arg_count)
        } else {
            self.runtime_error(&format!("Undefined property '{name}'."))
        }
    }

    fn invoke(&mut self, name: &str, arg_count: usize) -> Result<(), InterpretResult> {
        let receiver = self.peek_top(arg_count);
        let instance = if let Some(v) = receiver.as_instance_maybe() {
            v.borrow().clone()
        } else {
            return self.runtime_error("Only instances have methods.");
        };

        if let Some(value) = instance.fields.get(name) {
            let idx = self.stack.len() - arg_count - 1;
            self.stack[idx] = value.clone();
            return self.call_value(value, arg_count);
        }

        self.invoke_from_class(instance.klass, name, arg_count)
    }

    fn bind_method(
        &mut self,
        klass: Rc<RefCell<ObjClass>>,
        name: Rc<str>,
    ) -> Result<(), InterpretResult> {
        if let Some(method) = klass.borrow().methods.get(&name) {
            let closure = method.as_closure();
            let receiver = self.peek_top(0).clone();
            let bound = Value::Obj(
                Obj::BoundMethod(Rc::new(ObjBoundMethod::new(receiver, closure.clone()))).into(),
            );

            self.stack.pop();
            self.stack.push(bound);
            Ok(())
        } else {
            self.runtime_error(&format!("Undefined property '{name}'."))
        }
    }

    fn capture_upvalue(&mut self, local: usize) -> Rc<RefCell<ObjUpvalue>> {
        for upvalue in &self.open_upvalues {
            if upvalue.borrow().location == local {
                return upvalue.clone();
            }
        }
        let created_upvalue = Rc::new(RefCell::new(ObjUpvalue::new(local)));
        self.open_upvalues.push(created_upvalue.clone());
        created_upvalue
    }

    /// Closes all upvalues that have a stack index greater than or equal to `last`.
    fn close_upvalues(&mut self, last: usize) {
        // We iterate in reverse to avoid having to deal with changing indices after removal.
        let mut i = self.open_upvalues.len();
        while i != 0 {
            i -= 1; // Decrement first since we're going in reverse
            let upvalue_rc = &self.open_upvalues[i];
            let location = upvalue_rc.borrow().location;

            if location >= last {
                let closed_value = self.stack[location].clone();
                upvalue_rc.borrow_mut().closed = Some(closed_value);
                // NOTE(aalhendi): How expensive is remove?
                self.open_upvalues.remove(i);
            }
        }
    }

    fn runtime_error(&mut self, message: &str) -> Result<(), InterpretResult> {
        eprintln!("{message}");

        for frame in self.frames.iter().rev() {
            // -1 because IP already sitting on the next instruction to be executed
            // but we want stack trace to point to the previous failed instruction.
            let i = frame.ip - 1;
            let name = if frame.closure.function.name.is_empty() {
                "script".to_owned()
            } else {
                frame.closure.function.name.clone()
            };
            eprintln!(
                "[line {line}] in {name}()",
                line = frame.closure.function.chunk.lines[i]
            );
        }

        // Clear has no effect on capacity of vec
        self.stack.clear();

        Err(InterpretResult::RuntimeError)
    }

    /// Returns a reference to the current `CallFrame`.
    ///
    /// This function retrieves the last `CallFrame` from the VM's call stack,
    /// which represents the current execution context.
    #[inline]
    fn frame(&self) -> &CallFrame {
        self.frames.last().unwrap()
    }

    fn read_byte(&mut self) -> u8 {
        let frame = frame_mut!(self);
        let chunk = &frame.closure.function.chunk;
        let v = chunk.read_byte(frame.ip);
        frame.ip += 1;
        v
    }

    fn read_short(&mut self) -> u16 {
        let frame = frame_mut!(self);
        frame.ip += 2;
        let chunk = &frame.closure.function.chunk;
        let byte1 = chunk.read_byte(frame.ip - 2) as u16;
        let byte2 = chunk.read_byte(frame.ip - 1) as u16;
        (byte1 << 8) | byte2
    }

    fn read_constant(&mut self) -> &Value {
        let idx = self.read_byte() as usize;
        let frame = frame_mut!(self);
        let chunk = &frame.closure.function.chunk;
        &chunk.constants.values[idx]
    }
}

pub struct CallFrame2 {
    pub closure: *mut ObjClosure2,
    pub ip: *mut u8,
    pub slots: *mut Value2,
}

pub struct VM2 {
    frames: [CallFrame2; FRAMES_MAX],
    frame_count: usize,

    pub stack: [Value2; STACK_MAX],
    pub stack_top: *mut Value2,
    pub globals: Table,
    pub strings: Table,
    pub open_upvalues: *mut ObjUpvalue2, // Intrusive linked list

    pub bytes_allocated: usize,
    pub next_gc: usize,
    pub objects: *mut Obj2, // Intrusive linked list head
    pub gray_count: usize,
    pub gray_capacity: usize,
    // assume full responsibility for this array, including allocation failure.
    // If we can’t create or grow the gray stack, then we can’t finish the garbage collection.
    // this is bad news for the VM, but fortunately rare since the gray stack tends to be pretty small
    // TODO(aalhendi): do something more graceful
    pub gray_stack: *mut *mut Obj2,
}

impl VM2 {
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

        self.define_native("clock", native_clock2);
    }

    pub fn free(&mut self) {
        self.globals.free();
        self.strings.free();
        free_objects();
    }

    #[allow(non_snake_case)]
    // PERF(aalhendi): is macro faster?
    pub fn READ_BYTE(&mut self) -> u8 {
        let frame = self.get_frame();
        let byte = unsafe { *(*frame).ip };
        unsafe { (*frame).ip = (*frame).ip.offset(1) };
        byte
    }

    #[allow(non_snake_case)]
    fn READ_STRING(&mut self) -> *mut ObjString {
        self.READ_CONSTANT().as_string()
    }

    #[allow(non_snake_case)]
    pub fn READ_CONSTANT(&mut self) -> Value2 {
        let constant_index = self.READ_BYTE() as usize;
        let frame = self.get_frame();
        unsafe {
            *(*(*(*frame).closure).function)
                .chunk
                .constants
                .values
                .wrapping_add(constant_index)
        }
    }

    #[allow(non_snake_case)]
    pub fn READ_SHORT(&mut self) -> u16 {
        let frame = self.get_frame();
        let short = unsafe {
            let bytes = std::slice::from_raw_parts((*frame).ip, 2);
            ((bytes[0] as u16) << 8) | (bytes[1] as u16)
        };
        unsafe { (*frame).ip = (*frame).ip.offset(2) };
        short
    }

    fn get_frame(&mut self) -> *mut CallFrame2 {
        &mut self.frames[self.frame_count - 1]
    }

    pub fn run(&mut self) -> Result<(), InterpretResult> {
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
                let frame = self.get_frame();
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

            let instruction = OpCode::from(self.READ_BYTE());
            match instruction {
                OpCode::Print => {
                    println!("{}", self.pop());
                }
                OpCode::Jump => {
                    let offset = self.READ_SHORT();
                    let frame = self.get_frame();
                    unsafe {
                        (*frame).ip = (*frame).ip.wrapping_add(offset as usize);
                    }
                }
                OpCode::JumpIfFalse => {
                    let offset = self.READ_SHORT();
                    if self.peek(0).is_falsey() {
                        let frame = self.get_frame();
                        unsafe {
                            (*frame).ip = (*frame).ip.wrapping_add(offset as usize);
                        }
                    }
                }
                OpCode::Loop => {
                    let offset = self.READ_SHORT();
                    let frame = self.get_frame();
                    unsafe {
                        (*frame).ip = (*frame).ip.wrapping_sub(offset as usize);
                    }
                }
                OpCode::Call => {
                    let arg_count = self.READ_BYTE();
                    let value = self.peek(arg_count as isize);
                    if !self.call_value(value, arg_count) {
                        return Err(InterpretResult::RuntimeError);
                    }
                }
                OpCode::Closure => {
                    let function = self.READ_CONSTANT().as_function();
                    let closure = ObjClosure2::new(function);
                    self.push(Value2::obj_val(closure));
                    for i in 0..unsafe { (*closure).upvalue_count } {
                        let is_local = self.READ_BYTE();
                        let index = self.READ_BYTE();
                        if is_local != 0 {
                            unsafe {
                                let local = (*self.get_frame()).slots.wrapping_add(index as usize);
                                *(*closure).upvalues.wrapping_add(i as usize) =
                                    self.capture_upvalue(local);
                            }
                        } else {
                            unsafe {
                                *(*closure).upvalues.wrapping_add(i as usize) =
                                    *(*(*self.get_frame()).closure)
                                        .upvalues
                                        .wrapping_add(index as usize);
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
                    let old_frame = self.get_frame();
                    self.close_upvalues(unsafe { (*old_frame).slots });
                    self.frame_count -= 1;
                    if self.frame_count == 0 {
                        self.pop();
                        return Ok(());
                    }

                    self.stack_top = unsafe { (*old_frame).slots };
                    self.push(result);
                }
                OpCode::Constant => {
                    let constant = self.READ_CONSTANT();
                    self.push(constant);
                }
                OpCode::Negate => {
                    if !self.peek(0).is_number() {
                        self.runtime_error("Operand must be number.");
                        return Err(InterpretResult::RuntimeError);
                    }
                    let v = Value2::number_val(-self.pop().as_number());
                    self.push(v);
                }
                OpCode::Nil => self.push(Value2::nil_val()),
                OpCode::True => self.push(Value2::bool_val(true)),
                OpCode::False => self.push(Value2::bool_val(false)),
                OpCode::Pop => {
                    self.pop();
                }
                OpCode::GetLocal => {
                    let slot = self.READ_BYTE();
                    let frame = self.get_frame();
                    let val = unsafe { *(*frame).slots.wrapping_add(slot as usize) };
                    self.push(val);
                }
                OpCode::SetLocal => {
                    let slot = self.READ_BYTE();
                    let frame = self.get_frame();
                    unsafe {
                        *(*frame).slots.wrapping_add(slot as usize) = self.peek(0);
                    }
                }
                OpCode::GetGlobal => {
                    let name = self.READ_STRING();
                    #[allow(clippy::uninit_assumed_init)]
                    #[allow(invalid_value)]
                    let mut value = unsafe { MaybeUninit::uninit().assume_init() };
                    if !self.globals.get(name, &mut value) {
                        unsafe {
                            let name_deref = &*name;
                            self.runtime_error(&format!("Undefined variable '{name_deref}'."));
                        }
                    }
                    self.push(value);
                }
                OpCode::DefineGlobal => {
                    let name = self.READ_STRING();
                    self.globals.set(name, self.peek(0));
                    self.pop();
                }
                OpCode::SetGlobal => {
                    let name = self.READ_STRING();

                    if self.globals.set(name, self.peek(0)) {
                        self.globals.delete(name);
                        unsafe {
                            let name_deref = &*name;
                            self.runtime_error(&format!("Undefined variable '{name_deref}'."));
                        }
                    }
                }
                OpCode::GetUpvalue => {
                    let slot = self.READ_BYTE() as usize;
                    let v = unsafe {
                        *(*(*(*(*self.get_frame()).closure).upvalues.wrapping_add(slot))).location
                    };
                    self.push(v);
                }
                OpCode::SetUpvalue => {
                    let slot = self.READ_BYTE() as usize;
                    unsafe {
                        *(*(*(*(*self.get_frame()).closure).upvalues.wrapping_add(slot)))
                            .location = self.peek(0);
                    }
                }
                OpCode::SetProperty => {
                    if !self.peek(1).is_instance() {
                        self.runtime_error("Only instances have fields.");
                        return Err(InterpretResult::RuntimeError);
                    }

                    let instance = self.peek(1).as_instance();
                    unsafe {
                        (*instance).fields.set(self.READ_STRING(), self.peek(0));
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
                    let name = self.READ_STRING();
                    let mut value = unsafe { std::mem::zeroed() };
                    unsafe {
                        if (*instance).fields.get(name, &mut value) {
                            self.pop(); // instance
                            self.push(value);
                        } else {
                            self.runtime_error(&format!("Undefined property '{}'.", *name));
                        }
                    }
                }
                OpCode::Equal => {
                    let b = self.pop();
                    let a = self.pop();
                    self.push(Value2::bool_val(Value2::equal(a, b)));
                }
                OpCode::Greater => binary_op!(self, Value2::bool_val, >),
                OpCode::Less => binary_op!(self, Value2::bool_val, <),
                OpCode::Add => {
                    if self.peek(0).is_string() && self.peek(1).is_string() {
                        self.concatenate();
                    } else if self.peek(0).is_number() && self.peek(1).is_number() {
                        let b = self.pop().as_number();
                        let a = self.pop().as_number();
                        let v = Value2::number_val(a + b);
                        self.push(v);
                    } else {
                        self.runtime_error("Operands must be two numbers or two strings.");
                        return Err(InterpretResult::RuntimeError);
                    }
                }
                OpCode::Subtract => binary_op!(self, Value2::number_val, -),
                OpCode::Multiply => binary_op!(self, Value2::number_val, *),
                OpCode::Divide => binary_op!(self, Value2::number_val, /),
                OpCode::Not => {
                    let v = Value2::bool_val(self.pop().is_falsey());
                    self.push(v)
                }
                OpCode::Class => {
                    let v = Value2::obj_val(ObjClass2::new(self.READ_STRING()));
                    self.push(v);
                }
                OpCode::Method => {
                    let name = self.READ_STRING();
                    self.define_method(name);
                }
                _ => todo!(),
            }
        }
    }

    pub fn interpret(&mut self, source: String) -> Result<(), InterpretResult> {
        let function = compile(source);
        if function.is_null() {
            return Err(InterpretResult::CompileError);
        }

        self.push(Value2::obj_val(function));
        let closure = ObjClosure2::new(function);
        self.pop();
        self.push(Value2::obj_val(closure));
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
        self.push(Value2::obj_val(result));
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
        self.push(Value2::obj_val(Obj2::copy_string(
            name.as_bytes(),
            name.as_bytes().len(),
        )));
        self.push(Value2::obj_val(ObjNative2::new(function)));
        self.globals.set(self.stack[0].as_string(), self.stack[1]);
        self.pop();
        self.pop();
    }

    pub fn push(&mut self, value: Value2) {
        unsafe { *self.stack_top = value };
        self.stack_top = self.stack_top.wrapping_add(1);
    }

    pub fn pop(&mut self) -> Value2 {
        self.stack_top = self.stack_top.wrapping_sub(1);
        unsafe { *self.stack_top }
    }

    fn peek(&self, distance: isize) -> Value2 {
        unsafe { *self.stack_top.offset(-1 - distance) }
    }

    fn call(&mut self, closure: *mut ObjClosure2, arg_count: u8) -> bool {
        let arity = unsafe { (*(*closure).function).arity } as u8;
        if arg_count != arity {
            self.runtime_error(&format!("Expected {arity} arguments but got {arg_count}"));
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

    fn call_value(&mut self, callee: Value2, arg_count: u8) -> bool {
        if callee.is_obj() {
            match callee.obj_type() {
                ObjType::Closure => return self.call(callee.as_closure(), arg_count),
                ObjType::Function => unreachable!("Bare ObjFunction calls are not implemented. ObjFunctions are always wrapped in ObjClosures."),
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
                        *self.stack_top.wrapping_sub(arg_count as usize + 1) = Value2::obj_val(ObjInstance2::new(class));
                    }
                    return true;
                }
                ObjType::String | ObjType::Upvalue | ObjType::Instance => { /* Non-Callable Object Type */ }
            }
        }

        self.runtime_error("Can only call funcitons and classes.");
        false
    }

    fn capture_upvalue(&mut self, local: *mut Value2) -> *mut ObjUpvalue2 {
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

    fn close_upvalues(&mut self, last: *mut Value2) {
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
