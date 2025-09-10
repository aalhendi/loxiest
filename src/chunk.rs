use std::{fmt::Display, mem};

use crate::memory::reallocate;
use crate::value::{Value, ValueArray};
use crate::{vm, FREE_ARRAY, GROW_ARRAY, GROW_CAPACITY};

#[derive(Debug)]
#[repr(u8)]
pub enum OpCode {
    Constant,
    Nil,
    True,
    False,
    Pop,
    GetLocal,
    SetLocal,
    GetGlobal,
    DefineGlobal,
    SetGlobal,
    GetUpvalue,
    SetUpvalue,
    GetProperty,
    SetProperty,
    GetSuper,
    Equal,
    Greater,
    Less,
    Add,
    Subtract,
    Multiply,
    Divide,
    Not,
    Negate,
    Print,
    Jump,
    JumpIfFalse,
    Loop,
    Call,
    Invoke,
    SuperInvoke,
    Closure,
    CloseUpvalue,
    Return,
    Class,
    Inherit,
    Method,
}

impl From<OpCode> for u8 {
    fn from(opcode: OpCode) -> Self {
        opcode as u8
    }
}

impl Display for OpCode {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let out = match self {
            OpCode::Constant => "OP_CONSTANT",
            OpCode::Nil => "OP_NIL",
            OpCode::True => "OP_TRUE",
            OpCode::False => "OP_FALSE",
            OpCode::Pop => "OP_POP",
            OpCode::GetLocal => "OP_GET_LOCAL",
            OpCode::SetLocal => "OP_SET_LOCAL",
            OpCode::GetGlobal => "OP_GET_GLOBAL",
            OpCode::DefineGlobal => "OP_DEFINE_GLOBAL",
            OpCode::SetGlobal => "OP_SET_GLOBAL",
            OpCode::GetUpvalue => "OP_GET_UPVALUE",
            OpCode::SetUpvalue => "OP_SET_UPVALUE",
            OpCode::GetProperty => "OP_GET_PROPERTY",
            OpCode::SetProperty => "OP_SET_PROPERTY",
            OpCode::GetSuper => "OP_GET_SUPER",
            OpCode::Equal => "OP_EQUAL",
            OpCode::Greater => "OP_GREATER",
            OpCode::Less => "OP_LESS",
            OpCode::Add => "OP_ADD",
            OpCode::Subtract => "OP_SUBTRACT",
            OpCode::Multiply => "OP_MULTIPLY",
            OpCode::Divide => "OP_DIVIDE",
            OpCode::Not => "OP_NOT",
            OpCode::Negate => "OP_NEGATE",
            OpCode::Print => "OP_PRINT",
            OpCode::Jump => "OP_JUMP",
            OpCode::JumpIfFalse => "OP_JUMP_IF_FALSE",
            OpCode::Loop => "OP_LOOP",
            OpCode::Call => "OP_CALL",
            OpCode::Invoke => "OP_INVOKE",
            OpCode::SuperInvoke => "OP_SUPER_INVOKE",
            OpCode::Closure => "OP_CLOSURE",
            OpCode::CloseUpvalue => "OP_CLOSE_UPVALUE",
            OpCode::Return => "OP_RETURN",
            OpCode::Class => "OP_CLASS",
            OpCode::Inherit => "OP_INHERIT",
            OpCode::Method => "OP_METHOD",
        };
        write!(f, "{out}")
    }
}

impl From<u8> for OpCode {
    fn from(value: u8) -> Self {
        debug_assert!(value as usize <= std::mem::variant_count::<OpCode>());
        unsafe { std::mem::transmute::<u8, OpCode>(value) }
    }
}

#[repr(C)]
pub struct Chunk {
    pub count: i32,
    capacity: i32,
    pub code: *mut u8,
    pub lines: *mut i32,
    pub constants: ValueArray,
}

impl Default for Chunk {
    fn default() -> Self {
        Self {
            capacity: 0,
            count: 0,
            code: std::ptr::null_mut(),
            lines: std::ptr::null_mut(),
            constants: ValueArray::default(),
        }
    }
}

impl Chunk {
    pub fn free(&mut self) {
        FREE_ARRAY!(u8, self.code, self.capacity as usize);
        FREE_ARRAY!(usize, self.lines, self.capacity as usize);
        self.constants.free();
        *self = Self::default();
    }

    pub fn write(&mut self, byte: u8, line: i32) {
        if self.capacity < self.count + 1 {
            let old_capacity = self.capacity;
            self.capacity = GROW_CAPACITY!(old_capacity);
            self.code = GROW_ARRAY!(u8, self.code, old_capacity as usize, self.capacity as usize);
            self.lines = GROW_ARRAY!(i32, self.lines, old_capacity as usize, self.capacity as usize);
        }

        unsafe {
            *self.code.offset(self.count as isize) = byte;
            *self.lines.offset(self.count as isize) = line;
        }
        self.count += 1;
    }

    pub fn add_constant(&mut self, value: Value) -> i32 {
        vm().push(value);
        self.constants.write(value);
        vm().pop();
        self.constants.count - 1
    }

    #[cfg(any(feature = "debug-trace-execution", feature = "debug-print-code"))]
    pub fn disassemble<T: Display>(&self, name: T) {
        println!("== {name} ==");

        let mut offset = 0;
        while offset < self.count {
            offset = self.disassemble_instruction(offset);
        }
    }

    #[cfg(feature = "debug-print-code")]
    pub fn disassemble_instruction(&self, offset: i32) -> i32 {
        print!("{offset:04} ");

        let line = unsafe { *self.lines.wrapping_offset(offset as isize) };
        if offset > 0 && line == unsafe { *self.lines.wrapping_offset((offset - 1) as isize) } {
            print!("   | ");
        } else {
            print!("{line:4} ");
        }

        let instruction = OpCode::from(unsafe { *self.code.wrapping_offset(offset as isize) });
        match instruction {
            OpCode::Return
            | OpCode::Negate
            | OpCode::Add
            | OpCode::Subtract
            | OpCode::Multiply
            | OpCode::Divide
            | OpCode::False
            | OpCode::True
            | OpCode::Nil
            | OpCode::Not
            | OpCode::Equal
            | OpCode::Greater
            | OpCode::Less
            | OpCode::Print
            | OpCode::Pop
            | OpCode::CloseUpvalue
            | OpCode::Inherit => self.simple_instruction(instruction, offset),

            OpCode::Constant
            | OpCode::DefineGlobal
            | OpCode::GetGlobal
            | OpCode::SetGlobal
            | OpCode::Class
            | OpCode::GetProperty
            | OpCode::SetProperty
            | OpCode::Method
            | OpCode::GetSuper => self.constant_instruction(instruction, offset),

            OpCode::GetLocal
            | OpCode::SetLocal
            | OpCode::Call
            | OpCode::GetUpvalue
            | OpCode::SetUpvalue => self.byte_instruction(instruction, offset),

            OpCode::Jump | OpCode::JumpIfFalse | OpCode::Loop => {
                let is_loop = matches!(instruction, OpCode::Loop);
                self.jump_instruction(instruction, is_loop, offset)
            }

            OpCode::Invoke | OpCode::SuperInvoke => self.invoke_instruction(instruction, offset),

            OpCode::Closure => {
                let mut idx = offset + 1;
                let constant_idx = unsafe { *self.code.wrapping_offset(idx as isize) as isize };
                idx += 1;
                print!("{name:-16} {constant_idx:4} ", name = "OP_CLOSURE");
                let value = unsafe { *self.constants.values.wrapping_offset(constant_idx) };
                self.constants.print_value(value, None);

                let function =
                    unsafe { (*self.constants.values.wrapping_offset(constant_idx)).as_function() };
                for _ in 0..unsafe { (*function).upvalue_count } {
                    let is_local = if unsafe { *self.code.wrapping_offset(idx as isize) } == 0 {
                        "upvalue"
                    } else {
                        "local"
                    };
                    idx += 1;
                    let index = unsafe { *self.code.wrapping_offset(idx as isize) };
                    idx += 1;
                    println!(
                        "{:04}      |                     {is_local} {index}",
                        idx - 2
                    );
                }

                idx
            }
        }
    }

    #[cfg(any(feature = "debug-trace-execution", feature = "debug-print-code"))]
    fn simple_instruction(&self, code: OpCode, offset: i32) -> i32 {
        println!("{code}");
        offset + 1
    }

    #[cfg(any(feature = "debug-trace-execution", feature = "debug-print-code"))]
    fn byte_instruction(&self, name: OpCode, offset: i32) -> i32 {
        let slot = unsafe { *self.code.wrapping_offset(offset as isize + 1) };
        let name = name.to_string();
        println!("{name:-16} {slot:4}");
        offset + 2
    }

    #[cfg(any(feature = "debug-trace-execution", feature = "debug-print-code"))]
    fn constant_instruction(&self, code: OpCode, offset: i32) -> i32 {
        // TODO(aalhendi): check type
        let constant_idx = unsafe { *self.code.wrapping_offset(offset as isize + 1) } as isize;
        let name = code.to_string(); // This makes formatting work for some reason
        print!("{name:-16} {constant_idx:4} '");
        let value = unsafe { *self.constants.values.wrapping_offset(constant_idx) };
        self.constants.print_value(value, Some('\''));
        offset + 2
    }

    #[cfg(any(feature = "debug-trace-execution", feature = "debug-print-code"))]
    fn jump_instruction(&self, name: OpCode, is_neg: bool, offset: i32) -> i32 {
        let name = name.to_string();
        let jump = unsafe {
            (((*self.code.wrapping_offset(offset as isize + 1)) as i32) << 8)
                | ((*self.code.wrapping_offset(offset as isize + 2)) as i32)
        };

        // NOTE: This could underflow and that would be a bug in the impl so it shouldn't.
        let dst = match is_neg {
            true => offset + 3 - jump,
            false => offset + 3 + jump,
        };
        println!("{name:-16} {offset:4} -> {dst}",);
        offset + 3
    }

    #[cfg(any(feature = "debug-trace-execution", feature = "debug-print-code"))]
    fn invoke_instruction(&self, name: OpCode, offset: i32) -> i32 {
        let constant_idx = unsafe { *self.code.wrapping_offset(offset as isize + 1) } as isize;
        let arg_count = unsafe { *self.code.wrapping_offset(offset as isize + 2) };
        let name = name.to_string();

        print!("{name:-16} ({arg_count} args) {constant_idx:4} '");
        let value = unsafe { *self.constants.values.wrapping_offset(constant_idx) };
        self.constants.print_value(value, Some('\''));
        offset + 3
    }
}
