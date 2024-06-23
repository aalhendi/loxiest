use std::mem::MaybeUninit;
use std::{fmt::Display, mem, ptr::null_mut};

use crate::memory::reallocate;
use crate::value::{Value, Value2, ValueArray, ValueArray2};
use crate::{FREE_ARRAY, GROW_ARRAY, GROW_CAPACITY, VM};

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
        match value {
            0 => OpCode::Constant,
            1 => OpCode::Nil,
            2 => OpCode::True,
            3 => OpCode::False,
            4 => OpCode::Pop,
            5 => OpCode::GetLocal,
            6 => OpCode::SetLocal,
            7 => OpCode::GetGlobal,
            8 => OpCode::DefineGlobal,
            9 => OpCode::SetGlobal,
            10 => OpCode::GetUpvalue,
            11 => OpCode::SetUpvalue,
            12 => OpCode::GetProperty,
            13 => OpCode::SetProperty,
            14 => OpCode::GetSuper,
            15 => OpCode::Equal,
            16 => OpCode::Greater,
            17 => OpCode::Less,
            18 => OpCode::Add,
            19 => OpCode::Subtract,
            20 => OpCode::Multiply,
            21 => OpCode::Divide,
            22 => OpCode::Not,
            23 => OpCode::Negate,
            24 => OpCode::Print,
            25 => OpCode::Jump,
            26 => OpCode::JumpIfFalse,
            27 => OpCode::Loop,
            28 => OpCode::Call,
            29 => OpCode::Invoke,
            30 => OpCode::SuperInvoke,
            31 => OpCode::Closure,
            32 => OpCode::CloseUpvalue,
            33 => OpCode::Return,
            34 => OpCode::Class,
            35 => OpCode::Inherit,
            36 => OpCode::Method,
            _ => panic!("Unknown opcode {value}"),
        }
    }
}

pub struct Chunk2 {
    capacity: isize,
    pub count: isize,
    pub code: *mut u8,
    pub lines: *mut isize,
    pub constants: ValueArray2,
}

impl Chunk2 {
    #[allow(clippy::uninit_assumed_init)]
    #[allow(invalid_value)]
    pub fn init(&mut self) {
        self.capacity = 0;
        self.count = 0;
        self.code = null_mut();
        self.lines = null_mut();
        self.constants = unsafe { MaybeUninit::uninit().assume_init() };
        self.constants.init();
    }

    pub fn write(&mut self, byte: u8, line: isize) {
        if self.capacity < self.count + 1 {
            let old_capacity = self.capacity;
            self.capacity = GROW_CAPACITY!(old_capacity);
            self.code = GROW_ARRAY!(u8, self.code, old_capacity as usize, self.capacity as usize);
            self.lines = GROW_ARRAY!(
                isize,
                self.lines,
                old_capacity as usize,
                self.capacity as usize
            );
        }

        unsafe { *self.code.offset(self.count) = byte };
        unsafe { *self.lines.offset(self.count) = line };
        self.count += 1;
    }

    pub fn add_constant(&mut self, value: Value2) -> isize {
        unsafe { VM.push(value) };
        self.constants.write(value);
        unsafe { VM.pop() };
        self.constants.count - 1
    }

    pub fn free(&mut self) {
        FREE_ARRAY!(u8, self.code, self.capacity as usize);
        FREE_ARRAY!(isize, self.lines, self.capacity as usize);
        self.constants.free();
        self.init();
    }

    #[cfg(any(feature = "debug-trace-execution", feature = "debug-print-code"))]
    pub fn disassemble<T: Display>(&self, name: T) {
        println!("== {name} ==");

        let mut offset = 0;
        while offset < self.count as usize {
            offset = self.disassemble_instruction(offset);
        }
    }

    #[cfg(feature = "debug-print-code")]
    pub fn disassemble_instruction(&self, offset: usize) -> usize {
        print!("{offset:04} ");

        let line = unsafe { *self.lines.wrapping_add(offset) };
        if offset > 0 && line == unsafe { *self.lines.wrapping_add(offset - 1) } {
            print!("   | ");
        } else {
            print!("{line:4} ");
        }

        let instruction = OpCode::from(unsafe { *self.code.wrapping_add(offset) });
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
                let constant_idx = unsafe { *self.code.wrapping_add(idx) as usize };
                idx += 1;
                print!("{name:-16} {constant_idx:4} ", name = "OP_CLOSURE");
                let value = unsafe { *self.constants.values.wrapping_add(constant_idx) };
                self.constants.print_value(value, None);

                let function =
                    unsafe { (*self.constants.values.wrapping_add(constant_idx)).as_function() };
                for _ in 0..unsafe { (*function).upvalue_count } {
                    let is_local = if unsafe { *self.code.wrapping_add(idx) } == 0 {
                        "upvalue"
                    } else {
                        "local"
                    };
                    idx += 1;
                    let index = unsafe { *self.code.wrapping_add(idx) };
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
    fn simple_instruction(&self, code: OpCode, offset: usize) -> usize {
        println!("{code}");
        offset + 1
    }

    #[cfg(any(feature = "debug-trace-execution", feature = "debug-print-code"))]
    fn byte_instruction(&self, name: OpCode, offset: usize) -> usize {
        let slot = unsafe { *self.code.wrapping_add(offset + 1) };
        let name = name.to_string();
        println!("{name:-16} {slot:4}");
        offset + 2
    }

    #[cfg(any(feature = "debug-trace-execution", feature = "debug-print-code"))]
    fn constant_instruction(&self, code: OpCode, offset: usize) -> usize {
        // TODO(aalhendi): check type
        let constant_idx = unsafe { *self.code.wrapping_add(offset + 1) } as usize;
        let name = code.to_string(); // This makes formatting work for some reason
        print!("{name:-16} {constant_idx:4} '");
        let value = unsafe { *self.constants.values.wrapping_add(constant_idx) };
        self.constants.print_value(value, Some('\''));
        offset + 2
    }

    #[cfg(any(feature = "debug-trace-execution", feature = "debug-print-code"))]
    fn jump_instruction(&self, name: OpCode, is_neg: bool, offset: usize) -> usize {
        let name = name.to_string();
        let jump = unsafe {
            (((*self.code.wrapping_add(offset + 1)) as u16) << 8)
                | ((*self.code.wrapping_add(offset + 2)) as u16)
        };

        // NOTE: This could underflow and that would be a bug in the impl so it shouldn't.
        let dst = match is_neg {
            true => offset + 3 - jump as usize,
            false => offset + 3 + jump as usize,
        };
        println!("{name:-16} {offset:4} -> {dst}",);
        offset + 3
    }

    #[cfg(any(feature = "debug-trace-execution", feature = "debug-print-code"))]
    fn invoke_instruction(&self, name: OpCode, offset: usize) -> usize {
        let constant_idx = unsafe {*self.code.wrapping_add(offset + 1)} as usize;
        let arg_count = unsafe {*self.code.wrapping_add(offset + 2)} as usize;
        let name = name.to_string();

        print!("{name:-16} ({arg_count} args) {constant_idx:4} '");
        let value = unsafe { *self.constants.values.wrapping_add(constant_idx) };
        self.constants.print_value(value, Some('\''));
        offset + 3
    }
}

#[derive(Debug, Clone, PartialEq, PartialOrd)]
pub struct Chunk {
    pub code: Vec<u8>,
    pub constants: ValueArray,
    pub lines: Vec<usize>,
}

impl Chunk {
    pub fn new() -> Self {
        Self {
            code: Vec::with_capacity(8),
            constants: ValueArray::new(),
            lines: Vec::with_capacity(8),
        }
    }

    pub fn count(&self) -> usize {
        self.lines.len()
    }

    pub fn write_byte<T: Into<u8>>(&mut self, byte: T, line: usize) {
        self.code.push(byte.into());
        self.lines.push(line);
    }

    #[inline]
    pub fn read_byte(&self, ip_idx: usize) -> u8 {
        self.code[ip_idx]
    }

    pub fn add_constant(&mut self, value: Value) -> Option<u8> {
        self.constants.write(value);
        u8::try_from(self.constants.values.len() - 1).ok()
    }

    pub fn free(&mut self) {
        self.code = Vec::new();
        self.lines = Vec::new();
        self.constants.free();
    }

    #[cfg(any(feature = "debug-trace-execution", feature = "debug-print-code"))]
    fn simple_instruction(&self, code: OpCode, offset: usize) -> usize {
        println!("{code}");
        offset + 1
    }

    #[cfg(any(feature = "debug-trace-execution", feature = "debug-print-code"))]
    fn byte_instruction(&self, name: OpCode, offset: usize) -> usize {
        let slot = self.code[offset + 1];
        let name = name.to_string();
        println!("{name:-16} {slot:4}");
        offset + 2
    }

    #[cfg(any(feature = "debug-trace-execution", feature = "debug-print-code"))]
    fn constant_instruction(&self, code: OpCode, offset: usize) -> usize {
        let constant_idx = self.code[offset + 1] as usize;
        let name = code.to_string(); // This makes formatting work for some reason
        print!("{name:-16} {constant_idx:4} '");
        self.constants.print_value(constant_idx, Some('\''));
        offset + 2
    }

    #[cfg(any(feature = "debug-trace-execution", feature = "debug-print-code"))]
    fn jump_instruction(&self, name: OpCode, is_neg: bool, offset: usize) -> usize {
        let name = name.to_string();
        let jump = ((self.code[offset + 1] as u16) << 8) | (self.code[offset + 2] as u16);

        // NOTE: This could underflow and that would be a bug in the impl so it shouldn't.
        let dst = match is_neg {
            true => offset + 3 - jump as usize,
            false => offset + 3 + jump as usize,
        };
        println!("{name:-16} {offset:4} -> {dst}",);
        offset + 3
    }

    #[cfg(any(feature = "debug-trace-execution", feature = "debug-print-code"))]
    fn invoke_instruction(&self, name: OpCode, offset: usize) -> usize {
        let constant_idx = self.code[offset + 1] as usize;
        let arg_count = self.code[offset + 2] as usize;
        let name = name.to_string();

        print!("{name:-16} ({arg_count} args) {constant_idx:4} '");
        self.constants.print_value(constant_idx, Some('\''));
        offset + 3
    }

    #[cfg(feature = "debug-print-code")]
    pub fn disassemble_instruction(&self, offset: usize) -> usize {
        print!("{offset:04} ");

        if offset > 0 && self.lines[offset] == self.lines[offset - 1] {
            print!("   | ");
        } else {
            print!("{line:4} ", line = self.lines[offset]);
        }

        let instruction = OpCode::from(self.code[offset]);
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
                let constant_idx = self.code[idx] as usize;
                print!("{name:-16} {constant_idx:4} ", name = "OP_CLOSURE");
                self.constants.print_value(constant_idx, None);

                let c = self.constants.values[constant_idx].as_closure();
                idx += 1;
                for _ in 0..c.function.upvalue_count {
                    let is_local = if self.code[idx] == 0 {
                        "upvalue"
                    } else {
                        "local"
                    };
                    idx += 1;
                    let index = self.code[idx];
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
    pub fn disassemble<T: Display>(&self, name: T) {
        println!("== {name} ==");

        let mut offset = 0;
        while offset < self.code.len() {
            offset = self.disassemble_instruction(offset);
        }
    }
}
