use std::{
    cell::RefCell,
    fmt::Display,
    mem,
    ops::{Deref, Neg},
    ptr::null_mut,
    rc::Rc,
};

use crate::{memory::reallocate, FREE_ARRAY};
use crate::{
    object::{Obj, ObjClass, ObjClosure, ObjInstance},
    GROW_ARRAY, GROW_CAPACITY,
};

pub type Value2 = f64;

pub struct ValueArray2 {
    pub capacity: isize,
    pub count: isize,
    pub values: *mut Value2,
}

impl ValueArray2 {
    pub fn init(&mut self) {
        self.values = null_mut();
        self.capacity = 0;
        self.count = 0;
    }

    pub fn write(&mut self, value: Value2) {
        if self.capacity < self.count + 1 {
            let old_capacity = self.capacity;
            self.capacity = GROW_CAPACITY!(old_capacity);
            self.values = GROW_ARRAY!(
                Value2,
                self.values,
                old_capacity as usize,
                self.capacity as usize
            );
        }

        unsafe { *self.values.offset(self.count) = value };
        self.count += 1;
    }

    pub fn free(&mut self) {
        FREE_ARRAY!(Value2, self.values, self.capacity as usize);
        self.init();
    }

    #[cfg(any(feature = "debug-trace-execution", feature = "debug-print-code"))]
    pub fn print_value(&self, value: Value2, terminator: Option<char>) {
        if let Some(t) = terminator {
            println!("{value}{t}")
        } else {
            println!("{value}")
        }
    }
}

#[derive(Debug, Clone, PartialEq, PartialOrd)]
pub enum Value {
    Number(f64),
    Boolean(bool),
    Nil,
    Obj(Rc<Obj>),
}

impl Value {
    // NOTE: Lox follows ruby in that only false and nil are false in lox
    // TODO(aalhendi): just impl bool?
    pub fn is_falsey(&self) -> bool {
        match self {
            Value::Number(_) => false,
            Value::Obj(_) => false,
            Value::Boolean(v) => !v,
            Value::Nil => true,
        }
    }

    pub fn as_string(&self) -> &Rc<str> {
        match self {
            Value::Obj(o) => match o.deref() {
                Obj::String(c) => c,
                _ => unreachable!("Must be a string"),
            },
            _ => unreachable!("Must be a string"),
        }
    }

    pub fn as_closure(&self) -> &Rc<ObjClosure> {
        match self {
            Value::Obj(o) => match o.deref() {
                Obj::Closure(c) => c,
                _ => unreachable!("Must be a closure"),
            },
            _ => unreachable!("Must be a closure"),
        }
    }

    pub fn as_class(&self) -> &Rc<RefCell<ObjClass>> {
        match self {
            Value::Obj(o) => match o.deref() {
                Obj::Class(c) => c,
                _ => unreachable!("Must be a class"),
            },
            _ => unreachable!("Must be a class"),
        }
    }

    pub fn as_instance_maybe(&self) -> Option<&Rc<RefCell<ObjInstance>>> {
        match self {
            Value::Obj(o) => match o.deref() {
                Obj::Instance(i) => Some(i),
                _ => None,
            },
            _ => None,
        }
    }

    pub fn as_class_maybe(&self) -> Option<&Rc<RefCell<ObjClass>>> {
        match self {
            Value::Obj(o) => match o.deref() {
                Obj::Class(v) => Some(v),
                _ => None,
            },
            _ => None,
        }
    }
}

impl Display for Value {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Value::Number(v) => write!(f, "{v}"),
            Value::Boolean(v) => write!(f, "{v}"),
            Value::Nil => write!(f, "nil"),
            Value::Obj(v) => write!(f, "{v}"),
        }
    }
}

impl Neg for Value {
    type Output = Self;

    fn neg(self) -> Self::Output {
        match self {
            Value::Number(v) => Self::Number(-v),
            _ => unreachable!(),
        }
    }
}

#[derive(Debug, Clone, PartialEq, PartialOrd)]
pub struct ValueArray {
    pub values: Vec<Value>,
}

impl ValueArray {
    pub fn new() -> Self {
        Self {
            values: Vec::with_capacity(8),
        }
    }

    pub fn write(&mut self, value: Value) {
        self.values.push(value);
    }

    pub fn free(&mut self) {
        self.values = Vec::new();
    }

    #[cfg(any(feature = "debug-trace-execution", feature = "debug-print-code"))]
    pub fn print_value(&self, constant_idx: usize, terminator: Option<char>) {
        if let Some(t) = terminator {
            println!("{v}{t}", v = self.values[constant_idx])
        } else {
            println!("{v}", v = self.values[constant_idx])
        }
    }
}
