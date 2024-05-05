use std::{
    cell::RefCell,
    fmt::Display,
    ops::{Deref, Neg},
    rc::Rc,
};

use crate::object::{Obj, ObjClass, ObjClosure, ObjInstance};

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
