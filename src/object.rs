use std::{cell::RefCell, collections::HashMap, fmt::Display, rc::Rc, time::SystemTime};

use crate::{chunk::Chunk, value::Value};

#[derive(Debug, Clone, PartialOrd)]
pub enum Obj {
    String(Rc<str>), // Lox strings are immutable by default
    Native(ObjNative),
    Closure(Rc<ObjClosure>),
    Class(Rc<RefCell<ObjClass>>),
    Instance(Rc<RefCell<ObjInstance>>),
    BoundMethod(Rc<ObjBoundMethod>),
}

impl PartialEq for Obj {
    fn eq(&self, other: &Self) -> bool {
        match (self, other) {
            (Self::String(l0), Self::String(r0)) => l0 == r0,
            (Self::Native(l0), Self::Native(r0)) => l0 == r0,
            (Self::Closure(l0), Self::Closure(r0)) => l0 == r0,
            (Self::Class(l0), Self::Class(r0)) => l0 == r0,
            (Self::Instance(l0), Self::Instance(r0)) => l0 == r0,
            (Self::BoundMethod(l0), Self::BoundMethod(r0)) => Rc::ptr_eq(l0, r0),
            _ => false,
        }
    }
}

impl Display for Obj {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Obj::String(v) => write!(f, "{v}"),
            Obj::Native(v) => write!(f, "{v}"),
            Obj::Closure(v) => write!(f, "{v}"),
            Obj::Class(v) => write!(f, "{}", v.borrow()),
            Obj::Instance(v) => write!(f, "{}", v.borrow()),
            Obj::BoundMethod(v) => write!(f, "{v}"),
        }
    }
}

#[derive(Debug, Clone, PartialEq, PartialOrd)]
pub struct ObjBoundMethod {
    pub receiver: Value,
    pub method: Rc<ObjClosure>,
}

impl ObjBoundMethod {
    pub fn new(receiver: Value, method: Rc<ObjClosure>) -> Self {
        Self { receiver, method }
    }
}

impl Display for ObjBoundMethod {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{name}", name = self.method.function)
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct ObjClass {
    pub name: Rc<str>,
    pub methods: HashMap<Rc<str>, Value>,
}

impl ObjClass {
    pub fn new(name: Rc<str>) -> Self {
        Self {
            name,
            methods: HashMap::new(),
        }
    }
}

impl PartialOrd for ObjClass {
    fn partial_cmp(&self, _other: &Self) -> Option<std::cmp::Ordering> {
        todo!("Placeholder")
    }
}

impl Display for ObjClass {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{name}", name = self.name)
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct ObjInstance {
    pub fields: HashMap<Rc<str>, Value>,
    pub klass: Rc<RefCell<ObjClass>>,
}

impl PartialOrd for ObjInstance {
    fn partial_cmp(&self, _other: &Self) -> Option<std::cmp::Ordering> {
        todo!("Placeholder")
    }
}

impl ObjInstance {
    pub fn new(klass: Rc<RefCell<ObjClass>>) -> Self {
        Self {
            klass,
            fields: HashMap::new(),
        }
    }
}

impl Display for ObjInstance {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{name} instance", name = self.klass.borrow().name)
    }
}

#[derive(Debug, Clone, PartialEq, PartialOrd)]
pub struct ObjUpvalue {
    pub location: usize, // index into the stack, serving as a ptr
    pub closed: Option<Value>,
}

impl ObjUpvalue {
    pub fn new(slot: usize) -> Self {
        Self {
            location: slot,
            closed: None,
        }
    }
}

impl Display for ObjUpvalue {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "upvalue")
    }
}

#[derive(Debug, Clone, PartialEq, PartialOrd)]
pub struct ObjClosure {
    pub function: Rc<ObjFunction>,
    pub upvalues: Vec<Rc<RefCell<ObjUpvalue>>>,
}

impl ObjClosure {
    pub fn new(function: Rc<ObjFunction>) -> Self {
        Self {
            upvalues: Vec::with_capacity(function.upvalue_count),
            function,
        }
    }
}

impl Display for ObjClosure {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let name = format!("{}", self.function);
        write!(f, "{name}")
    }
}

pub fn native_clock(_arg_count: usize, _args: &[Value]) -> Value {
    match SystemTime::now().duration_since(SystemTime::UNIX_EPOCH) {
        Ok(n) => Value::Number(n.as_secs_f64()),
        Err(e) => panic!("{e}"),
    }
}

pub type NativeFn = fn(arg_count: usize, args: &[Value]) -> Value;

#[derive(Debug, Clone, PartialEq, PartialOrd)]
pub struct ObjNative {
    pub function: NativeFn,
}

impl ObjNative {
    pub fn new(function: NativeFn) -> Self {
        Self { function }
    }
}

impl Display for ObjNative {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "<native fn>")
    }
}

#[derive(Debug, Clone, PartialEq, PartialOrd)]
pub struct ObjFunction {
    pub arity: usize,
    pub chunk: Chunk,
    pub name: String,
    pub upvalue_count: usize,
}

impl ObjFunction {
    pub fn new(name: &str, arity: usize) -> Self {
        Self {
            arity,
            chunk: Chunk::new(),
            name: name.to_string(),
            upvalue_count: 0,
        }
    }
}

impl Display for ObjFunction {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let name = if self.name.is_empty() {
            "<script>".to_owned()
        } else {
            format!("<fn {}>", self.name.as_str())
        };
        write!(f, "{name}")
    }
}
