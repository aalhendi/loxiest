use std::{
    cell::RefCell,
    fmt::Display,
    mem,
    ops::{Deref, Neg},
    ptr::null_mut,
    rc::Rc,
};

use crate::{
    memory::reallocate,
    object2::{
        Obj2, ObjClass2, ObjClosure2, ObjFunction, ObjInstance2, ObjNative2, ObjString, ObjType,
    },
    FREE_ARRAY,
};
use crate::{
    object::{Obj, ObjClass, ObjClosure, ObjInstance},
    GROW_ARRAY, GROW_CAPACITY,
};

#[derive(Debug, Clone, Copy, PartialEq)]
enum ValueType {
    Bool,
    Nil,
    Number,
    Obj,
}

#[derive(Clone, Copy)]
union ValueUnion {
    boolean: bool,
    number: f64,
    obj: *mut Obj2,
}

#[derive(Clone, Copy)]
pub struct Value2 {
    type_: ValueType,
    as_: ValueUnion,
}

impl Display for Value2 {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self.type_ {
            ValueType::Bool => write!(f, "{}", self.as_bool()),
            ValueType::Nil => write!(f, "Nil"),
            ValueType::Number => write!(f, "{}", self.as_number()),
            ValueType::Obj => match self.obj_type() {
                ObjType::String => unsafe {
                    let str_ptr = self.as_cstring();
                    let mut i = 0;
                    loop {
                        if (*str_ptr.offset(i)) == b'\0' {
                            break Ok(());
                        }
                        write!(f, "{}", (*str_ptr.offset(i)) as char)?;
                        i += 1;
                    }
                },
                ObjType::Function => unsafe {
                    let name_ptr = (*self.as_function()).name;
                    if name_ptr.is_null() {
                        return write!(f, "<script>");
                    }

                    write!(f, "<fn ")?;
                    let str_ptr = (*name_ptr).chars;
                    let mut i = 0;
                    loop {
                        if (*str_ptr.offset(i)) == b'\0' {
                            break;
                        }
                        write!(f, "{}", (*str_ptr.offset(i)) as char)?;
                        i += 1;
                    }
                    write!(f, ">",)
                },
                ObjType::Native => write!(f, "<native fn>"),
                ObjType::Closure => {
                    // TODO(aalhendi): Refactor
                    unsafe {
                        let func = (*self.as_closure()).function;
                        let name_ptr = (*func).name;
                        if name_ptr.is_null() {
                            return write!(f, "<script>");
                        }

                        write!(f, "<fn ")?;
                        let str_ptr = (*name_ptr).chars;
                        let mut i = 0;
                        loop {
                            if (*str_ptr.offset(i)) == b'\0' {
                                break;
                            }
                            write!(f, "{}", (*str_ptr.offset(i)) as char)?;
                            i += 1;
                        }
                        write!(f, ">",)
                    }
                }
                // Upvalues exist only to take advantage VM’s memory management.
                // They aren’t first-class values that a Lox user can directly access in a program
                // Unreachable (?)
                ObjType::Upvalue => write!(f, "upvalue"),
                ObjType::Class => unsafe {
                    let str_ptr = (*self.as_class()).name;
                    let chars_ptr = (*str_ptr).chars;
                    for i in 0..(*str_ptr).length {
                        write!(f, "{}", (*chars_ptr.offset(i)) as char)?;
                    }
                    Ok(())
                },
                ObjType::Instance => unsafe {
                    let str_ptr = (*(*self.as_instance()).class).name;
                    let chars_ptr = (*str_ptr).chars;
                    for i in 0..(*str_ptr).length {
                        write!(f, "{}", (*chars_ptr.offset(i)) as char)?;
                    }
                    write!(f, "'s instance",)
                },
            },
        }
    }
}

impl Value2 {
    pub const fn bool_val(value: bool) -> Self {
        Self {
            type_: ValueType::Bool,
            as_: ValueUnion { boolean: value },
        }
    }

    pub const fn nil_val() -> Self {
        Self {
            type_: ValueType::Nil,
            as_: ValueUnion { number: 0.0 },
        }
    }

    pub const fn number_val(value: f64) -> Self {
        Self {
            type_: ValueType::Number,
            as_: ValueUnion { number: value },
        }
    }

    pub const fn obj_val<T>(object: *mut T) -> Self {
        Self {
            type_: ValueType::Obj,
            as_: ValueUnion {
                obj: object as *mut Obj2,
            },
        }
    }

    // PERF(aalhendi): does match/panic really add overhead? I didnt bother finding out...
    pub fn as_bool(&self) -> bool {
        // match self.type_ {
        //     ValueType::Bool => self.as_.boolean,
        //     _ => panic!("Value is not a boolean"),
        // }
        unsafe { self.as_.boolean }
    }

    pub fn as_number(&self) -> f64 {
        unsafe { self.as_.number }
    }

    pub fn as_obj(&self) -> *mut Obj2 {
        unsafe { self.as_.obj }
    }

    pub fn as_string(&self) -> *mut ObjString {
        debug_assert!(self.type_ == ValueType::Obj);
        unsafe {
            debug_assert!((*self.as_obj()).obj_type() == ObjType::String);
            std::mem::transmute(self.as_obj())
        }
    }

    // TODO(aalhendi): rename to as_native_string?
    pub fn as_cstring(&self) -> *mut u8 {
        unsafe { (*self.as_string()).chars }
    }

    pub fn as_function(&self) -> *mut ObjFunction {
        debug_assert!(self.type_ == ValueType::Obj);
        unsafe {
            debug_assert!((*self.as_obj()).obj_type() == ObjType::Function);
            std::mem::transmute(self.as_obj())
        }
    }

    pub fn as_native(&self) -> fn(arg_count: usize, args: &[Value2]) -> Value2 {
        debug_assert!(self.type_ == ValueType::Obj);
        unsafe {
            debug_assert!((*self.as_obj()).obj_type() == ObjType::Native);
            (*std::mem::transmute::<*mut Obj2, *mut ObjNative2>(self.as_obj())).function
        }
    }

    pub fn as_closure(&self) -> *mut ObjClosure2 {
        debug_assert!(self.type_ == ValueType::Obj);
        unsafe {
            debug_assert!((*self.as_obj()).obj_type() == ObjType::Closure);
            std::mem::transmute(self.as_obj())
        }
    }

    pub fn as_class(&self) -> *mut ObjClass2 {
        debug_assert!(self.type_ == ValueType::Obj);
        unsafe {
            debug_assert!((*self.as_obj()).obj_type() == ObjType::Class);
            std::mem::transmute(self.as_obj())
        }
    }

    pub fn as_instance(&self) -> *mut ObjInstance2 {
        debug_assert!(self.type_ == ValueType::Obj);
        unsafe {
            debug_assert!((*self.as_obj()).obj_type() == ObjType::Instance);
            std::mem::transmute(self.as_obj())
        }
    }

    pub fn is_bool(&self) -> bool {
        self.type_ == ValueType::Bool
    }

    pub fn is_nil(&self) -> bool {
        self.type_ == ValueType::Nil
    }

    pub fn is_number(&self) -> bool {
        self.type_ == ValueType::Number
    }

    pub fn is_obj(&self) -> bool {
        self.type_ == ValueType::Obj
    }

    pub fn is_string(&self) -> bool {
        self.is_obj_type(ObjType::String)
    }

    pub fn obj_type(&self) -> ObjType {
        debug_assert!(self.type_ == ValueType::Obj);
        unsafe { (*self.as_obj()).obj_type() }
    }

    pub fn is_obj_type(&self, type_: ObjType) -> bool {
        self.is_obj() && unsafe { (*self.as_obj()).type_ == type_ }
    }

    // NOTE(aalhendi): Lox follows ruby in that only false and nil are false in lox
    pub fn is_falsey(&self) -> bool {
        self.is_nil() || (self.is_bool() && !self.as_bool())
    }

    // TODO(aalhendi): probably revise this
    pub fn equal(a: Value2, b: Value2) -> bool {
        if a.type_ != b.type_ {
            return false;
        }
        match a.type_ {
            ValueType::Bool => a.as_bool() == b.as_bool(),
            ValueType::Nil => true,
            ValueType::Number => a.as_number() == b.as_number(),
            ValueType::Obj => a.as_obj() == b.as_obj(),
        }
    }
}

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
