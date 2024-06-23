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
    object::{
        Obj, ObjBoundMethod2, ObjClass2, ObjClosure2, ObjFunction, ObjInstance2, ObjNative2,
        ObjString, ObjType,
    },
    FREE_ARRAY,
};
use crate::{GROW_ARRAY, GROW_CAPACITY};

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
    obj: *mut Obj,
}

#[derive(Clone, Copy)]
pub struct Value {
    type_: ValueType,
    as_: ValueUnion,
}

impl Display for Value {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self.type_ {
            ValueType::Bool => write!(f, "{}", self.as_bool()),
            ValueType::Nil => write!(f, "nil"),
            ValueType::Number => write!(f, "{}", self.as_number()),
            ValueType::Obj => match self.obj_type() {
                ObjType::String => unsafe {
                    let str_ptr = self.as_string();
                    let chars_ptr = (*str_ptr).chars;
                    for i in 0..(*str_ptr).length {
                        write!(f, "{}", (*chars_ptr.offset(i)) as char)?;
                    }
                    Ok(())
                },
                ObjType::Function => unsafe {
                    let name_ptr = (*self.as_function()).name;
                    if name_ptr.is_null() {
                        return write!(f, "<script>");
                    }

                    write!(f, "<fn ")?;
                    let chars_ptr = (*name_ptr).chars;
                    for i in 0..(*name_ptr).length {
                        write!(f, "{}", (*chars_ptr.offset(i)) as char)?;
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
                        let chars_ptr = (*name_ptr).chars;
                        for i in 0..(*name_ptr).length {
                            write!(f, "{}", (*chars_ptr.offset(i)) as char)?;
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
                    write!(f, " instance",)
                },
                ObjType::BoundMethod => unsafe {
                    let func = (*(*self.as_bound_method()).method).function;
                    let name_ptr = (*func).name;
                    if name_ptr.is_null() {
                        return write!(f, "<script>");
                    }

                    write!(f, "<fn ")?;
                    let chars_ptr = (*name_ptr).chars;
                    for i in 0..(*name_ptr).length {
                        write!(f, "{}", (*chars_ptr.offset(i)) as char)?;
                    }
                    write!(f, ">",)
                },
            },
        }
    }
}

impl Value {
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
                obj: object as *mut Obj,
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

    pub fn as_obj(&self) -> *mut Obj {
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

    pub fn as_native(&self) -> fn(arg_count: usize, args: &[Value]) -> Value {
        debug_assert!(self.type_ == ValueType::Obj);
        unsafe {
            debug_assert!((*self.as_obj()).obj_type() == ObjType::Native);
            (*std::mem::transmute::<*mut Obj, *mut ObjNative2>(self.as_obj())).function
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

    pub fn as_bound_method(&self) -> *mut ObjBoundMethod2 {
        debug_assert!(self.type_ == ValueType::Obj);
        unsafe {
            debug_assert!((*self.as_obj()).obj_type() == ObjType::BoundMethod);
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

    pub fn is_instance(&self) -> bool {
        self.is_obj_type(ObjType::Instance)
    }

    pub fn is_class(&self) -> bool {
        self.is_obj_type(ObjType::Class)
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
    pub fn equal(a: Value, b: Value) -> bool {
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

pub struct ValueArray {
    pub capacity: isize,
    pub count: isize,
    pub values: *mut Value,
}

impl ValueArray {
    pub fn init(&mut self) {
        self.values = null_mut();
        self.capacity = 0;
        self.count = 0;
    }

    pub fn write(&mut self, value: Value) {
        if self.capacity < self.count + 1 {
            let old_capacity = self.capacity;
            self.capacity = GROW_CAPACITY!(old_capacity);
            self.values = GROW_ARRAY!(
                Value,
                self.values,
                old_capacity as usize,
                self.capacity as usize
            );
        }

        unsafe { *self.values.offset(self.count) = value };
        self.count += 1;
    }

    pub fn free(&mut self) {
        FREE_ARRAY!(Value, self.values, self.capacity as usize);
        self.init();
    }

    #[cfg(any(feature = "debug-trace-execution", feature = "debug-print-code"))]
    pub fn print_value(&self, value: Value, terminator: Option<char>) {
        if let Some(t) = terminator {
            println!("{value}{t}")
        } else {
            println!("{value}")
        }
    }
}
