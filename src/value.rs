use std::{fmt::Display, mem, ptr::null_mut};

use crate::{
    memory::reallocate,
    object::{
        Obj, ObjBoundMethod2, ObjClass2, ObjClosure, ObjFunction, ObjInstance2, ObjNative,
        ObjString, ObjType,
    },
    FREE_ARRAY,
};
use crate::{GROW_ARRAY, GROW_CAPACITY};
#[cfg(feature = "nan-boxing")]
use std::hint::unreachable_unchecked;

#[cfg(not(feature = "nan-boxing"))]
#[derive(Debug, Clone, Copy, PartialEq)]
enum ValueType {
    Bool,
    Nil,
    Number,
    Obj,
}

#[cfg(not(feature = "nan-boxing"))]
#[derive(Clone, Copy)]
union ValueUnion {
    boolean: bool,
    number: f64,
    obj: *mut Obj,
}

#[cfg(not(feature = "nan-boxing"))]
#[derive(Clone, Copy)]
pub struct Value {
    type_: ValueType,
    as_: ValueUnion,
}

#[cfg(feature = "nan-boxing")]
#[repr(transparent)]
#[derive(Clone, Copy)]
pub struct Value(pub u64);

#[cfg(feature = "nan-boxing")]
const SIGN_BIT: u64 = 0b1000000000000000000000000000000000000000000000000000000000000000;
#[cfg(feature = "nan-boxing")]
// 0x7ffc000000000000 - all exponent bits, QNAN bit and one more to avoid Intel magic value
const QNAN: u64 = 0b0111111111111100000000000000000000000000000000000000000000000000;
#[cfg(feature = "nan-boxing")]
const TAG_NIL: u64 = 0b01;
#[cfg(feature = "nan-boxing")]
const TAG_FALSE: u64 = 0b10;
#[cfg(feature = "nan-boxing")]
const TAG_TRUE: u64 = 0b11;
#[cfg(feature = "nan-boxing")]
pub const FALSE_VAL: Value = Value(QNAN | TAG_FALSE);
#[cfg(feature = "nan-boxing")]
pub const TRUE_VAL: Value = Value(QNAN | TAG_TRUE);
#[cfg(feature = "nan-boxing")]
pub const NIL_VAL: Value = Value(QNAN | TAG_NIL);

impl Display for Value {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        #[cfg(feature = "nan-boxing")]
        {
            if self.is_bool() {
                write!(f, "{}", self.as_bool())
            } else if self.is_nil() {
                write!(f, "nil")
            } else if self.is_number() {
                write!(f, "{}", self.as_number())
            } else if self.is_obj() {
                match self.obj_type() {
                    ObjType::String => {
                        let str_ptr = self.as_string();
                        unsafe { write!(f, "{}", *str_ptr) }
                    }
                    ObjType::Function => {
                        let fn_ptr = self.as_function();
                        unsafe { write!(f, "{}", *fn_ptr) }
                    }
                    ObjType::Native => write!(f, "<native fn>"),
                    ObjType::Closure => unsafe {
                        let fn_ptr = (*self.as_closure()).function;
                        write!(f, "{}", *fn_ptr)
                    },
                    // Upvalues exist only to take advantage VM’s memory management.
                    // They aren’t first-class values that a Lox user can directly access in a program
                    // ObjType::Upvalue => write!(f, "upvalue"),
                    ObjType::Upvalue => unsafe { unreachable_unchecked() },
                    ObjType::Class => unsafe {
                        let str_ptr = (*self.as_class()).name;
                        write!(f, "{}", *str_ptr)
                    },
                    ObjType::Instance => unsafe {
                        let str_ptr = (*(*self.as_instance()).class).name;
                        write!(f, "{} instance", *str_ptr)
                    },
                    ObjType::BoundMethod => unsafe {
                        let fn_ptr = (*(*self.as_bound_method()).method).function;
                        write!(f, "{}", *fn_ptr)
                    },
                }
            } else {
                unsafe { unreachable_unchecked() }
            }
        }

        #[cfg(not(feature = "nan-boxing"))]
        {
            match self.type_ {
                ValueType::Bool => write!(f, "{}", self.as_bool()),
                ValueType::Nil => write!(f, "nil"),
                ValueType::Number => write!(f, "{}", self.as_number()),
                ValueType::Obj => match self.obj_type() {
                    ObjType::String => {
                        let str_ptr = self.as_string();
                        unsafe { write!(f, "{}", *str_ptr) }
                    }
                    ObjType::Function => {
                        let fn_ptr = self.as_function();
                        unsafe { write!(f, "{}", *fn_ptr) }
                    }
                    ObjType::Native => write!(f, "<native fn>"),
                    ObjType::Closure => unsafe {
                        let fn_ptr = (*self.as_closure()).function;
                        write!(f, "{}", *fn_ptr)
                    },
                    // Upvalues exist only to take advantage VM’s memory management.
                    // They aren’t first-class values that a Lox user can directly access in a program
                    // Unreachable (?)
                    ObjType::Upvalue => write!(f, "upvalue"),
                    ObjType::Class => unsafe {
                        let str_ptr = (*self.as_class()).name;
                        write!(f, "{}", *str_ptr)
                    },
                    ObjType::Instance => unsafe {
                        let str_ptr = (*(*self.as_instance()).class).name;
                        write!(f, "{} instance", *str_ptr)
                    },
                    ObjType::BoundMethod => unsafe {
                        let fn_ptr = (*(*self.as_bound_method()).method).function;
                        write!(f, "{}", *fn_ptr)
                    },
                },
            }
        }
    }
}

impl Value {
    pub const fn bool_val(value: bool) -> Self {
        #[cfg(feature = "nan-boxing")]
        {
            if value {
                TRUE_VAL
            } else {
                FALSE_VAL
            }
        }
        #[cfg(not(feature = "nan-boxing"))]
        {
            Self {
                type_: ValueType::Bool,
                as_: ValueUnion { boolean: value },
            }
        }
    }

    pub const fn nil_val() -> Self {
        #[cfg(feature = "nan-boxing")]
        {
            NIL_VAL
        }
        #[cfg(not(feature = "nan-boxing"))]
        {
            Self {
                type_: ValueType::Nil,
                as_: ValueUnion { number: 0.0 },
            }
        }
    }

    pub const fn number_val(value: f64) -> Self {
        #[cfg(feature = "nan-boxing")]
        // Self(value.to_bits()) // TODO(aalhendi): unstable as const fn
        unsafe {
            Self(std::mem::transmute::<f64, u64>(value))
        }
        #[cfg(not(feature = "nan-boxing"))]
        Self {
            type_: ValueType::Number,
            as_: ValueUnion { number: value },
        }
    }

    pub const fn obj_val<T>(object: *mut T) -> Self {
        #[cfg(feature = "nan-boxing")]
        {
            // SAFETY: transmuting a ptr to u64, relies on ptr being 64-bit and is unsafe.
            #[allow(clippy::transmutes_expressible_as_ptr_casts)]
            // NOTE(aalhendi): Workaround to avoid error "pointers cannot be cast to integers during const eval"
            unsafe {
                Value(SIGN_BIT | QNAN | std::mem::transmute::<*mut T, u64>(object))
            }
        }

        #[cfg(not(feature = "nan-boxing"))]
        {
            Self {
                type_: ValueType::Obj,
                as_: ValueUnion {
                    obj: object as *mut Obj,
                },
            }
        }
    }

    // PERF(aalhendi): does match/panic really add overhead? I didnt bother finding out...
    pub fn as_bool(&self) -> bool {
        #[cfg(feature = "nan-boxing")]
        {
            self == &TRUE_VAL
        }

        #[cfg(not(feature = "nan-boxing"))]
        {
            // match self.type_ {
            //     ValueType::Bool => self.as_.boolean,
            //     _ => panic!("Value is not a boolean"),
            // }
            unsafe { self.as_.boolean }
        }
    }

    pub fn as_number(&self) -> f64 {
        #[cfg(feature = "nan-boxing")]
        {
            f64::from_bits(self.0)
        }
        #[cfg(not(feature = "nan-boxing"))]
        unsafe {
            self.as_.number
        }
    }

    pub fn as_obj(&self) -> *mut Obj {
        #[cfg(feature = "nan-boxing")]
        {
            (self.0 & !(SIGN_BIT | QNAN)) as *mut Obj
        }

        #[cfg(not(feature = "nan-boxing"))]
        unsafe {
            self.as_.obj
        }
    }

    pub fn as_string(&self) -> *mut ObjString {
        #[cfg(not(feature = "nan-boxing"))]
        {
            debug_assert!(self.type_ == ValueType::Obj);
        }
        unsafe {
            debug_assert!((*self.as_obj()).obj_type() == ObjType::String);
            std::mem::transmute(self.as_obj())
        }
    }

    pub fn as_function(&self) -> *mut ObjFunction {
        #[cfg(not(feature = "nan-boxing"))]
        {
            debug_assert!(self.type_ == ValueType::Obj);
        }
        unsafe {
            debug_assert!((*self.as_obj()).obj_type() == ObjType::Function);
            std::mem::transmute(self.as_obj())
        }
    }

    pub fn as_native(&self) -> fn(arg_count: usize, args: &[Value]) -> Value {
        #[cfg(not(feature = "nan-boxing"))]
        {
            debug_assert!(self.type_ == ValueType::Obj);
        }
        unsafe {
            debug_assert!((*self.as_obj()).obj_type() == ObjType::Native);
            (*std::mem::transmute::<*mut Obj, *mut ObjNative>(self.as_obj())).function
        }
    }

    pub fn as_closure(&self) -> *mut ObjClosure {
        #[cfg(not(feature = "nan-boxing"))]
        {
            debug_assert!(self.type_ == ValueType::Obj);
        }
        unsafe {
            debug_assert!((*self.as_obj()).obj_type() == ObjType::Closure);
            std::mem::transmute(self.as_obj())
        }
    }

    pub fn as_class(&self) -> *mut ObjClass2 {
        #[cfg(not(feature = "nan-boxing"))]
        {
            debug_assert!(self.type_ == ValueType::Obj);
        }
        unsafe {
            debug_assert!((*self.as_obj()).obj_type() == ObjType::Class);
            std::mem::transmute(self.as_obj())
        }
    }

    pub fn as_instance(&self) -> *mut ObjInstance2 {
        #[cfg(not(feature = "nan-boxing"))]
        {
            debug_assert!(self.type_ == ValueType::Obj);
        }
        unsafe {
            debug_assert!((*self.as_obj()).obj_type() == ObjType::Instance);
            std::mem::transmute(self.as_obj())
        }
    }

    pub fn as_bound_method(&self) -> *mut ObjBoundMethod2 {
        #[cfg(not(feature = "nan-boxing"))]
        {
            debug_assert!(self.type_ == ValueType::Obj);
        }
        unsafe {
            debug_assert!((*self.as_obj()).obj_type() == ObjType::BoundMethod);
            std::mem::transmute(self.as_obj())
        }
    }

    pub fn is_bool(&self) -> bool {
        #[cfg(feature = "nan-boxing")]
        {
            (self.0 | 1) == TRUE_VAL.0
            // self == &TRUE_VAL || self == &FALSE_VAL
        }
        #[cfg(not(feature = "nan-boxing"))]
        {
            self.type_ == ValueType::Bool
        }
    }

    pub fn is_nil(&self) -> bool {
        #[cfg(feature = "nan-boxing")]
        {
            self == &NIL_VAL
        }
        #[cfg(not(feature = "nan-boxing"))]
        {
            self.type_ == ValueType::Nil
        }
    }

    pub fn is_number(&self) -> bool {
        #[cfg(feature = "nan-boxing")]
        {
            (self.0 & QNAN) != QNAN
        }
        #[cfg(not(feature = "nan-boxing"))]
        {
            self.type_ == ValueType::Number
        }
    }

    pub fn is_obj(&self) -> bool {
        #[cfg(feature = "nan-boxing")]
        {
            self.0 & (QNAN | SIGN_BIT) == QNAN | SIGN_BIT
        }

        #[cfg(not(feature = "nan-boxing"))]
        {
            self.type_ == ValueType::Obj
        }
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
        #[cfg(not(feature = "nan-boxing"))]
        {
            debug_assert!(self.type_ == ValueType::Obj);
        }
        unsafe { (*self.as_obj()).obj_type() }
    }

    pub fn is_obj_type(&self, type_: ObjType) -> bool {
        self.is_obj() && unsafe { (*self.as_obj()).type_ == type_ }
    }

    // NOTE(aalhendi): Lox follows ruby in that only false and nil are false in lox
    pub fn is_falsey(&self) -> bool {
        self.is_nil() || (self.is_bool() && !self.as_bool())
    }
}

impl PartialEq for Value {
    #[cfg(feature = "nan-boxing")]
    fn eq(&self, other: &Self) -> bool {
        // NOTE(aalhendi): IEEE specs lists NaN != NaN
        // this ensures “real” arithmetic NaNs produced in lox are not equal to themselves
        if self.is_number() && other.is_number() {
            return self.as_number() == other.as_number();
        }
        self.0 == other.0
    }

    #[cfg(not(feature = "nan-boxing"))]
    fn eq(&self, other: &Self) -> bool {
        if self.type_ != other.type_ {
            return false;
        }
        match self.type_ {
            ValueType::Bool => self.as_bool() == other.as_bool(),
            ValueType::Nil => true,
            ValueType::Number => self.as_number() == other.as_number(),
            ValueType::Obj => self.as_obj() == other.as_obj(),
        }
    }
}

pub struct ValueArray {
    pub capacity: usize,
    pub count: usize,
    pub values: *mut Value,
}

impl Default for ValueArray {
    fn default() -> Self {
        Self {
            capacity: 0,
            count: 0,
            values: null_mut(),
        }
    }
}

impl ValueArray {
    pub fn write(&mut self, value: Value) {
        if self.capacity < self.count + 1 {
            let old_capacity = self.capacity;
            self.capacity = GROW_CAPACITY!(old_capacity);
            self.values = GROW_ARRAY!(Value, self.values, old_capacity, self.capacity);
        }

        unsafe { *self.values.wrapping_add(self.count) = value };
        self.count += 1;
    }

    pub fn free(&mut self) {
        FREE_ARRAY!(Value, self.values, self.capacity);
        *self = Self::default()
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
