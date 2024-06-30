use std::fmt::Display;
use std::time::SystemTime;

use crate::chunk::Chunk;
use crate::memory::reallocate;
use crate::table::Table;
use crate::value::Value;
use crate::{ALLOCATE, FREE_ARRAY, VM};

macro_rules! ALLOCATE_OBJ {
    ($type_:ty, $object_type:expr) => {
        allocate_object(std::mem::size_of::<$type_>(), $object_type) as *mut $type_
    };
}

fn allocate_object(size: usize, type_: ObjType) -> *mut Obj {
    let object = reallocate(std::ptr::null_mut(), 0, size) as *mut Obj;
    unsafe {
        (*object).type_ = type_;
        (*object).is_marked = false;
        (*object).next = VM.objects;
        VM.objects = object;
        #[cfg(feature = "debug-log-gc")]
        {
            println!("{object:p} allocate {size} for {type_:?}");
        }
    }

    object
}

#[derive(Debug, Clone, Copy, PartialEq)]
pub enum ObjType {
    String,
    Function,
    Native,
    Closure,
    Upvalue,
    Class,
    Instance,
    BoundMethod,
}

#[repr(C)]
pub struct Obj {
    pub type_: ObjType,
    pub is_marked: bool,
    pub next: *mut Obj,
}

impl Obj {
    pub fn obj_type(&self) -> ObjType {
        self.type_
    }

    pub fn copy_string(chars: &[u8], length: usize) -> *mut ObjString {
        let hash = hash_string(chars.as_ptr(), length as isize);
        unsafe {
            let interned = VM
                .strings
                .find_string(chars.as_ptr() as *mut u8, length, hash);
            if !interned.is_null() {
                return interned;
            }
        }

        let heap_chars = ALLOCATE!(u8, length);
        unsafe {
            // PERF(aalhendi): ghetto memcpy, not sure about perf
            for (idx, c) in chars.iter().enumerate() {
                (*heap_chars.wrapping_add(idx)) = *c;
            }
        }

        ObjString::allocate_string(heap_chars, length, hash)
    }
}

#[repr(C)]
pub struct ObjString {
    // Given an ObjString*, you can safely cast it to Obj* and then access the type field from it.
    // Given an Obj*, you can “downcast” it to an ObjString*. MUST ensure Obj* ptr points to obj field an actual ObjString
    pub obj: Obj,
    pub length: isize,
    pub chars: *mut u8,
    pub hash: u32,
}

impl Display for ObjString {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        for i in 0..self.length {
            unsafe { write!(f, "{}", (*self.chars.offset(i)) as char)? }
        }
        Ok(())
    }
}

impl ObjString {
    pub fn allocate_string(chars: *mut u8, length: usize, hash: u32) -> *mut Self {
        let string = ALLOCATE_OBJ!(ObjString, ObjType::String);
        unsafe {
            (*string).length = length as isize;
            (*string).chars = chars;
            (*string).hash = hash;
            VM.push(Value::obj_val(string));
            VM.strings.set(string, Value::nil_val());
            VM.pop();
        }
        string
    }

    pub fn take_string(chars: *mut u8, length: usize) -> *mut Self {
        let hash = hash_string(chars as *const u8, length as isize);
        unsafe {
            let interned = VM.strings.find_string(chars, length, hash);
            if !interned.is_null() {
                FREE_ARRAY!(u8, chars, length + 1); // TODO(aalhendi): verify +1
                return interned;
            }
        }

        ObjString::allocate_string(chars, length, hash)
    }
}

#[repr(C)]
pub struct ObjFunction {
    obj: Obj,
    pub arity: isize,
    pub upvalue_count: isize,
    pub chunk: Chunk,
    pub name: *mut ObjString,
}

impl ObjFunction {
    pub fn new() -> *mut Self {
        let function = ALLOCATE_OBJ!(ObjFunction, ObjType::Function);
        unsafe {
            (*function).arity = 0;
            (*function).upvalue_count = 0;
            (*function).name = std::ptr::null_mut();
            (*function).chunk.init();
        }
        function
    }
}

impl Display for ObjFunction {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        unsafe {
            let name_ptr = self.name;
            if name_ptr.is_null() {
                return write!(f, "<script>");
            }

            write!(f, "<fn {}>", *name_ptr)
        }
    }
}

pub fn native_clock2(_arg_count: usize, _args: &[Value]) -> Value {
    match SystemTime::now().duration_since(SystemTime::UNIX_EPOCH) {
        Ok(n) => Value::number_val(n.as_secs_f64()),
        Err(e) => panic!("{e}"),
    }
}

pub type NativeFn2 = fn(arg_count: usize, args: &[Value]) -> Value;

#[repr(C)]
pub struct ObjNative2 {
    obj: Obj,
    pub function: NativeFn2,
}

impl ObjNative2 {
    pub fn new(function: NativeFn2) -> *mut Self {
        let native = ALLOCATE_OBJ!(ObjNative2, ObjType::Native);
        unsafe {
            (*native).function = function;
        }

        native
    }
}

#[repr(C)]
pub struct ObjClosure2 {
    obj: Obj,
    pub function: *mut ObjFunction,
    pub upvalues: *mut *mut ObjUpvalue2,
    pub upvalue_count: isize,
}

impl ObjClosure2 {
    pub fn new(function: *mut ObjFunction) -> *mut Self {
        unsafe {
            let upvalues = ALLOCATE!(*mut ObjUpvalue2, (*function).upvalue_count as usize);
            for i in 0..(*function).upvalue_count {
                *upvalues.offset(i) = std::ptr::null_mut();
            }

            let closure = ALLOCATE_OBJ!(ObjClosure2, ObjType::Closure);
            (*closure).function = function;
            (*closure).upvalues = upvalues;
            (*closure).upvalue_count = (*function).upvalue_count;
            closure
        }
    }
}

#[repr(C)]
pub struct ObjUpvalue2 {
    obj: Obj,
    pub location: *mut Value,
    pub closed: Value,
    pub next: *mut ObjUpvalue2,
}

impl ObjUpvalue2 {
    pub fn new(slot: *mut Value) -> *mut Self {
        let upvalue = ALLOCATE_OBJ!(ObjUpvalue2, ObjType::Upvalue);
        unsafe {
            (*upvalue).closed = Value::nil_val();
            (*upvalue).location = slot;
            (*upvalue).next = std::ptr::null_mut();
        }

        upvalue
    }
}

#[repr(C)]
pub struct ObjClass2 {
    obj: Obj,
    pub name: *mut ObjString,
    pub methods: Table,
}

impl ObjClass2 {
    pub fn new(name: *mut ObjString) -> *mut Self {
        let class = ALLOCATE_OBJ!(ObjClass2, ObjType::Class);
        unsafe {
            (*class).name = name;
            (*class).methods.init();
        }

        class
    }
}

#[repr(C)]
pub struct ObjInstance2 {
    obj: Obj,
    pub class: *mut ObjClass2,
    pub fields: Table,
}

impl ObjInstance2 {
    pub fn new(class: *mut ObjClass2) -> *mut Self {
        let instance = ALLOCATE_OBJ!(ObjInstance2, ObjType::Instance);
        unsafe {
            (*instance).class = class;
        }

        instance
    }
}

#[repr(C)]
pub struct ObjBoundMethod2 {
    obj: Obj,
    pub reciever: Value,
    pub method: *mut ObjClosure2,
}

impl ObjBoundMethod2 {
    pub fn new(receiver: Value, method: *mut ObjClosure2) -> *mut Self {
        let bound = ALLOCATE_OBJ!(ObjBoundMethod2, ObjType::BoundMethod);
        unsafe {
            (*bound).reciever = receiver;
            (*bound).method = method;
        }

        bound
    }
}

/// FNV-1a
fn hash_string(key: *const u8, length: isize) -> u32 {
    let mut hash = 2166136261u32;
    for i in 0..length {
        unsafe {
            hash ^= (*key.offset(i)) as u32;
            hash = hash.overflowing_mul(16777619).0;
        }
    }
    hash
}
