use crate::memory::reallocate;
use crate::{ALLOCATE, VM};

macro_rules! ALLOCATE_OBJ {
    ($type_:ty, $object_type:expr) => {
        allocate_object(std::mem::size_of::<$type_>(), $object_type) as *mut $type_
    };
}

fn allocate_object(size: usize, type_: ObjType) -> *mut Obj2 {
    let object = reallocate(std::ptr::null_mut(), 0, size) as *mut Obj2;
    unsafe {
        (*object).type_ = type_;
        (*object).next = VM.objects;
        VM.objects = object;
    }
    object
}

#[derive(Debug, Clone, Copy, PartialEq)]
pub enum ObjType {
    String,
}

#[repr(C)]
pub struct Obj2 {
    pub type_: ObjType,
    pub next: *mut Obj2,
}

impl Obj2 {
    pub fn obj_type(&self) -> ObjType {
        self.type_
    }

    pub fn copy_string(chars: &[u8], length: usize) -> *mut ObjString {
        let heap_chars = ALLOCATE!(char, length + 1);
        unsafe {
            // PERF(aalhendi): ghetto memcpy, not sure about perf
            for (idx, c) in chars.iter().enumerate() {
                (*heap_chars.wrapping_add(idx)) = *c as char;
            }
            (*heap_chars.wrapping_add(length + 1)) = '\0'; // null terminator
        }

        ObjString::allocate_string(heap_chars, length)
    }
}

#[repr(C)]
pub struct ObjString {
    // Given an ObjString*, you can safely cast it to Obj* and then access the type field from it.
    // Given an Obj*, you can “downcast” it to an ObjString*. MUST ensure Obj* ptr points to obj field an actual ObjString
    obj: Obj2,
    pub length: isize,
    pub chars: *mut char,
}

impl ObjString {
    pub fn allocate_string(chars: *mut char, length: usize) -> *mut Self {
        let string = ALLOCATE_OBJ!(ObjString, ObjType::String);
        unsafe {
            (*string).length = length as isize;
            (*string).chars = chars;
        }
        string
    }

    pub fn take_string(chars: *mut char, length: usize) -> *mut Self {
        ObjString::allocate_string(chars, length)
    }
}
