use crate::memory::reallocate;
use crate::value::Value2;
use crate::{ALLOCATE, FREE_ARRAY, VM};

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
        let hash = hash_string(chars.as_ptr(), length as isize);
        unsafe {
            let interned = VM
                .strings
                .find_string(chars.as_ptr() as *mut u8, length, hash);
            if !interned.is_null() {
                return interned;
            }
        }

        let heap_chars = ALLOCATE!(u8, length + 1);
        unsafe {
            // PERF(aalhendi): ghetto memcpy, not sure about perf
            for (idx, c) in chars.iter().enumerate() {
                (*heap_chars.wrapping_add(idx)) = *c;
            }
            (*heap_chars.wrapping_add(length + 1)) = b'\0'; // null terminator
        }

        ObjString::allocate_string(heap_chars, length, hash)
    }
}

#[repr(C)]
pub struct ObjString {
    // Given an ObjString*, you can safely cast it to Obj* and then access the type field from it.
    // Given an Obj*, you can “downcast” it to an ObjString*. MUST ensure Obj* ptr points to obj field an actual ObjString
    obj: Obj2,
    pub length: isize,
    pub chars: *mut u8,
    pub hash: u32,
}

impl ObjString {
    pub fn allocate_string(chars: *mut u8, length: usize, hash: u32) -> *mut Self {
        let string = ALLOCATE_OBJ!(ObjString, ObjType::String);
        unsafe {
            (*string).length = length as isize;
            (*string).chars = chars;
            (*string).hash = hash;
            VM.strings.set(string, Value2::nil_val());
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

/// FNV-1a
// fn hash_string(key: *const u8, length: usize) -> u32 {
//     let mut hash = 2166136261_u32;
//     for i in 0..length {
//         unsafe {
//             hash ^= (*key.wrapping_add(i)) as u32;
//             hash.wrapping_mul(16777619);
//         }
//     }
//     hash
// }
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
