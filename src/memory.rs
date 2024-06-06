use std::{
    alloc::{dealloc, realloc, Layout},
    ptr::null_mut,
};

use crate::{
    object2::{Obj2, ObjClosure2, ObjFunction, ObjNative2, ObjString, ObjType, ObjUpvalue2},
    VM,
};

pub fn reallocate(
    pointer: *mut std::ffi::c_void,
    old_size: usize,
    new_size: usize,
) -> *mut std::ffi::c_void {
    if new_size == 0 {
        // TODO(aalhendi): Is this null check needed?
        if !pointer.is_null() {
            unsafe {
                dealloc(pointer as *mut u8, Layout::array::<u8>(old_size).unwrap());
            }
        }
        std::ptr::null_mut()
    } else {
        let old_layout = Layout::array::<u8>(old_size).unwrap();
        // let new_layout = Layout::array::<u8>(new_size).unwrap();
        // let new_ptr = unsafe { realloc(pointer as *mut u8, old_layout, new_layout.size()) };
        let new_ptr = unsafe { realloc(pointer as *mut u8, old_layout, new_size) };
        if new_ptr.is_null() {
            // eprintln!("Failed to reallocate memory");
            std::process::exit(1);
        }
        new_ptr as *mut std::ffi::c_void
    }
}

// macros
#[macro_export]
macro_rules! GROW_CAPACITY {
    ($capacity:expr) => {
        if $capacity < 8 {
            8
        } else {
            $capacity * 2
        }
    };
}

/// Pass counts as usize
#[macro_export]
macro_rules! GROW_ARRAY {
    ($type:ty, $pointer:expr, $old_count:expr, $new_count:expr) => {{
        let ptr = reallocate(
            $pointer as *mut std::ffi::c_void,
            $old_count * mem::size_of::<$type>(),
            $new_count * mem::size_of::<$type>(),
        ) as *mut $type;
        // TODO(aalhendi): Is the zeroing needed?
        /*
        if $new_count > $old_count {
            let new_slice =
                std::slice::from_raw_parts_mut(ptr.add($old_count), $new_count - $old_count);
            for elem in new_slice.iter_mut() {
                *elem = mem::zeroed();
            }
        }
        */
        ptr
    }};
}

#[macro_export]
macro_rules! FREE_ARRAY {
    ($type:ty, $pointer:expr, $old_count:expr) => {
        reallocate(
            $pointer as *mut std::ffi::c_void,
            $old_count * std::mem::size_of::<$type>(),
            0,
        ) as *mut $type;
    };
}

#[macro_export]
macro_rules! ALLOCATE {
    ($type:ty, $count:expr) => {
        reallocate(
            std::ptr::null_mut(),
            0,
            std::mem::size_of::<$type>() * $count,
        ) as *mut $type
    };
}

macro_rules! FREE {
    ($type:ty, $ptr:expr) => {
        reallocate(
            $ptr as *mut std::ffi::c_void,
            std::mem::size_of::<$type>(),
            0,
        )
    };
}

fn free_object(object: *mut Obj2) {
    unsafe {
        match (*object).type_ {
            ObjType::String => {
                let string = object as *mut ObjString;
                FREE_ARRAY!(u8, (*string).chars, (*string).length as usize + 1);
                FREE!(ObjString, object);
            }
            ObjType::Function => {
                let function = object as *mut ObjFunction;
                (*function).chunk.free();
                FREE!(ObjFunction, object);
            }
            ObjType::Native => {
                FREE!(ObjNative2, object);
            }
            ObjType::Closure => {
                let closure = object as *mut ObjClosure2;
                FREE_ARRAY!(
                    *mut ObjUpvalue2,
                    (*closure).upvalues,
                    (*closure).upvalue_count as usize
                );
                FREE!(ObjClosure2, object);
            }
            ObjType::Upvalue => {
                FREE!(ObjUpvalue2, object);
            }
        }
    }
}

pub fn free_objects() {
    unsafe {
        let mut object = VM.objects;
        while !object.is_null() {
            let next = (*object).next;
            free_object(object);
            object = next;
        }
    }
}
