use std::alloc::{dealloc, realloc, Layout};

#[cfg(target_os = "windows")]
use std::alloc::alloc;

use crate::{
    compiler::mark_compiler_roots,
    object::{
        Obj, ObjBoundMethod2, ObjClass2, ObjClosure, ObjFunction, ObjInstance2, ObjNative,
        ObjString, ObjType, ObjUpvalue,
    },
    value::{Value, ValueArray},
    VM,
};

const GC_HEAP_GROWTH_FACTOR: usize = 2;

pub fn reallocate(
    pointer: *mut std::ffi::c_void,
    old_size: usize,
    new_size: usize,
) -> *mut std::ffi::c_void {
    unsafe {
        VM.bytes_allocated = VM
            .bytes_allocated
            .wrapping_add(new_size.wrapping_sub(old_size));
    }

    if new_size > old_size {
        #[cfg(feature = "debug-stress-gc")]
        collect_garbage();
    }

    if new_size > old_size && unsafe { VM.bytes_allocated > VM.next_gc } {
        collect_garbage();
    }

    if new_size == 0 {
        // TODO(aalhendi): Is this null check needed?
        if !pointer.is_null() {
            unsafe {
                dealloc(pointer as *mut u8, Layout::array::<u8>(old_size).unwrap());
            }
        }
        std::ptr::null_mut()
    } else {
        #[cfg(target_os = "windows")]
        // NOTE(aalhendi): Windows needs null check and explicit alloc
        // See: https://github.com/rust-lang/rust/blob/8c3a94a1c79c67924558a4adf7fb6d98f5f0f741/library/std/src/sys/pal/windows/alloc.rs#L42-L66
        if pointer.is_null() {
            return unsafe {
                alloc(Layout::array::<u8>(new_size).unwrap()) as *mut std::ffi::c_void
            };
        }
        let old_layout = Layout::array::<u8>(old_size).unwrap();
        // let new_layout = Layout::array::<u8>(new_size).unwrap();
        // let new_ptr = unsafe { realloc(pointer as *mut u8, old_layout, new_layout.size()) };
        let new_ptr = unsafe { realloc(pointer as *mut u8, old_layout, new_size) };
        if new_ptr.is_null() {
            eprintln!("Failed to reallocate memory");
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

fn free_object(object: *mut Obj) {
    unsafe {
        #[cfg(feature = "debug-log-gc")]
        {
            println!("{object:p} free type {t:?}", t = (*object).type_);
        }
        match (*object).type_ {
            ObjType::String => {
                let string = object as *mut ObjString;
                FREE_ARRAY!(u8, (*string).chars, (*string).length);
                FREE!(ObjString, object);
            }
            ObjType::Function => {
                let function = object as *mut ObjFunction;
                (*function).chunk.free();
                FREE!(ObjFunction, object);
            }
            ObjType::Native => {
                FREE!(ObjNative, object);
            }
            ObjType::Closure => {
                let closure = object as *mut ObjClosure;
                FREE_ARRAY!(
                    *mut ObjUpvalue,
                    (*closure).upvalues,
                    (*closure).upvalue_count
                );
                FREE!(ObjClosure, object);
            }
            ObjType::Upvalue => {
                FREE!(ObjUpvalue, object);
            }
            ObjType::Class => {
                let class = object as *mut ObjClass2;
                (*class).methods.free();
                FREE!(ObjClass2, object);
            }
            ObjType::Instance => {
                let instance = object as *mut ObjInstance2;
                (*instance).fields.free();
                FREE!(ObjInstance2, object);
            }
            ObjType::BoundMethod => {
                FREE!(ObjBoundMethod2, object);
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

        let layout = Layout::array::<*mut Obj>(VM.gray_capacity).unwrap();
        dealloc(VM.gray_stack as *mut u8, layout);
    }
}

pub fn collect_garbage() {
    #[cfg(feature = "debug-log-gc")]
    let before = unsafe { VM.bytes_allocated };

    #[cfg(feature = "debug-log-gc")]
    println!("-- gc begin");

    mark_roots();
    trace_references();
    sweep();

    unsafe {
        VM.next_gc = VM.bytes_allocated * GC_HEAP_GROWTH_FACTOR;
    }

    #[cfg(feature = "debug-log-gc")]
    {
        println!(
            "-- gc end   collected {} bytes (from {} to {}) next at {}",
            unsafe { before - VM.bytes_allocated },
            before,
            unsafe { VM.bytes_allocated },
            unsafe { VM.next_gc }
        );
    }
}

fn mark_roots() {
    unsafe {
        let mut slot = VM.stack.as_mut_ptr();
        while slot < VM.stack_top {
            mark_value(*slot);

            slot = slot.offset(1);
        }

        let mut i = 0;
        while i < VM.frame_count {
            mark_object(VM.frames[i].closure as *mut Obj);

            i += 1;
        }

        let mut upvalue = VM.open_upvalues;
        while !upvalue.is_null() {
            mark_object(upvalue as *mut Obj);

            upvalue = (*upvalue).next;
        }

        VM.globals.mark();
        mark_compiler_roots();
        mark_object(VM.init_string as *mut Obj);
    }
}

// checks if actual heap object and does work
// numbers, Booleans, and nil are stored directly inline in Value and require no heap allocation
// gc doesn’t need to worry about them
pub fn mark_value(value: Value) {
    if value.is_obj() {
        mark_object(value.as_obj());
    }
}

fn mark_array(array: &mut ValueArray) {
    for i in 0..array.count {
        mark_value(unsafe { *array.values.wrapping_add(i) });
    }
}

fn blacken_object(object: *mut Obj) {
    #[cfg(feature = "debug-log-gc")]
    {
        println!("{object:p} blacken {v}", v = Value::obj_val(object));
    }

    unsafe {
        match (*object).type_ {
            ObjType::String | ObjType::Native => { /* Nothing to traverse */ }
            ObjType::Function => {
                let function = object as *mut ObjFunction;
                mark_object((*function).name as *mut Obj);
                mark_array(&mut (*function).chunk.constants);
            }
            ObjType::Closure => {
                let closure = object as *mut ObjClosure;
                mark_object((*closure).function as *mut Obj);
                for i in 0..(*closure).upvalue_count {
                    mark_object((*closure).upvalues.wrapping_add(i) as *mut Obj);
                }
            }
            ObjType::Upvalue => mark_value((*(object as *mut ObjUpvalue)).closed),
            ObjType::Class => {
                let class = object as *mut ObjClass2;
                mark_object((*class).name as *mut Obj);
                (*class).methods.mark();
            }
            ObjType::Instance => {
                let instance = object as *mut ObjInstance2;
                mark_object((*instance).class as *mut Obj);
                (*instance).fields.mark();
            }
            ObjType::BoundMethod => {
                let bound = object as *mut ObjBoundMethod2;
                mark_value((*bound).reciever);
                mark_object((*bound).method as *mut Obj);
            }
        }
    }
}

pub fn mark_object(object: *mut Obj) {
    if object.is_null() {
        return;
    }
    unsafe {
        if (*object).is_marked {
            return;
        }

        #[cfg(feature = "debug-log-gc")]
        {
            println!("{object:p} mark {v}", v = Value::obj_val(object));
        }
        (*object).is_marked = true;

        if VM.gray_capacity < VM.gray_count + 1 {
            VM.gray_capacity = GROW_CAPACITY!(VM.gray_capacity);
            // TODO(aalhendi): verify
            // calls system realloc and not our wrapper.
            // memory for the gray stack itself is not managed by the garbage collector
            VM.gray_stack = realloc(
                VM.gray_stack as *mut u8,
                Layout::array::<*mut Obj>(VM.gray_capacity).unwrap(),
                std::mem::size_of::<*mut Obj>() * VM.gray_capacity,
            ) as *mut *mut Obj;

            if VM.gray_stack.is_null() {
                std::process::exit(1);
            }
        }
        *VM.gray_stack.wrapping_add(VM.gray_count) = object;
        VM.gray_count += 1;
    }
}

fn trace_references() {
    unsafe {
        while VM.gray_count > 0 {
            VM.gray_count -= 1;
            let object = *VM.gray_stack.wrapping_add(VM.gray_count);
            blacken_object(object);
        }
    }
}

fn sweep() {
    let mut previous = std::ptr::null_mut();
    unsafe {
        let mut object = VM.objects;
        while !object.is_null() {
            if (*object).is_marked {
                (*object).is_marked = false;
                previous = object;
                object = (*object).next;
            } else {
                let unreached = object;
                object = (*object).next;
                if previous.is_null() {
                    VM.objects = object;
                } else {
                    (*previous).next = object;
                }
                free_object(unreached);
            }
        }
    }
}

// PERF(aalhendi): is this good enough? Not sure if theres a built-in version
/// Returns an integer less than, equal to, or greater than zero if `s1` is found,
/// respectively, to be less than, to match, or be greater than `s2`.
pub unsafe fn memcmp(s1: *const u8, s2: *const u8, n: usize) -> i32 {
    for i in 0..n {
        let a = *s1.add(i);
        let b = *s2.add(i);
        if a != b {
            return a as i32 - b as i32;
        }
    }
    0
}
