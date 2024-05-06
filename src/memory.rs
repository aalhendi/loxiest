use std::alloc::{dealloc, realloc, Layout};

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
