use crate::{
    memory::{self, mark_object, mark_value, reallocate},
    object::{Obj, ObjString},
    value::Value,
    ALLOCATE, FREE_ARRAY, GROW_CAPACITY,
};

const TABLE_MAX_LOAD: f64 = 0.75;

pub struct Table {
    count: usize,
    capacity: usize,
    entries: *mut Entry,
}

struct Entry {
    key: *mut ObjString,
    value: Value,
}

impl Table {
    pub fn init(&mut self) {
        self.count = 0;
        self.capacity = 0;
        self.entries = std::ptr::null_mut();
    }

    pub fn free(&mut self) {
        FREE_ARRAY!(Entry, self.entries, self.capacity);
        self.init();
    }

    fn find_entry(
        &mut self,
        entries: *mut Entry,
        key: *mut ObjString,
        capacity: usize,
    ) -> *mut Entry {
        unsafe {
            let mut index: u32 = (*key).hash & (capacity as u32 - 1);
            let mut tombstone: *mut Entry = std::ptr::null_mut();
            loop {
                let entry = entries.offset(index as isize);
                if (*entry).key.is_null() {
                    if (*entry).value.is_nil() {
                        // Empty entry
                        return if tombstone.is_null() {
                            entry
                        } else {
                            tombstone
                        };
                    } else {
                        // We found a tombstone.
                        if tombstone.is_null() {
                            tombstone = entry;
                        }
                    }
                } else if (*entry).key == key {
                    // We found the key.
                    return entry;
                }

                index = (index + 1) & (capacity as u32 - 1);
            }
        }
    }

    pub fn get(&mut self, key: *mut ObjString) -> Option<Value> {
        if self.count == 0 {
            return None;
        }

        let entry = self.find_entry(self.entries, key, self.capacity);
        unsafe {
            if (*entry).key.is_null() {
                return None;
            }

            Some((*entry).value)
        }
    }

    // NOTE(aalhendi): might have to move this as own fn if GC breaks.
    fn adjust_capcity(&mut self, capacity: usize) {
        let entries = ALLOCATE!(Entry, capacity);
        for i in 0..capacity {
            unsafe {
                (*entries.add(i)).key = std::ptr::null_mut();
                (*entries.add(i)).value = Value::nil_val();
            }
        }

        self.count = 0;
        for i in 0..self.capacity {
            unsafe {
                let entry = self.entries.add(i);
                if (*entry).key.is_null() {
                    continue;
                }

                let dst = self.find_entry(entries, (*entry).key, capacity);
                (*dst).key = (*entry).key;
                (*dst).value = (*entry).value;
                self.count += 1;
            }
        }

        FREE_ARRAY!(Entry, self.entries, self.capacity);
        self.entries = entries;
        self.capacity = capacity;
    }

    pub fn set(&mut self, key: *mut ObjString, value: Value) -> bool {
        if self.count + 1 > (self.capacity as f64 * TABLE_MAX_LOAD) as usize {
            let capacity = GROW_CAPACITY!(self.capacity);
            self.adjust_capcity(capacity);
        }

        let entry = self.find_entry(self.entries, key, self.capacity);
        unsafe {
            let is_new_key = (*entry).key.is_null();
            if is_new_key && (*entry).value.is_nil() {
                self.count += 1;
            }
            (*entry).key = key;
            (*entry).value = value;

            is_new_key
        }
    }

    pub fn delete(&mut self, key: *mut ObjString) -> bool {
        if self.count == 0 {
            return false;
        }

        let entry = self.find_entry(self.entries, key, self.capacity);
        unsafe {
            if (*entry).key.is_null() {
                return false;
            }

            // Place a tombstone in the entry.
            (*entry).key = std::ptr::null_mut();
            (*entry).value = Value::bool_val(true);
        }
        true
    }

    pub fn add_all(from: *mut Table, to: *mut Table) {
        unsafe {
            for i in 0..(*from).capacity {
                let entry = (*from).entries.add(i);
                if !(*entry).key.is_null() {
                    (*to).set((*entry).key, (*entry).value);
                }
            }
        }
    }

    pub fn find_string(&mut self, chars: *mut u8, length: usize, hash: u32) -> *mut ObjString {
        if self.count == 0 {
            return std::ptr::null_mut();
        }

        let mut index = hash & (self.capacity as u32 - 1);
        loop {
            unsafe {
                let entry = self.entries.offset(index as isize);
                if (*entry).key.is_null() {
                    // Stop if we find an empty non-tombstone entry.
                    if (*entry).value.is_nil() {
                        return std::ptr::null_mut();
                    }
                } else if (*(*entry).key).length == length
                    && (*(*entry).key).hash == hash
                    && memory::memcmp((*(*entry).key).chars, chars, length) == 0
                {
                    // We found it.
                    return (*entry).key;
                }
            }
            index = (index + 1) & (self.capacity as u32 - 1);
        }
    }

    pub fn remove_white(&mut self) {
        for i in 0..self.capacity {
            unsafe {
                let entry = self.entries.add(i);
                if !(*entry).key.is_null() && !(*(*entry).key).obj.is_marked {
                    self.delete((*entry).key);
                }
            }
        }
    }

    pub fn mark(&mut self) {
        for i in 0..self.capacity {
            unsafe {
                let entry = self.entries.add(i);
                mark_object((*entry).key as *mut Obj);
                mark_value((*entry).value);
            }
        }
    }
}
