#![feature(variant_count)]

use std::{
    env, fs,
    io::{self, BufRead, Write},
    ptr::addr_of_mut,
};

use compiler::Compiler;
use vm::VM;

mod chunk;
mod compiler;
mod memory;
mod object;
mod parser;
mod scanner;
mod table;
mod token;
mod value;
mod vm;

// Global storage source string. Used to get 'static lifetimes
static mut GLOBAL_SOURCE: String = String::new();
// NOTE(aalhendi): Cant use MaybeUninit::uninit().assume_init() because static variables
// must be initialized with a constant value or an expression that can be evaluated at compile-time.
pub static mut VM: VM = unsafe { std::mem::zeroed() };
// TODO(aalhendi): eventually, compiler shouldn't be global
pub static mut CURRENT: *mut Compiler = std::ptr::null_mut();

fn main() {
    unsafe {
        VM.init();

        let args: Vec<String> = env::args().collect();

        match args.len() {
            1 => repl(addr_of_mut!(VM)),
            2 => run_file(addr_of_mut!(VM), &args[1]).expect("Unable to run file."),
            _ => {
                println!("Usage: clox [path]");
                // EX_USAGE (64) Command was used incorrectly, e.g., with the wrong number of arguments, a bad flag, bad syntax in a parameter, or whatever.
                std::process::exit(64)
            }
        }

        VM.free();
    }
}

fn run_file(vm: *mut VM, file_path: &str) -> io::Result<()> {
    let contents = fs::read_to_string(file_path)?;
    // EX_DATAERR (65) User input data was incorrect in some way.
    // EX_SOFTWARE (70) Internal software error. Limited to non-OS errors.

    unsafe { GLOBAL_SOURCE = contents };
    match unsafe { (*vm).interpret(GLOBAL_SOURCE.as_str()) } {
        Ok(()) => Ok(()),
        Err(e) => match e {
            vm::InterpretResult::CompileError => std::process::exit(65),
            vm::InterpretResult::RuntimeError => std::process::exit(70),
        },
    }
}

/// Goes into prompt-mode. Starts a REPL:
/// Read a line of input, Evaluate it, Print the result, then Loop
fn repl(vm: *mut VM) {
    print!("> ");
    io::stdout().flush().expect("Unable to flush stdout");
    for line in io::stdin().lock().lines() {
        match line {
            Ok(line) => {
                if line.is_empty() {
                    break;
                }

                let static_line = get_static_str(line);
                // TODO: Discarded result
                let _ = unsafe { (*vm).interpret(static_line) };
                print!("> ");
                io::stdout().flush().expect("Unable to flush stdout");
            }
            Err(e) => panic!("{e}"),
        }
    }
}

fn get_static_str(s: String) -> &'static str {
    unsafe {
        GLOBAL_SOURCE = s;
        GLOBAL_SOURCE.as_str()
    }
}
