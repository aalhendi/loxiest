// TODO(aalhendi): Remove...
#![allow(dead_code)]
#![allow(unused_imports)]

use std::{
    env, fs,
    io::{self, BufRead, Write},
    mem::MaybeUninit,
    ptr::addr_of_mut,
};

use chunk::{Chunk2, OpCode};
use compiler2::{ClassCompiler, Compiler2, Parser};
use scanner::Scanner;
use vm::{VM, VM2};

mod chunk;
mod compiler;
mod compiler2;
mod memory;
mod object;
mod object2;
mod scanner;
mod table;
mod token;
mod value;
mod vm;

// NOTE(aalhendi): Cant use MaybeUninit::uninit().assume_init() because static variables
// must be initialized with a constant value or an expression that can be evaluated at compile-time.
pub static mut VM: VM2 = unsafe { std::mem::zeroed() };
pub static mut COMPILING_CHUNK: *mut Chunk2 = unsafe { std::mem::zeroed() };
// TODO(aalhendi): eventually, compiler shouldn't be global
pub static mut CURRENT: *mut Compiler2 = std::ptr::null_mut();
pub static mut CURRENT_CLASS: *mut ClassCompiler = std::ptr::null_mut();
pub static mut COMPILER: Compiler2 = Compiler2::new_uninit();
pub static mut PARSER: compiler2::Parser = Parser::new(String::new());

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

fn run_file(vm: *mut VM2, file_path: &str) -> io::Result<()> {
    let contents = fs::read_to_string(file_path)?;
    // EX_DATAERR (65) User input data was incorrect in some way.
    // EX_SOFTWARE (70) Internal software error. Limited to non-OS errors.
    match unsafe { (*vm).interpret(contents) } {
        Ok(()) => Ok(()),
        Err(e) => match e {
            vm::InterpretResult::CompileError => std::process::exit(65),
            vm::InterpretResult::RuntimeError => std::process::exit(70),
        },
    }
}

/// Goes into prompt-mode. Starts a REPL:
/// Read a line of input, Evaluate it, Print the result, then Loop
fn repl(vm: *mut VM2) {
    print!("> ");
    io::stdout().flush().expect("Unable to flush stdout");
    for line in io::stdin().lock().lines() {
        match line {
            Ok(line) => {
                if line.is_empty() {
                    break;
                }

                // TODO: Discarded result
                let _ = unsafe { (*vm).interpret(line) };
                print!("> ");
                io::stdout().flush().expect("Unable to flush stdout");
            }
            Err(e) => panic!("{e}"),
        }
    }
}
