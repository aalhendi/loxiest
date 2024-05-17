// TODO(aalhendi): Remove...
#![allow(dead_code)]
#![allow(unused_imports)]

use std::{
    env, fs,
    io::{self, BufRead, Write},
    mem::MaybeUninit,
};

use chunk::{Chunk2, OpCode};
use vm::{VM, VM2};

mod chunk;
mod compiler;
mod compiler2;
mod memory;
mod object;
mod scanner;
mod token;
mod value;
mod vm;

fn main() {
    #[allow(clippy::uninit_assumed_init)]
    #[allow(invalid_value)]
    let mut vm: VM2 = unsafe { MaybeUninit::uninit().assume_init() };
    vm.init();

    let args: Vec<String> = env::args().collect();

    match args.len() {
        1 => repl(&mut vm),
        2 => run_file(&mut vm, &args[1]).expect("Unable to run file."),
        _ => {
            println!("Usage: clox [path]");
            // EX_USAGE (64) Command was used incorrectly, e.g., with the wrong number of arguments, a bad flag, bad syntax in a parameter, or whatever.
            std::process::exit(64)
        }
    }

    vm.free();
}

fn run_file(vm: &mut VM2, file_path: &str) -> io::Result<()> {
    let contents = fs::read_to_string(file_path)?;
    // EX_DATAERR (65) User input data was incorrect in some way.
    // EX_SOFTWARE (70) Internal software error. Limited to non-OS errors.
    match vm.interpret(&contents) {
        Ok(()) => Ok(()),
        Err(e) => match e {
            vm::InterpretResult::CompileError => std::process::exit(65),
            vm::InterpretResult::RuntimeError => std::process::exit(70),
        },
    }
}

/// Goes into prompt-mode. Starts a REPL:
/// Read a line of input, Evaluate it, Print the result, then Loop
fn repl(vm: &mut VM2) {
    print!("> ");
    io::stdout().flush().expect("Unable to flush stdout");
    for line in io::stdin().lock().lines() {
        match line {
            Ok(line) => {
                if line.is_empty() {
                    break;
                }

                // TODO: Discarded result
                let _ = vm.interpret(&line);
                print!("> ");
                io::stdout().flush().expect("Unable to flush stdout");
            }
            Err(e) => panic!("{e}"),
        }
    }
}
