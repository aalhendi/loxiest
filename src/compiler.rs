use crate::{
    memory::mark_object,
    object::{Obj, ObjFunction},
    parser::Parser,
    token::{Token, TokenType},
    COMPILER, CURRENT,
};

#[derive(PartialEq, Clone)]
pub enum FunctionType {
    Function,
    Script,
    Method,
    Initializer,
}

pub struct ClassCompiler {
    pub enclosing: *mut ClassCompiler,
    pub has_superclass: bool,
}

pub struct Compiler {
    pub enclosing: *mut Compiler,
    pub function: *mut ObjFunction,
    pub function_type: FunctionType,

    pub locals: [Local; u8::MAX as usize + 1],
    pub local_count: usize,
    pub upvalues: [Upvalue; u8::MAX as usize + 1],
    pub scope_depth: usize,
}

pub struct Local {
    pub name: Token,
    pub depth: Option<usize>,
    pub is_captured: bool,
}

pub struct Upvalue {
    pub index: u8,
    pub is_local: bool,
}

pub fn compile(source: &'static str) -> *mut ObjFunction {
    unsafe {
        let mut parser = Parser::new(source);
        COMPILER.init(FunctionType::Script, "");

        parser.advance();
        while !parser.is_match(&TokenType::Eof) {
            parser.declaration();
        }

        let function = parser.end_compiler();
        if parser.had_error {
            std::ptr::null_mut()
        } else {
            function
        }
    }
}

pub fn mark_compiler_roots() {
    unsafe {
        let mut compiler = CURRENT;
        while !compiler.is_null() {
            mark_object((*compiler).function as *mut Obj);
            compiler = (*compiler).enclosing;
        }
    }
}

impl Compiler {
    pub const fn new_uninit() -> Self {
        Self {
            locals: {
                const DEFAULT: Local = Local {
                    name: Token::undefined(),
                    depth: Some(0),
                    is_captured: false,
                };
                [DEFAULT; 256]
            },
            upvalues: {
                const DEFAULT: Upvalue = Upvalue {
                    index: 0,
                    is_local: false,
                };
                [DEFAULT; u8::MAX as usize + 1]
            },
            local_count: 0,
            scope_depth: 0,
            function: std::ptr::null_mut(),
            function_type: unsafe { std::mem::zeroed() },
            enclosing: std::ptr::null_mut(),
        }
    }

    pub fn init(&mut self, function_type: FunctionType, function_name: &str) {
        let is_script = function_type == FunctionType::Script;
        let is_function = function_type == FunctionType::Function;
        self.enclosing = unsafe { CURRENT };
        self.function = std::ptr::null_mut();
        self.function_type = function_type;
        self.local_count = 0;
        self.scope_depth = 0;
        self.function = ObjFunction::new();
        unsafe {
            CURRENT = &mut *self;
            if !is_script {
                let chars = function_name.as_bytes();
                (*(*CURRENT).function).name = Obj::copy_string(chars, chars.len());
            }

            // compiler implicitly claims stack slot zero for the VM’s own internal use
            let local = &mut (*CURRENT).locals[(*CURRENT).local_count];
            (*CURRENT).local_count += 1;
            local.depth = Some(0);
            local.is_captured = false;
            local.name = if !is_function {
                Token::new(TokenType::This, "this", 0)
            } else {
                Token::undefined()
            };
        }
    }
}
