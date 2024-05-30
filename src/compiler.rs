use std::{cell::RefCell, rc::Rc};

use crate::{
    chunk::{Chunk, OpCode},
    object::{Obj, ObjClosure, ObjFunction},
    scanner::Scanner,
    token::{Token, TokenType},
    value::Value,
};

#[derive(Debug, PartialEq, PartialOrd)]
#[repr(u8)]
enum Precedence {
    None,
    Assignment, // =
    Or,         // or
    And,        // and
    Equality,   // == !=
    Comparison, // < > <= >=
    Term,       // + -
    Factor,     // * /
    Unary,      // ! -
    Call,       // . ()
    Primary,
}

impl From<u8> for Precedence {
    fn from(value: u8) -> Self {
        match value {
            0 => Precedence::None,
            1 => Precedence::Assignment,
            2 => Precedence::Or,
            3 => Precedence::And,
            4 => Precedence::Equality,
            5 => Precedence::Comparison,
            6 => Precedence::Term,
            7 => Precedence::Factor,
            8 => Precedence::Unary,
            9 => Precedence::Call,
            10 => Precedence::Primary,
            _ => panic!("Unknown precedence value: {}", value),
        }
    }
}

struct ParseRule {
    prefix: Option<fn(&mut Compiler, bool)>,
    infix: Option<fn(&mut Compiler, bool)>,
    precedence: Precedence,
}

impl ParseRule {
    fn new(
        prefix: Option<fn(&mut Compiler, bool)>,
        infix: Option<fn(&mut Compiler, bool)>,
        precedence: Precedence,
    ) -> Self {
        Self {
            prefix,
            infix,
            precedence,
        }
    }
}

pub struct Parser {
    previous: Token,
    current: Token,
    had_error: bool,
    panic_mode: bool,
}

impl Parser {
    fn new() -> Self {
        Self {
            previous: Token::undefined(),
            current: Token::undefined(),
            had_error: false,
            panic_mode: false,
        }
    }
}

struct Local {
    name: Token,
    depth: Option<u8>,
    is_captured: bool,
}

impl Local {
    pub fn new(name: Token, depth: Option<u8>) -> Self {
        Self {
            name,
            depth,
            is_captured: false,
        }
    }
}

#[derive(PartialEq, Clone)]
pub enum FunctionType {
    Function,
    Script,
    Method,
    Initializer,
}

struct Upvalue {
    is_local: bool,
    index: usize,
}

pub struct CompilerState {
    pub function: ObjFunction,
    kind: FunctionType,
    locals: Vec<Local>,
    scope_depth: u8,
    upvalues: Vec<Upvalue>,
}

impl CompilerState {
    fn new(kind: FunctionType) -> Self {
        Self {
            function: ObjFunction::new("", 0), // NOTE: Sort of a placeholder
            locals: {
                let mut locals = Vec::with_capacity(u8::MAX.into());
                let local = if kind != FunctionType::Function {
                    Local::new(Token::new(TokenType::This, "this", 0), Some(0))
                } else {
                    Local::new(Token::undefined(), Some(0))
                };
                locals.push(local);
                locals
            },
            kind,
            upvalues: Vec::with_capacity(u8::MAX as usize + 1),
            scope_depth: 0,
        }
    }
}

pub struct ClassCompiler {
    pub enclosing: Option<Rc<ClassCompiler>>,
    pub has_superclass: RefCell<bool>,
}

impl ClassCompiler {
    pub fn new(enclosing: Option<Rc<ClassCompiler>>) -> Self {
        Self {
            enclosing,
            has_superclass: RefCell::new(false),
        }
    }
}

pub struct Compiler {
    parser: Parser,
    scanner: Scanner,
    pub state: Vec<CompilerState>,               // Stack
    pub class_compilers: Vec<Rc<ClassCompiler>>, // Stack to manage class scopes. (current_class)
}

impl Compiler {
    pub fn new(source: String, kind: FunctionType) -> Self {
        Self {
            parser: Parser::new(),
            state: vec![CompilerState::new(kind)],
            scanner: Scanner::new(source),
            class_compilers: Vec::new(),
        }
    }

    pub fn compile(&mut self) -> Option<Rc<ObjFunction>> {
        self.advance();
        while !self.is_match(&TokenType::Eof) {
            self.declaration();
        }
        let function = self.end_compiler();
        if self.parser.had_error {
            None
        } else {
            Some(function)
        }
    }

    #[inline]
    pub fn current_chunk(&mut self) -> &mut Chunk {
        &mut self.state.last_mut().unwrap().function.chunk
    }

    fn emit_byte<T: Into<u8>>(&mut self, byte: T) {
        let line = self.parser.previous.line;
        self.current_chunk().write_byte(byte, line);
    }

    fn emit_loop(&mut self, loop_start: usize) {
        self.emit_byte(OpCode::Loop);

        // +2 to adjust for bytecode for OP_LOOP offset itself
        let offset = self.current_chunk().count() - loop_start + 2;
        if offset > u16::MAX as usize {
            self.error("Loop body too large.");
        }

        self.emit_byte(((offset >> 8) & u8::MAX as usize) as u8);
        self.emit_byte((offset & u8::MAX as usize) as u8);
    }

    fn emit_jump(&mut self, instruction: OpCode) -> usize {
        self.emit_byte(instruction);
        self.emit_byte(u8::MAX);
        self.emit_byte(u8::MAX);

        self.current_chunk().count() - 2
    }

    fn patch_jump(&mut self, offset: usize) {
        // -2 to adjust for bytecode for jmp offset itself
        let jump = self.current_chunk().count() - offset - 2;

        if jump > u16::MAX.into() {
            self.error("Too much code to jump over.");
        }

        self.current_chunk().code[offset] = ((jump >> 8) & u8::MAX as usize) as u8;
        self.current_chunk().code[offset + 1] = (jump as u8) & u8::MAX;
        // TODO: Change to something like this
        // self.chunk.code[offset..offset + 2].copy_from_slice(&jump.to_le_bytes());
    }

    fn end_compiler(&mut self) -> Rc<ObjFunction> {
        self.emit_return();
        let function = self.state.last().unwrap().function.clone();
        #[cfg(feature = "debug-print-code")]
        {
            if !self.parser.had_error {
                let name = if function.name.is_empty() {
                    "<script>"
                } else {
                    function.name.as_str()
                };

                self.current_chunk().disassemble(name);
            }
        }

        Rc::new(function)
    }

    fn begin_scope(&mut self) {
        self.state.last_mut().unwrap().scope_depth += 1;
    }

    fn end_scope(&mut self) {
        self.state.last_mut().unwrap().scope_depth -= 1;
        // While not empty and greater than current scope depth
        while self
            .state
            .last()
            .unwrap()
            .locals
            .last()
            .is_some_and(|l| l.depth > Some(self.state.last().unwrap().scope_depth))
        {
            if self
                .state
                .last()
                .unwrap()
                .locals
                .last()
                .unwrap()
                .is_captured
            {
                self.emit_byte(OpCode::CloseUpvalue);
            } else {
                self.emit_byte(OpCode::Pop);
            }
            self.state.last_mut().unwrap().locals.pop();
        }
    }

    fn emit_return(&mut self) {
        if self.state.last().unwrap().kind == FunctionType::Initializer {
            self.emit_bytes(OpCode::GetLocal as u8, 0);
        } else {
            self.emit_byte(OpCode::Nil);
        }
        self.emit_byte(OpCode::Return);
    }

    fn emit_bytes(&mut self, byte1: u8, byte2: u8) {
        self.emit_byte(byte1);
        self.emit_byte(byte2);
    }

    fn expression(&mut self) {
        self.parse_precedence(Precedence::Assignment);
    }

    fn block(&mut self) {
        while !self.check(&TokenType::RightBrace) && !self.check(&TokenType::Eof) {
            self.declaration();
        }
        self.consume(TokenType::RightBrace, "Expect '}' after block.");
    }

    fn function(&mut self, kind: FunctionType) {
        self.state.push(CompilerState::new(kind.clone()));

        if kind != FunctionType::Script {
            self.state.last_mut().unwrap().function.name = self.parser.previous.lexeme.to_string();
        }
        self.begin_scope();

        self.consume(TokenType::LeftParen, "Expect '(' after function name.");
        if !self.check(&TokenType::RightParen) {
            loop {
                let current = self.state.last_mut().unwrap();
                current.function.arity += 1;
                if current.function.arity > u8::MAX as usize {
                    self.error_at_current("Can't have more than 255 parameters.".to_owned());
                }

                // Semantically, a parameter is simply a local variable declared in the outermost lexical scope of the function body.
                let constant = self.parse_variable("Expect parameter name.");
                self.define_variable(constant);

                if !self.is_match(&TokenType::Comma) {
                    break;
                }
            }
        }
        self.consume(TokenType::RightParen, "Expect ')' after parameters.");
        self.consume(TokenType::LeftBrace, "Expect '{' before function body.");
        self.block();

        let function = self.end_compiler();
        // Point the CompileState to the enclosing one and discard the current compiler state.
        // To be used at the end of a script or when exiting a function.
        let state = self.state.pop().unwrap();
        let constant = self.make_constant(Value::Obj(
            Obj::Closure(Rc::new(ObjClosure::new(function))).into(),
        ));
        self.emit_bytes(OpCode::Closure as u8, constant);

        for i in state.upvalues {
            let b = if i.is_local { 1 } else { 0 };
            self.emit_byte(b);
            self.emit_byte(i.index as u8);
        }
    }

    fn method(&mut self) {
        self.consume(TokenType::Identifier, "Expect method name.");
        let constant = self.identifier_constant(&self.parser.previous.clone());

        let kind = if self.parser.previous.lexeme == "init" {
            FunctionType::Initializer
        } else {
            FunctionType::Method
        };
        self.function(kind);
        self.emit_bytes(OpCode::Method as u8, constant);
    }

    fn class_declaration(&mut self) {
        self.consume(TokenType::Identifier, "Expect class name.");
        // TODO(aalhendi): Rc?
        let class_name = &self.parser.previous.clone();
        let name_constant = self.identifier_constant(class_name);
        self.declare_variable();

        self.emit_bytes(OpCode::Class as u8, name_constant);
        self.define_variable(name_constant);

        let enclosing = self.class_compilers.last().cloned(); // None if empty, top of stack
        let new_class_compiler = Rc::new(ClassCompiler::new(enclosing));
        self.class_compilers.push(new_class_compiler);

        if self.is_match(&TokenType::Less) {
            self.consume(TokenType::Identifier, "Expect superclass name.");
            self.variable(false);

            if class_name.lexeme == self.parser.previous.lexeme {
                self.error("A class can't inherit from itself.");
            }

            self.begin_scope();
            // synthetic token
            self.add_local(&Token::new(TokenType::Super, "super", 0));
            self.define_variable(0);

            self.named_variable(class_name, false);
            self.emit_byte(OpCode::Inherit);
            self.class_compilers
                .last()
                .unwrap()
                .has_superclass
                .replace(true);
        }

        self.named_variable(class_name, false);
        self.consume(TokenType::LeftBrace, "Expect '{' before class body.");
        while !self.check(&TokenType::RightBrace) && !self.check(&TokenType::Eof) {
            self.method();
        }
        self.consume(TokenType::RightBrace, "Expect '}' after class body.");
        self.emit_byte(OpCode::Pop);

        if *self.class_compilers.last().unwrap().has_superclass.borrow() {
            self.end_scope();
        }

        // Pop the current ClassCompiler to restore previous scope.
        self.class_compilers.pop();
    }

    fn fun_declaration(&mut self) {
        let global = self.parse_variable("Expect function name.");
        self.mark_initialized();
        self.function(FunctionType::Function);
        self.define_variable(global);
    }

    fn var_declaration(&mut self) {
        let global = self.parse_variable("Expect variable name.");

        if self.is_match(&TokenType::Equal) {
            self.expression();
        } else {
            // Desugars an empty declaration like ``var a;`` to ``var a = nil;``
            self.emit_byte(OpCode::Nil);
        }
        self.consume(
            TokenType::Semicolon,
            "Expect ';' after variable declaration.",
        );

        self.define_variable(global);
    }

    fn expression_statement(&mut self) {
        self.expression();
        self.consume(TokenType::Semicolon, "Expect ';' after expression.");
        self.emit_byte(OpCode::Pop);
    }

    fn for_statement(&mut self) {
        self.begin_scope();
        self.consume(TokenType::LeftParen, "Expect '(' after 'for'.");
        if self.is_match(&TokenType::Semicolon) {
            // No initializer
        } else if self.is_match(&TokenType::Var) {
            self.var_declaration();
        } else {
            // consumes semicolon.
            self.expression_statement();
        }

        let mut loop_start = self.current_chunk().count();
        let mut exit_jump = None;
        if !self.is_match(&TokenType::Semicolon) {
            self.expression();
            self.consume(TokenType::Semicolon, "Expect ';' after loop condition.");

            // Jump out of loop if condition is false.
            exit_jump = Some(self.emit_jump(OpCode::JumpIfFalse));
            self.emit_byte(OpCode::Pop);
        }

        if !self.is_match(&TokenType::RightParen) {
            let body_jump = self.emit_jump(OpCode::Jump);
            let increment_start = self.current_chunk().count();
            self.expression();
            self.emit_byte(OpCode::Pop);
            self.consume(TokenType::RightParen, "Expect ')' after for clauses.");

            self.emit_loop(loop_start);
            loop_start = increment_start;
            self.patch_jump(body_jump);
        }

        self.statement();
        self.emit_loop(loop_start);

        if let Some(exit_jump) = exit_jump {
            self.patch_jump(exit_jump);
            self.emit_byte(OpCode::Pop); // pops the condition value from stack
        }

        self.end_scope();
    }

    fn if_statement(&mut self) {
        self.consume(TokenType::LeftParen, "Expect '(' after 'if'.");
        self.expression();
        self.consume(TokenType::RightParen, "Expect ')' after condition.");

        let then_jump = self.emit_jump(OpCode::JumpIfFalse);
        self.emit_byte(OpCode::Pop);
        self.statement();

        let else_jump = self.emit_jump(OpCode::Jump);

        self.patch_jump(then_jump);
        self.emit_byte(OpCode::Pop);

        if self.is_match(&TokenType::Else) {
            self.statement();
        }

        self.patch_jump(else_jump);
    }

    fn declaration(&mut self) {
        if self.is_match(&TokenType::Class) {
            self.class_declaration();
        } else if self.is_match(&TokenType::Fun) {
            self.fun_declaration();
        } else if self.is_match(&TokenType::Var) {
            self.var_declaration();
        } else {
            self.statement();
        }

        if self.parser.panic_mode {
            self.synchronize();
        }
    }

    /// Compiles statements. Statements have zero stack effect.
    fn statement(&mut self) {
        // TODO(aalhendi): Replace with match...
        if self.is_match(&TokenType::Print) {
            self.print_statement();
        } else if self.is_match(&TokenType::LeftBrace) {
            self.begin_scope();
            self.block();
            self.end_scope();
        } else if self.is_match(&TokenType::If) {
            self.if_statement();
        } else if self.is_match(&TokenType::Return) {
            self.return_statement();
        } else if self.is_match(&TokenType::While) {
            self.while_statement();
        } else if self.is_match(&TokenType::For) {
            self.for_statement();
        } else {
            self.expression_statement();
        }
    }

    fn print_statement(&mut self) {
        self.expression();
        self.consume(TokenType::Semicolon, "Expect ';' after value.");
        self.emit_byte(OpCode::Print);
    }

    fn return_statement(&mut self) {
        if self.state.last().unwrap().kind == FunctionType::Script {
            self.error("Can't return from top-level code.");
        }
        if self.is_match(&TokenType::Semicolon) {
            // This implicitly returns nil.
            self.emit_return();
        } else {
            if self.state.last().unwrap().kind == FunctionType::Initializer {
                self.error("Can't return a value from an initializer.");
            }
            // compile the value afterwards so compiler doesn’t get confused by trailing expression
            // and report a bunch of cascaded errors

            self.expression();
            self.consume(TokenType::Semicolon, "Expect ';' after return value.");
            self.emit_byte(OpCode::Return);
        }
    }

    /// Similar to if-statements, compiles condition expresion surrounded by mandatory parenthesis.
    /// If condition is falsey then jump and skip over the subsequent body statement.
    /// If truthy, execute the body statement then jump back to the loop_start before the condition.
    /// This re-evaluates the condition expression on every iteration.
    fn while_statement(&mut self) {
        let loop_start = self.current_chunk().count();
        self.consume(TokenType::LeftParen, "Expect '(' after 'while'.");
        self.expression();
        self.consume(TokenType::RightParen, "Expect ')' after condition.");

        let exit_jump = self.emit_jump(OpCode::JumpIfFalse);
        self.emit_byte(OpCode::Pop);
        self.statement();
        self.emit_loop(loop_start);

        self.patch_jump(exit_jump);
        self.emit_byte(OpCode::Pop);
    }

    fn synchronize(&mut self) {
        self.parser.panic_mode = false;

        while self.parser.current.kind != TokenType::Eof {
            if self.parser.previous.kind == TokenType::Semicolon {
                return;
            }
            match self.parser.current.kind {
                TokenType::Class
                | TokenType::Fun
                | TokenType::Var
                | TokenType::If
                | TokenType::While
                | TokenType::Print
                | TokenType::Return => return,
                _ => (),
            }
            self.advance();
        }
    }

    fn number(&mut self) {
        let value = self
            .parser
            .previous
            .lexeme
            .parse::<f64>()
            .expect("Unable to parse number");
        self.emit_constant(Value::Number(value));
    }

    /// Or expressions are "lazy" evaluated and short circuit if the left-hand side is truthy.
    /// This means we skip the right operand and need to jump to the end of the right operand expression.
    fn or(&mut self) {
        let else_jump = self.emit_jump(OpCode::JumpIfFalse);
        let end_jump = self.emit_jump(OpCode::Jump);

        self.patch_jump(else_jump);
        self.emit_byte(OpCode::Pop);

        self.parse_precedence(Precedence::Or);
        self.patch_jump(end_jump);
    }

    fn string(&mut self) {
        // This strips the quotation marks
        let str = &self.parser.previous.lexeme[1..self.parser.previous.lexeme.len() - 1];
        self.emit_constant(Value::Obj(Obj::String(Rc::from(str)).into()))
    }

    // Resolves local, global or upvalue
    fn named_variable(&mut self, name: &Token, can_assign: bool) {
        let (arg, get_op, set_op) =
            if let Some(arg_local) = self.resolve_local(name, self.state.len() - 1) {
                (arg_local, OpCode::GetLocal, OpCode::SetLocal)
            } else if let Some(arg_upvalue) = self.resolve_upvalue(name, self.state.len() - 1) {
                (arg_upvalue, OpCode::GetUpvalue, OpCode::SetUpvalue)
            } else {
                (
                    self.identifier_constant(name),
                    OpCode::GetGlobal,
                    OpCode::SetGlobal,
                )
            };
        if can_assign && self.is_match(&TokenType::Equal) {
            self.expression();
            self.emit_bytes(set_op as u8, arg);
        } else {
            self.emit_bytes(get_op as u8, arg);
        }
    }

    fn variable(&mut self, can_assign: bool) {
        self.named_variable(&self.parser.previous.clone(), can_assign);
    }

    fn super_(&mut self) {
        if let Some(class) = self.class_compilers.last() {
            if !*class.has_superclass.borrow() {
                self.error("Can't use 'super' in a class with no superclass.");
            }
        } else {
            self.error("Can't use 'super' outside of a class.");
        }

        self.consume(TokenType::Dot, "Expect '.' after 'super'.");
        self.consume(TokenType::Identifier, "Expect superclass method name.");
        let name = self.identifier_constant(&self.parser.previous.clone());

        self.named_variable(&Token::new(TokenType::This, "this", 0), false);
        if self.is_match(&TokenType::LeftParen) {
            let arg_count = self.argument_list();
            self.named_variable(&Token::new(TokenType::Super, "super", 0), false);
            self.emit_bytes(OpCode::SuperInvoke as u8, name);
            self.emit_byte(arg_count);
        } else {
            self.named_variable(&Token::new(TokenType::Super, "super", 0), false);
            self.emit_bytes(OpCode::GetSuper as u8, name);
        }
    }

    fn this(&mut self, _can_assign: bool) {
        if self.class_compilers.is_empty() {
            self.error("Can't use 'this' outside of a class.");
            return;
        }

        self.variable(false);
    }

    fn grouping(&mut self) {
        self.expression();
        self.consume(TokenType::RightParen, "Expect ')' after expression.")
    }

    fn unary(&mut self, _can_assignn: bool) {
        let operator_type = &self.parser.previous.kind.clone();
        self.parse_precedence(Precedence::Unary);

        match operator_type {
            TokenType::Bang => self.emit_byte(OpCode::Not),
            TokenType::Minus => self.emit_byte(OpCode::Negate),
            _ => unreachable!(),
        }
    }

    // NOTE: According to IEEE 754, all comparison operators return false when an operand is NaN./
    // That means NaN <= 1 is false and NaN > 1 is also false.
    // But our desugaring assumes the latter is always the negation of the former.
    // BONUS: Create instructions for (!=, <=, and >=). VM would execute faster if we did.
    fn binary(&mut self, can_assign: bool) {
        let operator_kind = &self.parser.previous.kind.clone();
        let rule = self.get_rule(operator_kind, can_assign);

        let next = self.next_precedence(rule.precedence);
        self.parse_precedence(next);

        match operator_kind {
            TokenType::Plus => self.emit_byte(OpCode::Add),
            TokenType::Minus => self.emit_byte(OpCode::Subtract),
            TokenType::Star => self.emit_byte(OpCode::Multiply),
            TokenType::Slash => self.emit_byte(OpCode::Divide),
            TokenType::BangEqual => self.emit_bytes(OpCode::Equal as u8, OpCode::Not as u8),
            TokenType::EqualEqual => self.emit_byte(OpCode::Equal),
            TokenType::Greater => self.emit_byte(OpCode::Greater),
            TokenType::GreaterEqual => self.emit_bytes(OpCode::Less as u8, OpCode::Not as u8),
            TokenType::Less => self.emit_byte(OpCode::Less),
            TokenType::LessEqual => self.emit_bytes(OpCode::Greater as u8, OpCode::Not as u8),
            _ => unreachable!(),
        }
    }

    fn call(&mut self) {
        let arg_count = self.argument_list();
        self.emit_bytes(OpCode::Call as u8, arg_count);
    }

    fn dot(&mut self, can_assign: bool) {
        self.consume(TokenType::Identifier, "Expect property name after '.'.");
        let name = self.identifier_constant(&self.parser.previous.clone());

        if can_assign && self.is_match(&TokenType::Equal) {
            self.expression();
            self.emit_bytes(OpCode::SetProperty as u8, name);
        } else if self.is_match(&TokenType::LeftParen) {
            let arg_count = self.argument_list();
            self.emit_bytes(OpCode::Invoke as u8, name);
            self.emit_byte(arg_count);
        } else {
            self.emit_bytes(OpCode::GetProperty as u8, name);
        }
    }

    fn literal(&mut self) {
        match &self.parser.previous.kind {
            TokenType::False => self.emit_byte(OpCode::False),
            TokenType::True => self.emit_byte(OpCode::True),
            TokenType::Nil => self.emit_byte(OpCode::Nil),
            _ => unreachable!(),
        }
    }

    fn next_precedence(&self, precedence: Precedence) -> Precedence {
        // Precedence::Primary has no next.
        Precedence::from(precedence as u8 + 1)
    }

    fn parse_precedence(&mut self, precedence: Precedence) {
        let can_assign = precedence <= Precedence::Assignment;
        self.advance();
        match self.get_rule(&self.parser.previous.kind, can_assign).prefix {
            Some(prefix_rule) => {
                prefix_rule(self, can_assign);

                while precedence
                    <= self
                        .get_rule(&self.parser.current.kind, can_assign)
                        .precedence
                {
                    self.advance();
                    if let Some(infix_rule) =
                        self.get_rule(&self.parser.previous.kind, can_assign).infix
                    {
                        infix_rule(self, can_assign);
                    }
                }
                if can_assign && self.is_match(&TokenType::Equal) {
                    self.error("Invalid assignment target.")
                }
            }
            None => self.error("Expect expression."),
        }
    }

    fn identifier_constant(&mut self, name: &Token) -> u8 {
        self.make_constant(Value::Obj(
            Obj::String(Rc::from(name.lexeme.as_str())).into(),
        ))
    }

    fn resolve_local(&mut self, name: &Token, state_idx: usize) -> Option<u8> {
        let state = &self.state[state_idx];
        for i in (0..state.locals.len()).rev() {
            let local = &state.locals[i];
            if name.lexeme == local.name.lexeme {
                if local.depth.is_none() {
                    self.error("Can't read local variable in its own initializer.");
                }
                return Some(i as u8);
            }
        }
        None
    }

    fn add_upvalue(&mut self, state_idx: usize, index: usize, is_local: bool) -> u8 {
        let state = &mut self.state[state_idx];
        let upvalue_count = state.function.upvalue_count;

        for i in 0..upvalue_count {
            let upvalue = &state.upvalues[i];
            if upvalue.index == index && upvalue.is_local == is_local {
                return i as u8;
            }
        }

        if upvalue_count == u8::MAX as usize + 1 {
            self.error("Too many closure variables in function.");
            return 0;
        }

        state.upvalues.push(Upvalue { is_local, index });
        state.function.upvalue_count += 1;
        state.function.upvalue_count as u8 - 1
    }

    fn resolve_upvalue(&mut self, name: &Token, state_idx: usize) -> Option<u8> {
        // If global scope (recursively decremented)
        if state_idx == 0 {
            return None;
        }

        match self.resolve_local(name, state_idx - 1) {
            Some(l) => {
                self.state[state_idx - 1].locals[l as usize].is_captured = true;
                Some(self.add_upvalue(state_idx, l as usize, true))
            }
            // Recursively resolve
            None => self
                .resolve_upvalue(name, state_idx - 1)
                .map(|u| self.add_upvalue(state_idx, u.into(), false)),
        }
    }

    fn add_local(&mut self, name: &Token) {
        if self.state.last().unwrap().locals.len() > u8::MAX.into() {
            self.error("Too many local variables in function.");
            return;
        }
        self.state
            .last_mut()
            .unwrap()
            .locals
            .push(Local::new(name.clone(), None));
    }

    fn declare_variable(&mut self) {
        if self.state.last().unwrap().scope_depth == 0 {
            return;
        }

        // TODO(aalhendi): This double clone business is not cool... find a workaround? RefCell?
        let name = &self.parser.previous.clone();

        for i in (0..self.state.last().unwrap().locals.len()).rev() {
            let local = &self.state.last().unwrap().locals[i];
            if local
                .depth
                .is_some_and(|d| d < self.state.last().unwrap().scope_depth)
            {
                break;
            }

            if name.lexeme == local.name.lexeme {
                self.error("Already a variable with this name in this scope.");
            }
        }
        self.add_local(name);
    }

    fn parse_variable(&mut self, error_msg: &str) -> u8 {
        self.consume(TokenType::Identifier, error_msg);

        self.declare_variable();
        if self.state.last().unwrap().scope_depth > 0 {
            return 0;
        }
        self.identifier_constant(&self.parser.previous.clone())
    }

    fn mark_initialized(&mut self) {
        self.state
            .last_mut()
            .unwrap()
            .locals
            .last_mut()
            .unwrap()
            .depth = Some(self.state.last().unwrap().scope_depth);
    }

    fn define_variable(&mut self, global: u8) {
        if self.state.last().unwrap().scope_depth > 0 {
            self.mark_initialized();
            return;
        }
        self.emit_bytes(OpCode::DefineGlobal as u8, global);
    }

    // TODO(aalhendi): Docs
    fn argument_list(&mut self) -> u8 {
        let mut arg_count = 0;

        if !self.check(&TokenType::RightParen) {
            loop {
                self.expression();
                if arg_count == u8::MAX {
                    self.error("Can't have more than 255 arguments.");
                }
                arg_count += 1;

                if !self.is_match(&TokenType::Comma) {
                    break;
                }
            }
        }
        self.consume(TokenType::RightParen, "Expect ')' after arguments.");
        arg_count
    }

    /// And expressions are "lazy" evaluated and short circuit if the left-hand side is falsey.
    /// This means we skip the right operand and need to jump to the end of the right operand expression.
    /// If the left-hand expression is truthy, then we discard it and eval the right operand expression.
    fn and(&mut self) {
        let end_jump = self.emit_jump(OpCode::JumpIfFalse);
        self.emit_byte(OpCode::Pop);
        self.parse_precedence(Precedence::And);
        self.patch_jump(end_jump);
    }

    fn get_rule(&self, kind: &TokenType, _can_assign: bool) -> ParseRule {
        // TODO: Make a fixed array called rules. Once cell it or something.
        // TODO: Kill this function once array is used. Just index the array
        use TokenType as t;
        match kind {
            t::LeftParen => ParseRule::new(
                Some(|c, _can_assign: bool| c.grouping()),
                Some(|c, _can_assign| c.call()),
                Precedence::Call,
            ),
            t::RightParen => ParseRule::new(None, None, Precedence::None),
            t::LeftBrace => ParseRule::new(None, None, Precedence::None),
            t::RightBrace => ParseRule::new(None, None, Precedence::None),
            t::Comma => ParseRule::new(None, None, Precedence::None),
            t::Dot => ParseRule::new(
                None,
                Some(|c, can_assign| c.dot(can_assign)),
                Precedence::Call,
            ),
            t::Minus => ParseRule::new(
                Some(|c, can_assign: bool| c.unary(can_assign)),
                Some(|c, can_assign: bool| c.binary(can_assign)),
                Precedence::Term,
            ),
            t::Plus => ParseRule::new(
                None,
                Some(|c, can_assign: bool| c.binary(can_assign)),
                Precedence::Term,
            ),
            t::Semicolon => ParseRule::new(None, None, Precedence::None),
            t::Slash => ParseRule::new(
                None,
                Some(|c, can_assign: bool| c.binary(can_assign)),
                Precedence::Factor,
            ),
            t::Star => ParseRule::new(
                None,
                Some(|c, can_assign: bool| c.binary(can_assign)),
                Precedence::Factor,
            ),
            t::Bang => ParseRule::new(
                Some(|c, can_assign: bool| c.unary(can_assign)),
                None,
                Precedence::None,
            ),
            t::BangEqual => ParseRule::new(
                None,
                Some(|c, can_assign: bool| c.binary(can_assign)),
                Precedence::Equality,
            ),
            t::Equal => ParseRule::new(None, None, Precedence::None),
            t::EqualEqual => ParseRule::new(
                None,
                Some(|c, can_assign: bool| c.binary(can_assign)),
                Precedence::Equality,
            ),
            t::Greater => ParseRule::new(
                None,
                Some(|c, can_assign: bool| c.binary(can_assign)),
                Precedence::Comparison,
            ),
            t::GreaterEqual => ParseRule::new(
                None,
                Some(|c, can_assign: bool| c.binary(can_assign)),
                Precedence::Comparison,
            ),
            t::Less => ParseRule::new(
                None,
                Some(|c, can_assign: bool| c.binary(can_assign)),
                Precedence::Comparison,
            ),
            t::LessEqual => ParseRule::new(
                None,
                Some(|c, can_assign: bool| c.binary(can_assign)),
                Precedence::Comparison,
            ),
            t::Identifier => ParseRule::new(
                Some(|c, can_assign: bool| c.variable(can_assign)),
                None,
                Precedence::None,
            ),
            t::String => ParseRule::new(
                Some(|c, _can_assign: bool| c.string()),
                None,
                Precedence::None,
            ),
            t::Number => ParseRule::new(
                Some(|c, _can_assign: bool| c.number()),
                None,
                Precedence::None,
            ),
            t::And => ParseRule::new(None, Some(|c, _can_assign| c.and()), Precedence::And),
            t::Class => ParseRule::new(None, None, Precedence::None),
            t::Else => ParseRule::new(None, None, Precedence::None),
            t::False => ParseRule::new(
                Some(|c, _can_assign: bool| c.literal()),
                None,
                Precedence::None,
            ),
            t::Fun => ParseRule::new(None, None, Precedence::None),
            t::For => ParseRule::new(None, None, Precedence::None),
            t::If => ParseRule::new(None, None, Precedence::None),
            t::Nil => ParseRule::new(
                Some(|c, _can_assign: bool| c.literal()),
                None,
                Precedence::None,
            ),
            t::Or => ParseRule::new(None, Some(|c, _can_assign| c.or()), Precedence::Or),
            t::Print => ParseRule::new(None, None, Precedence::None),
            t::Return => ParseRule::new(None, None, Precedence::None),
            t::Super => ParseRule::new(Some(|c, _can_assign| c.super_()), None, Precedence::None),
            t::This => ParseRule::new(
                Some(|c, can_assign| c.this(can_assign)),
                None,
                Precedence::None,
            ),
            t::True => ParseRule::new(
                Some(|c, _can_assign: bool| c.literal()),
                None,
                Precedence::None,
            ),
            t::Var => ParseRule::new(None, None, Precedence::None),
            t::While => ParseRule::new(None, None, Precedence::None),
            t::Error => ParseRule::new(None, None, Precedence::None),
            t::Eof => ParseRule::new(None, None, Precedence::None),
            t::Undefined => unimplemented!(),
        }
    }

    fn emit_constant(&mut self, value: Value) {
        let constant = self.make_constant(value);
        self.emit_bytes(OpCode::Constant as u8, constant);
    }

    fn make_constant(&mut self, value: Value) -> u8 {
        match self
            .state
            .last_mut()
            .unwrap()
            .function
            .chunk
            .add_constant(value)
        {
            Some(constant) => constant,
            None => {
                self.error("Too many constants in one chunk.");
                0
            }
        }
    }
    fn advance(&mut self) {
        self.parser.previous = self.parser.current.clone();

        loop {
            self.parser.current = self.scanner.scan_token();
            if self.parser.current.kind != TokenType::Error {
                break;
            }

            self.error_at_current(self.parser.current.lexeme.clone());
        }
    }

    fn consume(&mut self, kind: TokenType, message: &str) {
        if self.parser.current.kind == kind {
            self.advance();
        } else {
            self.error_at_current(message.to_owned());
        }
    }

    fn is_match(&mut self, kind: &TokenType) -> bool {
        if self.check(kind) {
            self.advance();
            true
        } else {
            false
        }
    }

    fn check(&self, kind: &TokenType) -> bool {
        &self.parser.current.kind == kind
    }

    fn error_at_current(&mut self, message: String) {
        self.error_at(&self.parser.current.clone(), message);
    }

    fn error(&mut self, message: &str) {
        self.error_at(&self.parser.previous.clone(), message.to_owned());
    }

    fn error_at(&mut self, token: &Token, message: String) {
        if self.parser.panic_mode {
            return;
        }
        self.parser.panic_mode = true;
        eprint!("[line {line}] Error", line = token.line);
        match token.kind {
            TokenType::Eof => eprint!(" at end"),
            TokenType::Error => {}
            _ => eprint!(" at '{lexeme}'", lexeme = token.lexeme),
        }
        eprintln!(": {message}");
        self.parser.had_error = true;
    }
}
