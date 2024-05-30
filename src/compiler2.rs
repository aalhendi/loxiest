use std::mem::MaybeUninit;

use crate::{
    chunk::{Chunk, Chunk2, OpCode},
    object2::{Obj2, ObjFunction},
    scanner::{self, Scanner},
    token::{Token, TokenType},
    value::{Value, Value2},
    COMPILER, COMPILING_CHUNK, CURRENT, PARSER,
};

#[derive(PartialEq, Clone)]
pub enum FunctionType {
    Function,
    Script,
    // Method,
    // Initializer,
}

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
    _Call,      // . ()
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
            9 => Precedence::_Call,
            10 => Precedence::Primary,
            _ => panic!("Unknown precedence value: {}", value),
        }
    }
}

struct ParseRule {
    prefix: Option<fn(&mut Parser, bool)>,
    infix: Option<fn(&mut Parser, bool)>,
    precedence: Precedence,
}

impl ParseRule {
    fn new(
        prefix: Option<fn(&mut Parser, bool)>,
        infix: Option<fn(&mut Parser, bool)>,
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
    scanner: Scanner,
}

impl Parser {
    pub const fn new(source: String) -> Self {
        Self {
            previous: Token::undefined(),
            current: Token::undefined(),
            had_error: false,
            panic_mode: false,
            scanner: Scanner::new(source),
        }
    }

    fn current_chunk(&mut self) -> *mut Chunk2 {
        unsafe { &mut (*(*CURRENT).function).chunk }
    }

    fn check(&self, kind: &TokenType) -> bool {
        &self.current.kind == kind
    }

    fn is_match(&mut self, kind: &TokenType) -> bool {
        if self.check(kind) {
            self.advance();
            true
        } else {
            false
        }
    }

    fn identifier_constant(&mut self, name: &Token) -> u8 {
        let chars = name.lexeme.as_bytes();
        self.make_constant(Value2::obj_val(Obj2::copy_string(chars, chars.len())))
    }

    fn define_variable(&mut self, global: u8) {
        if unsafe { (*CURRENT).scope_depth > 0 } {
            self.mark_initialized();
            return;
        }

        self.emit_bytes(OpCode::DefineGlobal, global.into());
    }

    fn mark_initialized(&mut self) {
        unsafe {
            if (*CURRENT).scope_depth == 0 {
                return;
            }
            (*CURRENT).locals[(*CURRENT).local_count - 1].depth = (*CURRENT).scope_depth;
        }
    }

    fn parse_variable(&mut self, error_msg: &str) -> u8 {
        self.consume(TokenType::Identifier, error_msg);

        self.declare_variable();
        if unsafe { (*CURRENT).scope_depth > 0 } {
            return 0;
        }

        self.identifier_constant(&self.previous.clone())
    }

    fn declare_variable(&mut self) {
        if unsafe { (*CURRENT).scope_depth == 0 } {
            return;
        }

        let name = &self.previous.clone();
        let mut i = (unsafe { (*CURRENT).local_count } as isize) - 1;
        while i >= 0 {
            let local = unsafe { &(*CURRENT).locals[i as usize] };
            if local.depth != 1 && local.depth < unsafe { (*CURRENT).scope_depth } {
                break;
            }

            if self.identifiers_equal(name, &local.name) {
                self.error("Already a variable with this name in this scope.");
            }

            i -= 1
        }

        self.add_local(name.clone());
    }

    fn identifiers_equal(&mut self, a: &Token, b: &Token) -> bool {
        a.lexeme == b.lexeme
    }

    fn resolve_local(&mut self, compiler: *mut Compiler2, name: &Token) -> isize {
        let mut i = (unsafe { (*compiler).local_count } as isize) - 1;
        while i >= 0 {
            let local = unsafe { &(*compiler).locals[i as usize] };
            if self.identifiers_equal(name, &local.name) {
                if local.depth == -1 {
                    self.error("Can't read local variable in its own initializer.");
                }
                return i;
            }
            i -= 1;
        }
        -1
    }

    fn add_local(&mut self, name: Token) {
        unsafe {
            if (*CURRENT).local_count == u8::MAX as usize + 1 {
                self.error("Too many local variables in function.");
                return;
            }

            let local = &mut (*CURRENT).locals[(*CURRENT).local_count];
            (*CURRENT).local_count += 1;

            local.name = name;
            local.depth = -1;
        }
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
            self.emit_byte(OpCode::Nil);
        }

        self.consume(
            TokenType::Semicolon,
            "Expect a ';' after variable declaration.",
        );

        self.define_variable(global);
    }

    fn declaration(&mut self) {
        if self.is_match(&TokenType::Class) {
            // self.class_declaration();
            todo!()
        } else if self.is_match(&TokenType::Fun) {
            self.fun_declaration();
        } else if self.is_match(&TokenType::Var) {
            self.var_declaration();
        } else {
            self.statement();
        }

        if self.panic_mode {
            self.synchronize();
        }
    }

    fn synchronize(&mut self) {
        self.panic_mode = false;

        while self.current.kind != TokenType::Eof {
            if self.previous.kind == TokenType::Semicolon {
                return;
            }
            match self.current.kind {
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

    fn expression_statement(&mut self) {
        self.expression();
        self.consume(TokenType::Semicolon, "Expect ';' after expression.");
        self.emit_byte(OpCode::Pop);
    }

    fn statement(&mut self) {
        if self.is_match(&TokenType::Print) {
            self.print_statement();
        } else if self.is_match(&TokenType::If) {
            self.if_statement();
        } else if self.is_match(&TokenType::While) {
            self.while_statement();
        } else if self.is_match(&TokenType::For) {
            self.for_statement();
        } else if self.is_match(&TokenType::LeftBrace) {
            self.begin_scope();
            self.block();
            self.end_scope();
        } else {
            self.expression_statement();
        }
    }

    /// Similar to if-statements, compiles condition expresion surrounded by mandatory parenthesis.
    /// If condition is falsey then jump and skip over the subsequent body statement.
    /// If truthy, execute the body statement then jump back to the loop_start before the condition.
    /// This re-evaluates the condition expression on every iteration.
    fn while_statement(&mut self) {
        let loop_start = unsafe { (*self.current_chunk()).count };
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

        let mut loop_start = unsafe { (*self.current_chunk()).count };
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
            let increment_start = unsafe { (*self.current_chunk()).count };
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

    fn emit_jump<T: Into<u8>>(&mut self, instruction: T) -> isize {
        self.emit_byte(instruction);
        self.emit_byte(0xff);
        self.emit_byte(0xff);

        unsafe { (*self.current_chunk()).count - 2 }
    }

    fn patch_jump(&mut self, offset: isize) {
        // -2 to adjust for the bytecode for the jump offset itself.
        let jump = unsafe { (*self.current_chunk()).count } - offset - 2;

        if jump > u16::MAX as isize {
            self.error("Too much code to jump over.");
        }

        unsafe {
            *(*self.current_chunk()).code.offset(offset) = ((jump >> 8) & 0xff) as u8;
            *(*self.current_chunk()).code.offset(offset + 1) = (jump & 0xff) as u8;
        }
    }

    fn print_statement(&mut self) {
        self.expression();
        self.consume(TokenType::Semicolon, "Expect ';' after value");
        self.emit_byte(OpCode::Print);
    }

    fn emit_byte<T: Into<u8>>(&mut self, byte: T) {
        unsafe { (*self.current_chunk()).write(byte.into(), self.previous.line as isize) };
    }

    fn emit_loop(&mut self, loop_start: isize) {
        self.emit_byte(OpCode::Loop);

        // +2 to adjust for bytecode for OP_LOOP offset itself
        let offset = unsafe { (*self.current_chunk()).count - loop_start + 2 };
        if offset > u16::MAX as isize {
            self.error("Loop body too large.");
        }

        self.emit_byte(((offset >> 8) & u8::MAX as isize) as u8);
        self.emit_byte((offset & u8::MAX as isize) as u8);
    }

    fn end_compiler(&mut self) -> *mut ObjFunction {
        self.emit_return();
        let function = unsafe { (*CURRENT).function };
        #[cfg(feature = "debug-print-code")]
        {
            if !self.had_error {
                unsafe {
                    if (*function).name.is_null() {
                        (*self.current_chunk()).disassemble("<script>");
                    } else {
                        let name = &(*(*function).name);
                        (*self.current_chunk()).disassemble(name);
                    }
                }
            }
        }

        unsafe {
            CURRENT = (*CURRENT).enclosing;
        }
        function
    }

    fn emit_return(&mut self) {
        self.emit_byte(OpCode::Return);
    }

    fn emit_bytes<T: Into<u8>>(&mut self, byte1: T, byte2: T) {
        self.emit_byte(byte1.into());
        self.emit_byte(byte2.into());
    }

    fn expression(&mut self) {
        self.parse_precedence(Precedence::Assignment);
    }

    fn number(&mut self) {
        let value = self
            .previous
            .lexeme
            .parse::<f64>()
            .expect("Unable to parse number");
        self.emit_constant(Value2::number_val(value));
    }

    fn begin_scope(&mut self) {
        unsafe { (*CURRENT).scope_depth += 1 };
    }

    fn end_scope(&mut self) {
        unsafe {
            (*CURRENT).scope_depth -= 1;

            while (*CURRENT).local_count > 0
                && (*CURRENT).locals[(*CURRENT).local_count - 1].depth > (*CURRENT).scope_depth
            {
                self.emit_byte(OpCode::Pop); // TODO(aalhendi): OpCode::Pop_n
                (*CURRENT).local_count -= 1;
            }
        }
    }

    fn block(&mut self) {
        while !self.check(&TokenType::RightBrace) && !self.check(&TokenType::Eof) {
            self.declaration();
        }
        self.consume(TokenType::RightBrace, "Expect '}' after block.");
    }

    fn function(&mut self, function_type: FunctionType) {
        let mut compiler = Compiler2::new_uninit();
        compiler.init(function_type);
        self.begin_scope();

        self.consume(TokenType::LeftParen, "Expect '(' after function name.");
        if !self.check(&TokenType::RightParen) {
            loop {
                unsafe {
                    (*(*CURRENT).function).arity += 1;
                    if (*(*CURRENT).function).arity > u8::MAX as isize {
                        self.error_at_current("Can't have more than 255 parameters.".to_owned());
                    }
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
        let val = Value2::obj_val(function);
        let constant = self.make_constant(val);
        self.emit_bytes(OpCode::Constant, constant.into());
    }

    fn grouping(&mut self) {
        self.expression();
        self.consume(TokenType::RightParen, "Expect ')' after expression.")
    }

    fn unary(&mut self) {
        let operator_type = &self.previous.kind.clone();
        self.parse_precedence(Precedence::Unary);

        match operator_type {
            TokenType::Bang => self.emit_byte(OpCode::Not),
            TokenType::Minus => self.emit_byte(OpCode::Negate),
            _ => unreachable!(),
        }
    }

    fn literal(&mut self) {
        match self.previous.kind {
            TokenType::False => self.emit_byte(OpCode::False),
            TokenType::True => self.emit_byte(OpCode::True),
            TokenType::Nil => self.emit_byte(OpCode::Nil),
            _ => unreachable!(),
        }
    }

    fn string(&mut self) {
        let chars = self.previous.lexeme.as_bytes();
        self.emit_constant(Value2::obj_val(Obj2::copy_string(
            &chars[1..chars.len() - 1],
            self.previous.lexeme.len() - 2,
        )))
    }

    fn variable(&mut self, can_assign: bool) {
        let name = &self.previous.clone();
        self.named_variable(name, can_assign);
    }

    fn named_variable(&mut self, name: &Token, can_assign: bool) {
        let mut arg = self.resolve_local(unsafe { CURRENT }, name);

        let (get_op, set_op) = if arg != -1 {
            (OpCode::GetLocal, OpCode::SetLocal)
        } else {
            arg = self.identifier_constant(name) as isize;
            (OpCode::GetGlobal, OpCode::SetGlobal)
        };

        if can_assign && self.is_match(&TokenType::Equal) {
            self.expression();
            self.emit_bytes(set_op, (arg as u8).into());
        } else {
            self.emit_bytes(get_op, (arg as u8).into());
        }
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

    // NOTE(aalhendi): According to IEEE 754, all comparison operators return false when an operand is NaN./
    // That means NaN <= 1 is false and NaN > 1 is also false.
    // But our desugaring assumes the latter is always the negation of the former.
    // TODO(aalhendi): Create instructions for (!=, <=, and >=). VM would execute faster if we did.
    fn binary(&mut self, can_assign: bool) {
        let operator_kind = &self.previous.kind.clone();
        let rule = self.get_rule(operator_kind, can_assign);

        let next = self.next_precedence(rule.precedence);
        self.parse_precedence(next);

        match operator_kind {
            TokenType::Plus => self.emit_byte(OpCode::Add),
            TokenType::Minus => self.emit_byte(OpCode::Subtract),
            TokenType::Star => self.emit_byte(OpCode::Multiply),
            TokenType::Slash => self.emit_byte(OpCode::Divide),
            TokenType::BangEqual => self.emit_bytes(OpCode::Equal, OpCode::Not),
            TokenType::EqualEqual => self.emit_byte(OpCode::Equal),
            TokenType::Greater => self.emit_byte(OpCode::Greater),
            TokenType::GreaterEqual => self.emit_bytes(OpCode::Less as u8, OpCode::Not as u8),
            TokenType::Less => self.emit_byte(OpCode::Less),
            TokenType::LessEqual => self.emit_bytes(OpCode::Greater, OpCode::Not),
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
        match self.get_rule(&self.previous.kind, can_assign).prefix {
            Some(prefix_rule) => {
                prefix_rule(self, can_assign);

                while precedence <= self.get_rule(&self.current.kind, can_assign).precedence {
                    self.advance();
                    if let Some(infix_rule) = self.get_rule(&self.previous.kind, can_assign).infix {
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

    fn get_rule(&self, kind: &TokenType, _can_assign: bool) -> ParseRule {
        // TODO: Make a fixed array called rules. Once cell it or something.
        // TODO: Kill this function once array is used. Just index the array
        use TokenType::*;
        match kind {
            LeftParen => {
                ParseRule::new(Some(|c, _can_assign| c.grouping()), None, Precedence::None)
            }
            RightParen => ParseRule::new(None, None, Precedence::None),
            LeftBrace => ParseRule::new(None, None, Precedence::None),
            RightBrace => ParseRule::new(None, None, Precedence::None),
            Comma => ParseRule::new(None, None, Precedence::None),
            Dot => ParseRule::new(None, None, Precedence::None),
            Minus => ParseRule::new(
                Some(|c, _can_assign| c.unary()),
                Some(|c, can_assign| c.binary(can_assign)),
                Precedence::Term,
            ),
            Plus => ParseRule::new(
                None,
                Some(|c, can_assign: bool| c.binary(can_assign)),
                Precedence::Term,
            ),
            Semicolon => ParseRule::new(None, None, Precedence::None),
            Slash => ParseRule::new(
                None,
                Some(|c, can_assign| c.binary(can_assign)),
                Precedence::Factor,
            ),
            Star => ParseRule::new(
                None,
                Some(|c, can_assign| c.binary(can_assign)),
                Precedence::Factor,
            ),
            Bang => ParseRule::new(Some(|c, _can_assign| c.unary()), None, Precedence::None),
            BangEqual => ParseRule::new(
                None,
                Some(|c, can_assign| c.binary(can_assign)),
                Precedence::Equality,
            ),
            Equal => ParseRule::new(None, None, Precedence::None),
            EqualEqual => ParseRule::new(
                None,
                Some(|c, can_assign| c.binary(can_assign)),
                Precedence::Equality,
            ),
            Greater => ParseRule::new(
                None,
                Some(|c, can_assign| c.binary(can_assign)),
                Precedence::Comparison,
            ),
            GreaterEqual => ParseRule::new(
                None,
                Some(|c, can_assign| c.binary(can_assign)),
                Precedence::Comparison,
            ),
            Less => ParseRule::new(
                None,
                Some(|c, can_assign| c.binary(can_assign)),
                Precedence::Comparison,
            ),
            LessEqual => ParseRule::new(
                None,
                Some(|c, can_assign| c.binary(can_assign)),
                Precedence::Comparison,
            ),
            Identifier => ParseRule::new(
                Some(|c, can_assign| c.variable(can_assign)),
                None,
                Precedence::None,
            ),
            String => ParseRule::new(Some(|c, _can_assign| c.string()), None, Precedence::None),
            Number => ParseRule::new(Some(|c, _can_assign| c.number()), None, Precedence::None),
            And => ParseRule::new(None, Some(|c, _can_assign| c.and()), Precedence::And),
            Class => ParseRule::new(None, None, Precedence::None),
            Else => ParseRule::new(None, None, Precedence::None),
            False => ParseRule::new(Some(|c, _can_assign| c.literal()), None, Precedence::None),
            Fun => ParseRule::new(None, None, Precedence::None),
            For => ParseRule::new(None, None, Precedence::None),
            If => ParseRule::new(None, None, Precedence::None),
            Nil => ParseRule::new(Some(|c, _can_assign| c.literal()), None, Precedence::None),
            Or => ParseRule::new(None, Some(|c, _can_assign| c.or()), Precedence::Or),
            Print => ParseRule::new(None, None, Precedence::None),
            Return => ParseRule::new(None, None, Precedence::None),
            Super => ParseRule::new(None, None, Precedence::None),
            This => ParseRule::new(None, None, Precedence::None),
            True => ParseRule::new(Some(|c, _can_assign| c.literal()), None, Precedence::None),
            Var => ParseRule::new(None, None, Precedence::None),
            While => ParseRule::new(None, None, Precedence::None),
            Error => ParseRule::new(None, None, Precedence::None),
            Eof => ParseRule::new(None, None, Precedence::None),
            Undefined => unimplemented!(),
        }
    }

    fn emit_constant(&mut self, value: Value2) {
        let constant = self.make_constant(value);
        self.emit_bytes(OpCode::Constant as u8, constant);
    }

    fn make_constant(&mut self, value: Value2) -> u8 {
        let constant = unsafe { (*self.current_chunk()).add_constant(value) };
        if constant > u8::MAX.into() {
            self.error("Too many constants in one chunk.");
            return 0;
        }
        constant as u8
    }
    fn advance(&mut self) {
        self.previous = self.current.clone();

        loop {
            self.current = self.scanner.scan_token();
            if self.current.kind != TokenType::Error {
                break;
            }

            self.error_at_current(self.current.lexeme.clone());
        }
    }

    fn consume(&mut self, kind: TokenType, message: &str) {
        if self.current.kind == kind {
            self.advance();
        } else {
            self.error_at_current(message.to_owned());
        }
    }

    fn error_at_current(&mut self, message: String) {
        self.error_at(&self.current.clone(), message);
    }

    fn error(&mut self, message: &str) {
        self.error_at(&self.previous.clone(), message.to_owned());
    }

    fn error_at(&mut self, token: &Token, message: String) {
        if self.panic_mode {
            return;
        }
        self.panic_mode = true;
        eprint!("[line {line}] Error", line = token.line);
        match token.kind {
            TokenType::Eof => eprint!(" at end"),
            TokenType::Error => {}
            _ => eprint!(" at '{lexeme}'", lexeme = token.lexeme),
        }
        eprintln!(": {message}");
        self.had_error = true;
    }
}

pub struct Compiler2 {
    enclosing: *mut Compiler2,
    function: *mut ObjFunction,
    function_type: FunctionType,

    pub locals: [Local; u8::MAX as usize + 1],
    pub local_count: usize,
    pub scope_depth: isize,
}

pub struct Local {
    name: Token,
    depth: isize,
}

pub fn compile(source: String) -> *mut ObjFunction {
    unsafe {
        PARSER = Parser::new(source);
        COMPILER.init(FunctionType::Script);

        PARSER.advance();
        while !PARSER.is_match(&TokenType::Eof) {
            PARSER.declaration();
        }

        let function = PARSER.end_compiler();
        if PARSER.had_error {
            std::ptr::null_mut()
        } else {
            function
        }
    }
}

impl Compiler2 {
    pub const fn new_uninit() -> Self {
        Self {
            locals: {
                const DEFAULT: Local = Local {
                    name: Token::undefined(),
                    depth: 0,
                };
                [DEFAULT; 256]
            },
            local_count: 0,
            scope_depth: 0,
            function: std::ptr::null_mut(),
            function_type: unsafe { std::mem::zeroed() },
            enclosing: std::ptr::null_mut(),
        }
    }

    pub fn init(&mut self, function_type: FunctionType) {
        let is_script = function_type == FunctionType::Script;
        self.enclosing = unsafe { CURRENT };
        self.function = std::ptr::null_mut();
        self.function_type = function_type;
        self.local_count = 0;
        self.scope_depth = 0;
        self.function = ObjFunction::new();
        unsafe {
            CURRENT = &mut *self;
            if !is_script {
                let chars = PARSER.previous.lexeme.as_bytes();
                (*(*CURRENT).function).name = Obj2::copy_string(chars, chars.len());
            }

            // compiler implicitly claims stack slot zero for the VM’s own internal use
            let local = &mut (*CURRENT).locals[(*CURRENT).local_count];
            (*CURRENT).local_count += 1;
            local.depth = 0;
            local.name.lexeme = String::new();
        }
    }
}
