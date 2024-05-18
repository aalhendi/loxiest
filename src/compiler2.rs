use crate::{
    chunk::{Chunk, Chunk2, OpCode},
    object2::Obj2,
    scanner::Scanner,
    token::{Token, TokenType},
    value::{Value, Value2},
};

#[derive(Debug, PartialEq, PartialOrd)]
#[repr(u8)]
enum Precedence {
    None,
    Assignment, // =
    _Or,        // or
    _And,       // and
    Equality,   // == !=
    Comparison, // < > <= >=
    Term,       // + -
    Factor,     // * /
    Unary,      // ! -
    _Call,      // . ()
    Primary,
}

struct ParseRule {
    prefix: Option<fn(&mut Compiler2)>,
    infix: Option<fn(&mut Compiler2)>,
    precedence: Precedence,
}

impl ParseRule {
    fn new(
        prefix: Option<fn(&mut Compiler2)>,
        infix: Option<fn(&mut Compiler2)>,
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
            previous: Token::new(TokenType::Undefined, "", 0),
            current: Token::new(TokenType::Undefined, "", 0),
            had_error: false,
            panic_mode: false,
        }
    }
}

pub struct Compiler2<'a> {
    parser: Parser,
    compiling_chunk: *mut Chunk2,
    scanner: Scanner<'a>,
}

impl<'a> Compiler2<'a> {
    pub fn new(source: &'a str, chunk: *mut Chunk2) -> Self {
        Self {
            parser: Parser::new(),
            compiling_chunk: chunk,
            scanner: Scanner::new(source),
        }
    }

    fn current_chunk(&mut self) -> *mut Chunk2 {
        self.compiling_chunk
    }

    pub fn compile(&mut self) -> bool {
        self.advance();
        self.expression();
        self.consume(TokenType::Eof, "Expect end of expression.");
        self.end_compiler();
        !self.parser.had_error
    }

    fn emit_byte<T: Into<u8>>(&mut self, byte: T) {
        unsafe { (*self.current_chunk()).write(byte.into(), self.parser.previous.line as isize) };
    }

    fn end_compiler(&mut self) {
        self.emit_return();
        #[cfg(feature = "debug-print-code")]
        {
            if !self.parser.had_error {
                unsafe { (*self.current_chunk()).disassemble("code") };
            }
        }
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
            .parser
            .previous
            .lexeme
            .parse::<f64>()
            .expect("Unable to parse number");
        self.emit_constant(Value2::number_val(value));
    }

    fn grouping(&mut self) {
        self.expression();
        self.consume(TokenType::RightParen, "Expect ')' after expression.")
    }

    fn unary(&mut self) {
        let operator_type = &self.parser.previous.kind.clone();
        self.parse_precedence(Precedence::Unary);

        match operator_type {
            TokenType::Bang => self.emit_byte(OpCode::Not),
            TokenType::Minus => self.emit_byte(OpCode::Negate),
            _ => unreachable!(),
        }
    }

    fn literal(&mut self) {
        match self.parser.previous.kind {
            TokenType::False => self.emit_byte(OpCode::False),
            TokenType::True => self.emit_byte(OpCode::True),
            TokenType::Nil => self.emit_byte(OpCode::Nil),
            _ => unreachable!(),
        }
    }

    fn string(&mut self) {
        let chars = self.parser.previous.lexeme.as_bytes();
        self.emit_constant(Value2::obj_val(Obj2::copy_string(
            &chars[1..chars.len() - 1],
            self.parser.previous.lexeme.len() - 2,
        )))
    }

    // NOTE(aalhendi): According to IEEE 754, all comparison operators return false when an operand is NaN./
    // That means NaN <= 1 is false and NaN > 1 is also false.
    // But our desugaring assumes the latter is always the negation of the former.
    // TODO(aalhendi): Create instructions for (!=, <=, and >=). VM would execute faster if we did.
    fn binary(&mut self) {
        let operator_kind = &self.parser.previous.kind.clone();
        let rule = self.get_rule(operator_kind);

        let next = unsafe { self.next_precedence(rule.precedence) };
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

    /// .
    ///
    /// # Safety: Should be ok...
    ///
    /// TODO: ...
    /// .
    unsafe fn next_precedence(&self, precedence: Precedence) -> Precedence {
        // TODO: Remove? or keep?
        if precedence == Precedence::Primary {
            panic!("Has no next.");
        }
        std::mem::transmute(precedence as u8 + 1)
    }

    fn parse_precedence(&mut self, precedence: Precedence) {
        self.advance();
        match self.get_rule(&self.parser.previous.kind).prefix {
            Some(prefix_rule) => {
                prefix_rule(self);

                while precedence <= self.get_rule(&self.parser.current.kind).precedence {
                    self.advance();
                    if let Some(infix_rule) = self.get_rule(&self.parser.previous.kind).infix {
                        infix_rule(self);
                    }
                }
            }
            None => self.error("Expect Expression."),
        }
    }

    fn get_rule(&self, kind: &TokenType) -> ParseRule {
        // TODO: Make a fixed array called rules. Once cell it or something.
        // TODO: Kill this function once array is used. Just index the array
        use TokenType::*;
        match kind {
            LeftParen => ParseRule::new(Some(|c| c.grouping()), None, Precedence::None),
            RightParen => ParseRule::new(None, None, Precedence::None),
            LeftBrace => ParseRule::new(None, None, Precedence::None),
            RightBrace => ParseRule::new(None, None, Precedence::None),
            Comma => ParseRule::new(None, None, Precedence::None),
            Dot => ParseRule::new(None, None, Precedence::None),
            Minus => ParseRule::new(Some(|c| c.unary()), Some(|c| c.binary()), Precedence::Term),
            Plus => ParseRule::new(None, Some(|c| c.binary()), Precedence::Term),
            Semicolon => ParseRule::new(None, None, Precedence::None),
            Slash => ParseRule::new(None, Some(|c| c.binary()), Precedence::Factor),
            Star => ParseRule::new(None, Some(|c| c.binary()), Precedence::Factor),
            Bang => ParseRule::new(Some(|c| c.unary()), None, Precedence::None),
            BangEqual => ParseRule::new(None, Some(|c| c.binary()), Precedence::Equality),
            Equal => ParseRule::new(None, None, Precedence::None),
            EqualEqual => ParseRule::new(None, Some(|c| c.binary()), Precedence::Equality),
            Greater => ParseRule::new(None, Some(|c| c.binary()), Precedence::Comparison),
            GreaterEqual => ParseRule::new(None, Some(|c| c.binary()), Precedence::Comparison),
            Less => ParseRule::new(None, Some(|c| c.binary()), Precedence::Comparison),
            LessEqual => ParseRule::new(None, Some(|c| c.binary()), Precedence::Comparison),
            Identifier => ParseRule::new(None, None, Precedence::None),
            String => ParseRule::new(Some(|c| c.string()), None, Precedence::None),
            Number => ParseRule::new(Some(|c| c.number()), None, Precedence::None),
            And => ParseRule::new(None, None, Precedence::None),
            Class => ParseRule::new(None, None, Precedence::None),
            Else => ParseRule::new(None, None, Precedence::None),
            False => ParseRule::new(Some(|c| c.literal()), None, Precedence::None),
            Fun => ParseRule::new(None, None, Precedence::None),
            For => ParseRule::new(None, None, Precedence::None),
            If => ParseRule::new(None, None, Precedence::None),
            Nil => ParseRule::new(Some(|c| c.literal()), None, Precedence::None),
            Or => ParseRule::new(None, None, Precedence::None),
            Print => ParseRule::new(None, None, Precedence::None),
            Return => ParseRule::new(None, None, Precedence::None),
            Super => ParseRule::new(None, None, Precedence::None),
            This => ParseRule::new(None, None, Precedence::None),
            True => ParseRule::new(Some(|c| c.literal()), None, Precedence::None),
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
