use std::fmt::Display;

#[derive(Debug, Clone, PartialEq)]
pub struct Token {
    pub kind: TokenType,
    pub lexeme: &'static str,
    pub line: i32,
}

impl Token {
    pub fn new(kind: TokenType, lexeme: &'static str, line: i32) -> Token {
        Token { kind, lexeme, line }
    }

    pub const fn undefined() -> Self {
        Self {
            kind: TokenType::Undefined,
            lexeme: "",
            line: 0,
        }
    }
}

impl Display for Token {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "{token_type:?} {lexeme}",
            token_type = self.kind,
            lexeme = self.lexeme,
        )
    }
}

#[derive(Debug, PartialEq, Clone, Copy)]
#[repr(C)]
pub enum TokenType {
    // --- Single-character tokens. ---
    LeftParen,
    RightParen,
    LeftBrace,
    RightBrace,
    Comma,
    // Colon,
    Dot,
    Minus,
    Plus,
    // QuestionMark,
    Semicolon,
    Slash,
    Star,
    // --- One or two character tokens. ---
    Bang,
    BangEqual,
    Equal,
    EqualEqual,
    Greater,
    GreaterEqual,
    Less,
    LessEqual,
    // --- Literals. ---
    Identifier,
    String,
    Number,
    // --- Keywords. ---
    And,
    Class,
    Else,
    False,
    Fun,
    For,
    If,
    Nil,
    Or,
    Print,
    Return,
    Super,
    This,
    True,
    Var,
    While,
    // --- Other. ---
    Error,
    Eof,
    Undefined,
}

impl From<TokenType> for usize {
    fn from(token: TokenType) -> Self {
        // Safety: TokenType is #[repr(C)]. We know its memory layout and count
        unsafe { std::mem::transmute::<TokenType, u32>(token) as usize }
    }
}
