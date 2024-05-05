use crate::token::{Token, TokenType};

pub struct Scanner<'a> {
    source: &'a str,
    start: usize,
    current: usize,
    line: usize,
}

impl<'a> Scanner<'a> {
    pub fn new(source: &'a str) -> Self {
        Self {
            source,
            start: 0,
            current: 0,
            line: 1,
        }
    }

    pub fn scan_token(&mut self) -> Token {
        self.skip_whitespace();

        self.start = self.current;

        if self.is_at_end() {
            return self.make_token(TokenType::Eof);
        }

        let c = self.advance();
        match c {
            '(' => self.make_token(TokenType::LeftParen),
            ')' => self.make_token(TokenType::RightParen),
            '{' => self.make_token(TokenType::LeftBrace),
            '}' => self.make_token(TokenType::RightBrace),
            ';' => self.make_token(TokenType::Semicolon),
            ',' => self.make_token(TokenType::Comma),
            '.' => self.make_token(TokenType::Dot),
            '-' => self.make_token(TokenType::Minus),
            '+' => self.make_token(TokenType::Plus),
            '*' => self.make_token(TokenType::Star),
            '/' => self.make_token(TokenType::Slash),

            '!' => {
                let kind = if self.match_peek('=') {
                    TokenType::BangEqual
                } else {
                    TokenType::Bang
                };
                self.make_token(kind)
            }
            '=' => {
                let kind = if self.match_peek('=') {
                    TokenType::EqualEqual
                } else {
                    TokenType::Equal
                };
                self.make_token(kind)
            }
            '<' => {
                let kind = if self.match_peek('=') {
                    TokenType::LessEqual
                } else {
                    TokenType::Less
                };
                self.make_token(kind)
            }
            '>' => {
                let kind = if self.match_peek('=') {
                    TokenType::GreaterEqual
                } else {
                    TokenType::Greater
                };
                self.make_token(kind)
            }

            '"' => self.string(),
            _ if c.is_ascii_digit() => self.number(),
            _ if c.is_ascii_alphabetic() || c == '_' => self.identifier(),

            _ => self.error_token("Unexpected character."),
        }
    }

    fn string(&mut self) -> Token {
        // TODO: Support escaping quotes?
        while self.peek() != '"' && !self.is_at_end() {
            if self.peek() == '\n' {
                self.line += 1;
            }
            self.advance();
        }

        if self.is_at_end() {
            self.error_token("Unterminated string.")
        } else {
            self.advance();
            self.make_token(TokenType::String)
        }
    }

    fn number(&mut self) -> Token {
        while self.peek().is_ascii_digit() {
            self.advance();
        }

        if self.peek() == '.' && self.peek_next().is_some_and(|c| c.is_ascii_digit()) {
            self.advance();
        }

        while self.peek().is_ascii_digit() {
            self.advance();
        }

        self.make_token(TokenType::Number)
    }

    fn identifier(&mut self) -> Token {
        while self.peek().is_ascii_alphanumeric() || self.peek() == '_' {
            self.advance();
        }
        self.make_token(self.identifier_type())
    }

    fn is_at_end(&self) -> bool {
        self.current == self.source.len()
    }

    fn make_token(&self, kind: TokenType) -> Token {
        Token::new(kind, &self.source[self.start..self.current], self.line)
    }

    fn identifier_type(&self) -> TokenType {
        match self.source.as_bytes()[self.start] as char {
            'a' => self.check_keyword(1, 2, "nd", TokenType::And),
            'c' => self.check_keyword(1, 4, "lass", TokenType::Class),
            'e' => self.check_keyword(1, 3, "lse", TokenType::Else),
            'f' => {
                if self.current - self.start > 1 {
                    match self.source.as_bytes()[self.start + 1] as char {
                        'a' => self.check_keyword(2, 3, "lse", TokenType::False),
                        'o' => self.check_keyword(2, 1, "r", TokenType::For),
                        'u' => self.check_keyword(2, 1, "n", TokenType::Fun),
                        _ => TokenType::Identifier,
                    }
                } else {
                    TokenType::Identifier
                }
            }
            'i' => self.check_keyword(1, 1, "f", TokenType::If),
            'n' => self.check_keyword(1, 2, "il", TokenType::Nil),
            'o' => self.check_keyword(1, 1, "r", TokenType::Or),
            'p' => self.check_keyword(1, 4, "rint", TokenType::Print),
            'r' => self.check_keyword(1, 5, "eturn", TokenType::Return),
            's' => self.check_keyword(1, 4, "uper", TokenType::Super),
            't' => {
                if self.current - self.start > 1 {
                    match self.source.as_bytes()[self.start + 1] as char {
                        'h' => self.check_keyword(2, 2, "is", TokenType::This),
                        'r' => self.check_keyword(2, 2, "ue", TokenType::True),
                        _ => TokenType::Identifier,
                    }
                } else {
                    TokenType::Identifier
                }
            }
            'v' => self.check_keyword(1, 2, "ar", TokenType::Var),
            'w' => self.check_keyword(1, 3, "hile", TokenType::While),
            _ => TokenType::Identifier,
        }
    }

    fn error_token(&self, message: &'a str) -> Token {
        Token::new(TokenType::Error, message, self.line)
    }

    fn advance(&mut self) -> char {
        self.current += 1;
        // TODO: This limits us to u8
        self.source.as_bytes()[self.current - 1] as char
    }

    fn match_peek(&mut self, expected: char) -> bool {
        if self.is_at_end() || self.source.as_bytes()[self.current] as char != expected {
            false
        } else {
            self.current += 1;
            true
        }
    }

    fn skip_whitespace(&mut self) {
        loop {
            match self.peek() {
                ' ' | '\r' | '\t' => {
                    self.advance();
                }
                '\n' => {
                    self.line += 1;
                    self.advance();
                }
                '/' => {
                    if let Some('/') = self.peek_next() {
                        while self.peek() != '\n' && !self.is_at_end() {
                            self.advance();
                        }
                    } else {
                        return;
                    }
                }
                _ => return,
            }
        }
    }

    fn check_keyword(
        &self,
        start: usize,
        _length: usize,
        rest: &str,
        kind: TokenType,
    ) -> TokenType {
        // No need to compare lengths because we have string slices.. which cmp length anyway
        // self.current - self.start != start + length
        if &self.source[self.start + start..self.current] == rest {
            kind
        } else {
            TokenType::Identifier
        }
    }

    fn peek(&self) -> char {
        if self.is_at_end() {
            return '\0';
        }
        self.source.as_bytes()[self.current] as char
    }

    fn peek_next(&self) -> Option<char> {
        if self.current + 1 >= self.source.len() {
            return None;
        }
        Some(self.source.as_bytes()[self.current + 1] as char)
    }
}
