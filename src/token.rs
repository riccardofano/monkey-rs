use std::{fmt::Display, str::from_utf8};

#[derive(PartialEq, Eq)]
pub enum TokenKind {
    Illegal,
    Eof,

    Ident(String),
    Int(usize),

    Assign,
    Plus,

    Comma,
    Semicolon,

    Lparen,
    Rparen,
    Lbrace,
    Rbrace,

    Function,
    Let,
}

impl Display for TokenKind {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let matched = match self {
            Self::Illegal => "ILLEGAL",
            Self::Eof => "EOF",
            Self::Ident(_) => "IDENT",
            Self::Int(_) => "INT",
            Self::Assign => "=",
            Self::Plus => "+",
            Self::Comma => ",",
            Self::Semicolon => ";",
            Self::Lparen => "(",
            Self::Rparen => ")",
            Self::Lbrace => "{",
            Self::Rbrace => "}",
            Self::Function => "FUNCTION",
            Self::Let => "LET",
        };
        write!(f, "{matched}")
    }
}

pub struct Token {
    pub kind: TokenKind,
    pub literal: String,
}

impl Token {
    pub fn new(kind: TokenKind, byte: u8) -> Self {
        Self {
            kind,
            literal: byte.escape_ascii().to_string(),
        }
    }
}
