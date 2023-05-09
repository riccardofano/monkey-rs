use std::{fmt::Display, str::FromStr};

#[derive(PartialEq, Eq)]
pub enum TokenKind<'a> {
    Illegal,
    Eof,

    Ident(&'a str),
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

impl<'a> TokenKind<'a> {
    pub fn from_letters(literal: &'a str) -> Self {
        match literal {
            "fn" => Self::Function,
            "let" => Self::Let,
            _ => Self::Ident(literal),
        }
    }
}

impl<'a> Display for TokenKind<'a> {
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

// NOTE: right now Token is a bit useless because it as all the same info as
// TokenKind, but I don't know where the book is going so I don't want to remove
// it yet.
pub struct Token<'a> {
    pub kind: TokenKind<'a>,
}

impl<'a> Token<'a> {
    pub fn new(kind: TokenKind<'a>) -> Self {
        Self { kind }
    }
}
