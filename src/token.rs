use std::fmt::Display;

enum TokenKind {
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

struct Token {
    kind: TokenKind,
}
