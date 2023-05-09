use std::fmt::Display;

#[derive(Debug, PartialEq, Eq)]
pub enum TokenKind<'a> {
    Illegal,
    Eof,

    Ident(&'a str),
    Int(usize),

    Assign,
    Plus,
    Minus,
    Bang,
    Asterisk,
    Slash,

    If,
    Else,
    Equal,
    NotEqual,
    False,
    True,
    Return,

    LessThan,
    GreaterThan,

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
        // TODO: use an hashmap
        match literal {
            "fn" => TokenKind::Function,
            "let" => TokenKind::Let,
            "true" => TokenKind::True,
            "false" => TokenKind::False,
            "if" => TokenKind::If,
            "else" => TokenKind::Else,
            "return" => TokenKind::Return,
            _ => TokenKind::Ident(literal),
        }
    }
}

impl<'a> Display for TokenKind<'a> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let matched = match self {
            TokenKind::Illegal => "ILLEGAL",
            TokenKind::Eof => "EOF",
            TokenKind::Ident(_) => "IDENT",
            TokenKind::Int(_) => "INT",
            TokenKind::Assign => "=",
            TokenKind::Plus => "+",
            TokenKind::Comma => ",",
            TokenKind::Semicolon => ";",
            TokenKind::Lparen => "(",
            TokenKind::Rparen => ")",
            TokenKind::Lbrace => "{",
            TokenKind::Rbrace => "}",
            TokenKind::Function => "FUNCTION",
            TokenKind::Let => "LET",
            TokenKind::Minus => "-",
            TokenKind::Bang => "!",
            TokenKind::Asterisk => "*",
            TokenKind::Slash => "/",
            TokenKind::LessThan => "<",
            TokenKind::GreaterThan => ">",
            TokenKind::Equal => "==",
            TokenKind::NotEqual => "!=",
            TokenKind::False => "FALSE",
            TokenKind::True => "TRUE",
            TokenKind::Return => "RETURN",
            TokenKind::If => "IF",
            TokenKind::Else => "ELSE",
        };
        write!(f, "{matched}")
    }
}

// NOTE: right now Token is a bit useless because it as all the same info as
// TokenKind, but I don't know where the book is going so I don't want to remove
// it yet.
#[derive(Debug)]
pub struct Token<'a> {
    pub kind: TokenKind<'a>,
}

impl<'a> Token<'a> {
    pub fn new(kind: TokenKind<'a>) -> Self {
        Self { kind }
    }
}
