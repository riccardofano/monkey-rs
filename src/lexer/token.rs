use std::fmt::Display;

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord)]
pub enum TokenKind {
    Illegal,
    Eof,

    Ident(String),
    Int(i64),
    String(String),

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
    Colon,
    Semicolon,

    Lparen,
    Rparen,
    Lbracket,
    Rbracket,
    Lbrace,
    Rbrace,

    Function,
    Let,
}

impl TokenKind {
    pub fn from_letters(literal: &str) -> Self {
        match literal {
            "fn" => TokenKind::Function,
            "let" => TokenKind::Let,
            "true" => TokenKind::True,
            "false" => TokenKind::False,
            "if" => TokenKind::If,
            "else" => TokenKind::Else,
            "return" => TokenKind::Return,
            _ => TokenKind::Ident(literal.to_string()),
        }
    }
}

impl Display for TokenKind {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let matched = match self {
            TokenKind::Illegal => "ILLEGAL",
            TokenKind::Eof => "EOF",
            TokenKind::Ident(_) => "IDENT",
            TokenKind::Int(_) => "INT",
            TokenKind::String(_) => "STRING",
            TokenKind::Assign => "=",
            TokenKind::Plus => "+",
            TokenKind::Comma => ",",
            TokenKind::Colon => ":",
            TokenKind::Semicolon => ";",
            TokenKind::Lparen => "(",
            TokenKind::Rparen => ")",
            TokenKind::Lbracket => "[",
            TokenKind::Rbracket => "]",
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

#[derive(Debug)]
pub struct Token {
    pub kind: TokenKind,
}

impl Token {
    pub fn new(kind: TokenKind) -> Self {
        Self { kind }
    }
}
