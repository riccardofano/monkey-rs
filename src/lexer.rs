pub mod token;

use token::{Token, TokenKind};

#[derive(Debug)]
pub struct Lexer {
    input: String,
    position: usize,
    read_position: usize,
    character: u8,
}

impl Lexer {
    pub fn new(input: &str) -> Self {
        let mut lexer = Self {
            input: input.to_string(),
            position: 0,
            read_position: 0,
            character: 0,
        };
        lexer.read_char();
        lexer
    }

    fn read_char(&mut self) {
        if self.read_position >= self.input.len() {
            self.character = 0;
        } else {
            self.character = self.input.as_bytes()[self.read_position];
        }
        self.position = self.read_position;
        self.read_position += 1;
    }

    fn peek_char(&self) -> u8 {
        if self.read_position >= self.input.len() {
            return 0;
        }
        return self.input.as_bytes()[self.read_position];
    }

    fn if_peeked(&mut self, to_match: u8, matched: TokenKind, default: TokenKind) -> TokenKind {
        if self.peek_char() == to_match {
            self.read_char();
            return matched;
        }
        default
    }

    fn read_identifier(&mut self) -> &str {
        let start = self.position;
        while is_letter(self.character) {
            self.read_char();
        }
        &self.input[start..self.position]
    }

    fn read_number(&mut self) -> i64 {
        let start = self.position;
        while is_number(self.character) {
            self.read_char();
        }
        let number = &self.input[start..self.position];
        number.parse().unwrap()
    }

    fn read_string(&mut self) -> String {
        let position = self.position + 1;
        loop {
            self.read_char();
            if self.character == b'"' || self.character == 0 {
                break;
            }
        }
        self.input[position..self.position].to_string()
    }

    pub fn next_token(&mut self) -> Token {
        use TokenKind::*;

        self.skip_whitespace();

        let token_kind = match self.character {
            0 => Eof,
            b'+' => Plus,
            b'-' => Minus,
            b'*' => Asterisk,
            b'/' => Slash,
            b'<' => LessThan,
            b'>' => GreaterThan,
            b',' => Comma,
            b':' => Colon,
            b';' => Semicolon,
            b'(' => Lparen,
            b')' => Rparen,
            b'[' => Lbracket,
            b']' => Rbracket,
            b'{' => Lbrace,
            b'}' => Rbrace,
            b'"' => String(self.read_string()),
            b'=' => self.if_peeked(b'=', Equal, Assign),
            b'!' => self.if_peeked(b'=', NotEqual, Bang),
            c if is_letter(c) => {
                let literal = self.read_identifier();
                let kind = TokenKind::from_letters(literal);
                return Token::new(kind);
            }
            c if is_number(c) => {
                let number = self.read_number();
                return Token::new(Int(number));
            }
            _ => return Token::new(Illegal),
        };

        self.read_char();
        Token::new(token_kind)
    }

    fn skip_whitespace(&mut self) {
        while self.character.is_ascii_whitespace() {
            self.read_char()
        }
    }
}

fn is_letter(character: u8) -> bool {
    character.is_ascii_alphabetic() || character == b'_'
}

fn is_number(character: u8) -> bool {
    character.is_ascii_digit()
}

#[cfg(test)]
mod tests {
    use std::vec;

    use super::*;
    use token::TokenKind;

    fn test_next_token(input: &str, expected: &[TokenKind]) {
        let mut lexer = Lexer::new(input);

        for expected_token in expected.iter() {
            let token = lexer.next_token();

            assert_eq!(&token.kind, expected_token,);
        }
    }

    #[test]
    fn test_basic_tokens() {
        let input = "=+(){},;";
        let expected = vec![
            TokenKind::Assign,
            TokenKind::Plus,
            TokenKind::Lparen,
            TokenKind::Rparen,
            TokenKind::Lbrace,
            TokenKind::Rbrace,
            TokenKind::Comma,
            TokenKind::Semicolon,
            TokenKind::Eof,
        ];
        test_next_token(input, &expected);
    }

    #[test]
    fn test_basic_source_code() {
        let input = r#"let five = 5;
let ten = 10;

let add = fn(x, y) {
x + y;
};

let result = add(five, ten);
"#;

        let expected = vec![
            TokenKind::Let,
            TokenKind::Ident(String::from("five")),
            TokenKind::Assign,
            TokenKind::Int(5),
            TokenKind::Semicolon,
            TokenKind::Let,
            TokenKind::Ident(String::from("ten")),
            TokenKind::Assign,
            TokenKind::Int(10),
            TokenKind::Semicolon,
            TokenKind::Let,
            TokenKind::Ident(String::from("add")),
            TokenKind::Assign,
            TokenKind::Function,
            TokenKind::Lparen,
            TokenKind::Ident(String::from("x")),
            TokenKind::Comma,
            TokenKind::Ident(String::from("y")),
            TokenKind::Rparen,
            TokenKind::Lbrace,
            TokenKind::Ident(String::from("x")),
            TokenKind::Plus,
            TokenKind::Ident(String::from("y")),
            TokenKind::Semicolon,
            TokenKind::Rbrace,
            TokenKind::Semicolon,
            TokenKind::Let,
            TokenKind::Ident(String::from("result")),
            TokenKind::Assign,
            TokenKind::Ident(String::from("add")),
            TokenKind::Lparen,
            TokenKind::Ident(String::from("five")),
            TokenKind::Comma,
            TokenKind::Ident(String::from("ten")),
            TokenKind::Rparen,
            TokenKind::Semicolon,
            TokenKind::Eof,
        ];
        test_next_token(input, &expected);
    }

    #[test]
    fn test_chapter_1_4() {
        let input = r#"let five = 5;
let ten = 10;

let add = fn(x, y) {
  x + y;
};

let result = add(five, ten);
!-/*5;
5 < 10 > 5;

if (5 < 10) {
    return true;
} else {
    return false;
}

10 == 10;
10 != 9;
{"foo": "bar"}
"#;
        let expected = vec![
            TokenKind::Let,
            TokenKind::Ident(String::from("five")),
            TokenKind::Assign,
            TokenKind::Int(5),
            TokenKind::Semicolon,
            TokenKind::Let,
            TokenKind::Ident(String::from("ten")),
            TokenKind::Assign,
            TokenKind::Int(10),
            TokenKind::Semicolon,
            TokenKind::Let,
            TokenKind::Ident(String::from("add")),
            TokenKind::Assign,
            TokenKind::Function,
            TokenKind::Lparen,
            TokenKind::Ident(String::from("x")),
            TokenKind::Comma,
            TokenKind::Ident(String::from("y")),
            TokenKind::Rparen,
            TokenKind::Lbrace,
            TokenKind::Ident(String::from("x")),
            TokenKind::Plus,
            TokenKind::Ident(String::from("y")),
            TokenKind::Semicolon,
            TokenKind::Rbrace,
            TokenKind::Semicolon,
            TokenKind::Let,
            TokenKind::Ident(String::from("result")),
            TokenKind::Assign,
            TokenKind::Ident(String::from("add")),
            TokenKind::Lparen,
            TokenKind::Ident(String::from("five")),
            TokenKind::Comma,
            TokenKind::Ident(String::from("ten")),
            TokenKind::Rparen,
            TokenKind::Semicolon,
            TokenKind::Bang,
            TokenKind::Minus,
            TokenKind::Slash,
            TokenKind::Asterisk,
            TokenKind::Int(5),
            TokenKind::Semicolon,
            TokenKind::Int(5),
            TokenKind::LessThan,
            TokenKind::Int(10),
            TokenKind::GreaterThan,
            TokenKind::Int(5),
            TokenKind::Semicolon,
            TokenKind::If,
            TokenKind::Lparen,
            TokenKind::Int(5),
            TokenKind::LessThan,
            TokenKind::Int(10),
            TokenKind::Rparen,
            TokenKind::Lbrace,
            TokenKind::Return,
            TokenKind::True,
            TokenKind::Semicolon,
            TokenKind::Rbrace,
            TokenKind::Else,
            TokenKind::Lbrace,
            TokenKind::Return,
            TokenKind::False,
            TokenKind::Semicolon,
            TokenKind::Rbrace,
            TokenKind::Int(10),
            TokenKind::Equal,
            TokenKind::Int(10),
            TokenKind::Semicolon,
            TokenKind::Int(10),
            TokenKind::NotEqual,
            TokenKind::Int(9),
            TokenKind::Semicolon,
            TokenKind::Lbrace,
            TokenKind::String(String::from("foo")),
            TokenKind::Colon,
            TokenKind::String(String::from("bar")),
            TokenKind::Rbrace,
            TokenKind::Eof,
        ];

        test_next_token(input, &expected);
    }

    #[test]
    fn text_strings() {
        let input = r#""foobar"
            "foo bar""#;
        let expected = vec![
            TokenKind::String("foobar".into()),
            TokenKind::String("foo bar".into()),
            TokenKind::Eof,
        ];

        test_next_token(input, &expected);
    }

    #[test]
    fn text_arrays() {
        let input = "[1, 2];";
        let expected = vec![
            TokenKind::Lbracket,
            TokenKind::Int(1),
            TokenKind::Comma,
            TokenKind::Int(2),
            TokenKind::Rbracket,
            TokenKind::Semicolon,
            TokenKind::Eof,
        ];

        test_next_token(input, &expected);
    }
}
