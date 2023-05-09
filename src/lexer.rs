use crate::token::Token;
use crate::token::TokenKind;
use crate::token::TokenKind::*;

struct Lexer<'a> {
    input: &'a str,
    position: usize,
    read_position: usize,
    character: u8,
}

impl<'a> Lexer<'a> {
    pub fn new(input: &'a str) -> Self {
        let mut lexer = Self {
            input,
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

    fn read_identifier<'b>(&'b mut self) -> &'b str {
        let start = self.position;
        while is_letter(self.character) {
            self.read_char();
        }
        &self.input[start..self.position]
    }

    fn read_number(&mut self) -> usize {
        let start = self.position;
        while is_number(self.character) {
            self.read_char();
        }
        let number = &self.input[start..self.position];
        number.parse().unwrap()
    }

    pub fn next_token<'b>(&'b mut self) -> Token {
        self.skip_whitespace();

        let token = match self.character {
            b'+' => Token::new(Plus),
            b'-' => Token::new(Minus),
            b'*' => Token::new(Asterisk),
            b'/' => Token::new(Slash),
            b'<' => Token::new(LessThan),
            b'>' => Token::new(GreaterThan),
            b',' => Token::new(Comma),
            b';' => Token::new(Semicolon),
            b'(' => Token::new(Lparen),
            b')' => Token::new(Rparen),
            b'{' => Token::new(Lbrace),
            b'}' => Token::new(Rbrace),
            b'=' => {
                if self.peek_char() == b'=' {
                    self.read_char();
                    Token::new(Equal)
                } else {
                    Token::new(Assign)
                }
            }
            b'!' => {
                if self.peek_char() == b'=' {
                    self.read_char();
                    Token::new(NotEqual)
                } else {
                    Token::new(Bang)
                }
            }
            0 => Token::new(Eof),
            c => {
                if is_letter(c) {
                    let literal = self.read_identifier();
                    let kind = TokenKind::from_letters(literal);
                    return Token::new(kind);
                } else if is_number(c) {
                    let number = self.read_number();
                    return Token::new(Int(number));
                } else {
                    return Token::new(Illegal);
                }
            }
        };

        self.read_char();
        token
    }

    fn skip_whitespace(&mut self) {
        while self.character.is_ascii_whitespace() {
            self.read_char()
        }
    }
}

fn is_letter(character: u8) -> bool {
    return character.is_ascii_alphabetic() || character == b'_';
}

fn is_number(character: u8) -> bool {
    return character.is_ascii_digit();
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::token::TokenKind;

    fn test_next_token(input: &str, expected: &[TokenKind]) {
        let mut lexer = Lexer::new(input);

        for expected_token in expected.iter() {
            let token = lexer.next_token();

            println!("token: `{:?}` | test: `{:?}`", &token.kind, expected_token);
            assert!(&token.kind == expected_token);
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
            TokenKind::Ident("five"),
            TokenKind::Assign,
            TokenKind::Int(5),
            TokenKind::Semicolon,
            TokenKind::Let,
            TokenKind::Ident("ten"),
            TokenKind::Assign,
            TokenKind::Int(10),
            TokenKind::Semicolon,
            TokenKind::Let,
            TokenKind::Ident("add"),
            TokenKind::Assign,
            TokenKind::Function,
            TokenKind::Lparen,
            TokenKind::Ident("x"),
            TokenKind::Comma,
            TokenKind::Ident("y"),
            TokenKind::Rparen,
            TokenKind::Lbrace,
            TokenKind::Ident("x"),
            TokenKind::Plus,
            TokenKind::Ident("y"),
            TokenKind::Semicolon,
            TokenKind::Rbrace,
            TokenKind::Semicolon,
            TokenKind::Let,
            TokenKind::Ident("result"),
            TokenKind::Assign,
            TokenKind::Ident("add"),
            TokenKind::Lparen,
            TokenKind::Ident("five"),
            TokenKind::Comma,
            TokenKind::Ident("ten"),
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
"#;
        let expected = vec![
            TokenKind::Let,
            TokenKind::Ident("five"),
            TokenKind::Assign,
            TokenKind::Int(5),
            TokenKind::Semicolon,
            TokenKind::Let,
            TokenKind::Ident("ten"),
            TokenKind::Assign,
            TokenKind::Int(10),
            TokenKind::Semicolon,
            TokenKind::Let,
            TokenKind::Ident("add"),
            TokenKind::Assign,
            TokenKind::Function,
            TokenKind::Lparen,
            TokenKind::Ident("x"),
            TokenKind::Comma,
            TokenKind::Ident("y"),
            TokenKind::Rparen,
            TokenKind::Lbrace,
            TokenKind::Ident("x"),
            TokenKind::Plus,
            TokenKind::Ident("y"),
            TokenKind::Semicolon,
            TokenKind::Rbrace,
            TokenKind::Semicolon,
            TokenKind::Let,
            TokenKind::Ident("result"),
            TokenKind::Assign,
            TokenKind::Ident("add"),
            TokenKind::Lparen,
            TokenKind::Ident("five"),
            TokenKind::Comma,
            TokenKind::Ident("ten"),
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
            TokenKind::Eof,
        ];

        test_next_token(input, &expected);
    }
}
