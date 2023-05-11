use crate::token::Token;
use crate::token::TokenKind;
use crate::token::TokenKind::*;

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

    fn read_identifier(&mut self) -> &str {
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

    pub fn next_token(&mut self) -> Option<Token> {
        self.skip_whitespace();

        if self.character == 0 {
            return None;
        }

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
            c => {
                if is_letter(c) {
                    let literal = self.read_identifier();
                    let kind = TokenKind::from_letters(literal);
                    return Some(Token::new(kind));
                } else if is_number(c) {
                    let number = self.read_number();
                    return Some(Token::new(Int(number)));
                } else {
                    return Some(Token::new(Illegal));
                }
            }
        };

        self.read_char();
        Some(token)
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
    use super::*;
    use crate::token::TokenKind;

    fn test_next_token(input: &str, expected: &[Option<TokenKind>]) {
        let mut lexer = Lexer::new(input);

        for expected_token in expected.iter() {
            let token = lexer.next_token();

            match (token, expected_token) {
                (Some(a), Some(b)) => assert!(&a.kind == b),
                (None, None) => todo!(),
                (a, b) => panic!("{a:?} is not equal to {b:?}"),
            }
        }
    }

    #[test]
    fn test_basic_tokens() {
        let input = "=+(){},;";
        let expected = vec![
            Some(TokenKind::Assign),
            Some(TokenKind::Plus),
            Some(TokenKind::Lparen),
            Some(TokenKind::Rparen),
            Some(TokenKind::Lbrace),
            Some(TokenKind::Rbrace),
            Some(TokenKind::Comma),
            Some(TokenKind::Semicolon),
            None,
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
            Some(TokenKind::Let),
            Some(TokenKind::Ident(String::from("five"))),
            Some(TokenKind::Assign),
            Some(TokenKind::Int(5)),
            Some(TokenKind::Semicolon),
            Some(TokenKind::Let),
            Some(TokenKind::Ident(String::from("ten"))),
            Some(TokenKind::Assign),
            Some(TokenKind::Int(10)),
            Some(TokenKind::Semicolon),
            Some(TokenKind::Let),
            Some(TokenKind::Ident(String::from("add"))),
            Some(TokenKind::Assign),
            Some(TokenKind::Function),
            Some(TokenKind::Lparen),
            Some(TokenKind::Ident(String::from("x"))),
            Some(TokenKind::Comma),
            Some(TokenKind::Ident(String::from("y"))),
            Some(TokenKind::Rparen),
            Some(TokenKind::Lbrace),
            Some(TokenKind::Ident(String::from("x"))),
            Some(TokenKind::Plus),
            Some(TokenKind::Ident(String::from("y"))),
            Some(TokenKind::Semicolon),
            Some(TokenKind::Rbrace),
            Some(TokenKind::Semicolon),
            Some(TokenKind::Let),
            Some(TokenKind::Ident(String::from("result"))),
            Some(TokenKind::Assign),
            Some(TokenKind::Ident(String::from("add"))),
            Some(TokenKind::Lparen),
            Some(TokenKind::Ident(String::from("five"))),
            Some(TokenKind::Comma),
            Some(TokenKind::Ident(String::from("ten"))),
            Some(TokenKind::Rparen),
            Some(TokenKind::Semicolon),
            None,
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
            Some(TokenKind::Let),
            Some(TokenKind::Ident(String::from("five"))),
            Some(TokenKind::Assign),
            Some(TokenKind::Int(5)),
            Some(TokenKind::Semicolon),
            Some(TokenKind::Let),
            Some(TokenKind::Ident(String::from("ten"))),
            Some(TokenKind::Assign),
            Some(TokenKind::Int(10)),
            Some(TokenKind::Semicolon),
            Some(TokenKind::Let),
            Some(TokenKind::Ident(String::from("add"))),
            Some(TokenKind::Assign),
            Some(TokenKind::Function),
            Some(TokenKind::Lparen),
            Some(TokenKind::Ident(String::from("x"))),
            Some(TokenKind::Comma),
            Some(TokenKind::Ident(String::from("y"))),
            Some(TokenKind::Rparen),
            Some(TokenKind::Lbrace),
            Some(TokenKind::Ident(String::from("x"))),
            Some(TokenKind::Plus),
            Some(TokenKind::Ident(String::from("y"))),
            Some(TokenKind::Semicolon),
            Some(TokenKind::Rbrace),
            Some(TokenKind::Semicolon),
            Some(TokenKind::Let),
            Some(TokenKind::Ident(String::from("result"))),
            Some(TokenKind::Assign),
            Some(TokenKind::Ident(String::from("add"))),
            Some(TokenKind::Lparen),
            Some(TokenKind::Ident(String::from("five"))),
            Some(TokenKind::Comma),
            Some(TokenKind::Ident(String::from("ten"))),
            Some(TokenKind::Rparen),
            Some(TokenKind::Semicolon),
            Some(TokenKind::Bang),
            Some(TokenKind::Minus),
            Some(TokenKind::Slash),
            Some(TokenKind::Asterisk),
            Some(TokenKind::Int(5)),
            Some(TokenKind::Semicolon),
            Some(TokenKind::Int(5)),
            Some(TokenKind::LessThan),
            Some(TokenKind::Int(10)),
            Some(TokenKind::GreaterThan),
            Some(TokenKind::Int(5)),
            Some(TokenKind::Semicolon),
            Some(TokenKind::If),
            Some(TokenKind::Lparen),
            Some(TokenKind::Int(5)),
            Some(TokenKind::LessThan),
            Some(TokenKind::Int(10)),
            Some(TokenKind::Rparen),
            Some(TokenKind::Lbrace),
            Some(TokenKind::Return),
            Some(TokenKind::True),
            Some(TokenKind::Semicolon),
            Some(TokenKind::Rbrace),
            Some(TokenKind::Else),
            Some(TokenKind::Lbrace),
            Some(TokenKind::Return),
            Some(TokenKind::False),
            Some(TokenKind::Semicolon),
            Some(TokenKind::Rbrace),
            Some(TokenKind::Int(10)),
            Some(TokenKind::Equal),
            Some(TokenKind::Int(10)),
            Some(TokenKind::Semicolon),
            Some(TokenKind::Int(10)),
            Some(TokenKind::NotEqual),
            Some(TokenKind::Int(9)),
            Some(TokenKind::Semicolon),
            None,
        ];

        test_next_token(input, &expected);
    }
}
