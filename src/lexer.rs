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

    // I feel like this shouldn't be here and I should just implement iterator
    // for Token/TokenKind but we'll see where the book goes first
    pub fn next_token<'b>(&'b mut self) -> Token {
        self.skip_whitespace();

        let token = match self.character {
            b'=' => Token::new(Assign),
            b'+' => Token::new(Plus),
            b',' => Token::new(Comma),
            b';' => Token::new(Semicolon),
            b'(' => Token::new(Lparen),
            b')' => Token::new(Rparen),
            b'{' => Token::new(Lbrace),
            b'}' => Token::new(Rbrace),
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

        for token_test in expected.iter() {
            let token = lexer.next_token();

            assert!(
                &token.kind == token_test,
                "a = {}, b = {}",
                &token.kind,
                token_test
            );
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
}
