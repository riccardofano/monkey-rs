use crate::token::Token;
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

    pub fn read_char(&mut self) {
        if self.read_position >= self.input.len() {
            self.character = 0;
        } else {
            self.character = self.input.as_bytes()[self.read_position];
        }
        self.position = self.read_position;
        self.read_position += 1;
    }

    // I feel like this shouldn't be here and I should just implement iterator
    // for Token/TokenKind but we'll see where the book goes first
    pub fn next_token(&mut self) -> Token {
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
            b => unimplemented!("Unknown byte {b:?}"),
        };

        self.read_char();
        token
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::token::TokenKind;

    fn test_next_token(input: &str, expected: &[TokenKind]) {
        let mut lexer = Lexer::new(input);

        for token_test in expected.iter() {
            let token = lexer.next_token();

            assert!(&token.kind == token_test);
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
}
