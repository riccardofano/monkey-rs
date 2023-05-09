use crate::token::Token;
use crate::token::TokenKind::*;

struct Lexer {
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
            b'=' => Token::new(Assign, self.character),
            b'+' => Token::new(Plus, self.character),
            b',' => Token::new(Comma, self.character),
            b';' => Token::new(Semicolon, self.character),
            b'(' => Token::new(Lparen, self.character),
            b')' => Token::new(Rparen, self.character),
            b'{' => Token::new(Lbrace, self.character),
            b'}' => Token::new(Rbrace, self.character),
            0 => Token {
                kind: Eof,
                literal: String::new(),
            },
            _ => unimplemented!("Unknown byte"),
        };

        self.read_char();
        token
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::token::TokenKind;

    #[test]
    fn test_next_token() {
        let input = "=+(){},;";

        let tests = vec![
            (TokenKind::Assign, "="),
            (TokenKind::Plus, "+"),
            (TokenKind::Lparen, "("),
            (TokenKind::Rparen, ")"),
            (TokenKind::Lbrace, "{"),
            (TokenKind::Rbrace, "}"),
            (TokenKind::Comma, ","),
            (TokenKind::Semicolon, ";"),
            (TokenKind::Eof, ""),
        ];

        let mut lexer = Lexer::new(input);

        for token_test in tests.iter() {
            let token = lexer.next_token();

            assert!(token.kind == token_test.0);
            dbg!(&token.literal, token_test.1);
            assert!(token.literal == token_test.1);
        }
    }
}
