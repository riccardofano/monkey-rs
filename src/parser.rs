use crate::{ast::Program, lexer::Lexer, token::Token};

struct Parser {
    lexer: Lexer,
    current_token: Option<Token>,
    peeked_token: Option<Token>,
}

impl Parser {
    pub fn new(lexer: Lexer) -> Self {
        let mut parser = Self {
            lexer,
            current_token: None,
            peeked_token: None,
        };
        parser.next_token();
        parser.next_token();
        parser
    }

    pub fn parse_program() -> Program {
        todo!()
    }

    fn next_token(&mut self) {
        self.current_token = self.peeked_token.take();
        self.peeked_token = self.lexer.next_token()
    }
}
