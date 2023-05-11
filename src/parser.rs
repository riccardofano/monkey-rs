use crate::{
    ast::{Expression, Identifier, Program, Statement},
    lexer::Lexer,
    token::{Token, TokenKind},
};

#[derive(Debug)]
struct Parser {
    lexer: Lexer,
    current_token: Token,
    peeked_token: Token,
    errors: Vec<String>,
}

impl Parser {
    pub fn new(lexer: Lexer) -> Self {
        let mut parser = Self {
            lexer,
            current_token: Token::new(TokenKind::Illegal),
            peeked_token: Token::new(TokenKind::Illegal),
            errors: Vec::new(),
        };
        parser.next_token();
        parser.next_token();
        dbg!(&parser);
        parser
    }

    pub fn parse_program(&mut self) -> Program {
        let mut program = Program::new();

        while !self.current_token_is(&TokenKind::Eof) {
            if let Some(statement) = self.parser_statement() {
                program.statements.push(statement);
            }
            self.next_token();
        }

        program
    }

    fn parser_statement(&mut self) -> Option<Statement> {
        let statement_result = match self.current_token.kind {
            TokenKind::Let => self.parse_let_statement(),
            _ => self.parse_expression_statement(),
        };

        match statement_result {
            Ok(statement) => Some(statement),
            Err(e) => {
                self.errors.push(e);
                None
            }
        }
    }

    pub fn errors(&self) -> &[String] {
        &self.errors
    }

    fn next_token(&mut self) {
        let previously_peeked = std::mem::replace(&mut self.peeked_token, self.lexer.next_token());
        self.current_token = previously_peeked;
    }

    fn parse_let_statement(&mut self) -> Result<Statement, String> {
        let TokenKind::Ident(name) = &self.peeked_token.kind else {
            return Err(format!("expected TokenKind to be Identifier(_), got: {:?}", &self.peeked_token.kind));
        };
        let name = name.clone();

        self.next_token();

        if self.peeked_token.kind != TokenKind::Assign {
            return Err(format!(
                "expected TokenKind to be Assign, got {:?}",
                self.peeked_token.kind
            ));
        };

        while !self.current_token_is(&TokenKind::Semicolon) {
            self.next_token();
        }

        Ok(Statement::LetStatement(
            Identifier(name),
            Expression::Placeholder,
        ))
    }

    fn peek_token_is(&self, kind: &TokenKind) -> bool {
        &self.peeked_token.kind == kind
    }

    // TODO: use thiserror for errors instead of strings
    fn expect_peek(&mut self, kind: &TokenKind) -> Result<(), String> {
        if !self.peek_token_is(kind) {
            return Err(format!(
                "expected next token to be {kind}, got: {:?}",
                self.peeked_token
            ));
        }

        self.next_token();
        Ok(())
    }

    fn parse_expression_statement(&self) -> Result<Statement, String> {
        Ok(Statement::ExpressionStatement)
    }

    fn current_token_is(&self, kind: &TokenKind) -> bool {
        &self.current_token.kind == kind
    }
}

#[cfg(test)]
mod tests {
    use crate::ast::Statement;

    use super::*;

    #[test]
    fn test_let_statements() {
        let input = r#"let x = 5;
        let y = 10;
        let foobar = 838383;"#;

        let lexer = Lexer::new(input);
        let mut parser = Parser::new(lexer);
        let program = parser.parse_program();

        assert!(parser.errors().is_empty());

        dbg!(&program.statements);
        if program.statements.len() != 3 {
            panic!(
                "The program does not contain 3 statements. Got {}",
                program.statements.len()
            )
        }

        let tests = vec!["x", "y", "foobar"];
        for (expected, statement) in tests.into_iter().zip(program.statements) {
            assert!(is_let_statement(statement, expected));
        }
    }

    fn is_let_statement(statement: Statement, name: &str) -> bool {
        let Statement::LetStatement(identifier, _) = statement else {
            eprintln!("statement is not let, got: {:?}", statement);
            return false;
        };

        if identifier.0 != name {
            eprintln!("TokenKind wasn't identifier, got: {}", identifier.0);
            return false;
        };
        true
    }
}
