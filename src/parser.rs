use crate::{
    ast::{Expression, Identifier, Program, Statement},
    lexer::Lexer,
    token::{Token, TokenKind},
};

#[derive(PartialEq, Eq, PartialOrd, Ord, Clone, Copy)]
pub enum Precedence {
    Lowest,
    Equals,
    LessGreater,
    Sum,
    Product,
    Prefix,
    Call,
}

impl From<&TokenKind> for Precedence {
    fn from(value: &TokenKind) -> Self {
        match value {
            TokenKind::Equal => Precedence::Equals,
            TokenKind::NotEqual => Precedence::Equals,
            TokenKind::LessThan => Precedence::LessGreater,
            TokenKind::GreaterThan => Precedence::LessGreater,
            TokenKind::Plus => Precedence::Sum,
            TokenKind::Minus => Precedence::Sum,
            TokenKind::Slash => Precedence::Product,
            TokenKind::Asterisk => Precedence::Product,
            TokenKind::Lparen => Precedence::Call,
            _ => Precedence::Lowest,
        }
    }
}

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
            TokenKind::Return => self.parse_return_statement(),
            _ => self.parse_expression_statement(Precedence::Lowest),
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

    fn peek_token_is(&self, kind: &TokenKind) -> bool {
        &self.peeked_token.kind == kind
    }

    fn current_token_is(&self, kind: &TokenKind) -> bool {
        &self.current_token.kind == kind
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

    fn parse_return_statement(&mut self) -> Result<Statement, String> {
        self.next_token();

        // TODO: parse expression; We're skipping until a semicolon for now.
        while !self.current_token_is(&TokenKind::Semicolon) {
            self.next_token();
        }

        Ok(Statement::ReturnStatement(Expression::Placeholder))
    }

    fn parse_expression_statement(&mut self, precedence: Precedence) -> Result<Statement, String> {
        let expression = self.parse_expression(precedence)?;
        if self.peek_token_is(&TokenKind::Semicolon) {
            self.next_token();
        }
        Ok(Statement::ExpressionStatement(expression))
    }

    fn parse_expression(&mut self, precedence: Precedence) -> Result<Expression, String> {
        if !Parser::has_parse_prefix_fn(&self.current_token.kind) {
            return Err(format!(
                "Expected a prefix. Got: {}",
                self.current_token.kind
            ));
        };

        let mut expression = self.parse_prefix()?;

        while !self.peek_token_is(&TokenKind::Semicolon) && precedence < self.peek_precedence() {
            if !Parser::has_parse_infix_fn(&self.current_token.kind) {
                return Ok(expression);
            };

            self.next_token();
            expression = self.parse_infix(expression);
        }

        Ok(expression)
    }

    fn has_parse_prefix_fn(kind: &TokenKind) -> bool {
        matches!(
            kind,
            TokenKind::Ident(_) | TokenKind::Int(_) | TokenKind::Bang | TokenKind::Minus
        )
    }

    fn has_parse_infix_fn(kind: &TokenKind) -> bool {
        matches!(kind, TokenKind::Plus)
    }

    fn parse_prefix(&mut self) -> Result<Expression, String> {
        let expr = match &self.current_token.kind {
            TokenKind::Ident(value) => Expression::Identifier(Identifier(value.clone())),
            TokenKind::Int(value) => Expression::IntegerLiteral(*value),
            TokenKind::Minus => {
                self.next_token();
                Expression::Prefix(
                    TokenKind::Minus,
                    Box::new(self.parse_expression(Precedence::Prefix)?),
                )
            }
            TokenKind::Bang => {
                self.next_token();
                Expression::Prefix(
                    TokenKind::Bang,
                    Box::new(self.parse_expression(Precedence::Prefix)?),
                )
            }
            _ => unimplemented!(),
        };

        Ok(expr)
    }

    fn parse_infix(&mut self, expression: Expression) -> Expression {
        todo!()
    }

    fn peek_precedence(&self) -> Precedence {
        let current_kind = &self.current_token.kind;
        current_kind.into()
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

        assert!(parser.errors().is_empty(), "{:?}", parser.errors());

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

    #[test]
    fn test_return_statements() {
        let input = r#"
        return 5;
        return 10;
        return 993322;
        "#;
        let mut parser = Parser::new(Lexer::new(input));
        let program = parser.parse_program();

        assert!(parser.errors().is_empty(), "{:?}", parser.errors());

        if program.statements.len() != 3 {
            panic!(
                "The program does not contain 3 statements. Got {}",
                program.statements.len()
            )
        }

        for statement in program.statements {
            let Statement::ReturnStatement(_) = statement else {
                eprintln!("statement is not let, got: {:?}", statement);
                continue;
            };
        }
    }

    #[test]
    fn test_identifier_expressions() {
        let input = "foobar;";
        let mut parser = Parser::new(Lexer::new(input));
        let program = parser.parse_program();

        assert!(parser.errors().is_empty(), "{:?}", parser.errors());

        if program.statements.len() != 1 {
            panic!("expected 1 statement. Got {}", program.statements.len());
        }

        let Statement::ExpressionStatement(ident) = &program.statements[0] else {
            panic!("expected an ExpressionStatement. Got {}", program.statements[0]);
        };

        assert_eq!(ident.to_string(), "foobar");
    }

    #[test]
    fn test_integer_expressions() {
        let input = "5;";
        let mut parser = Parser::new(Lexer::new(input));
        let program = parser.parse_program();

        assert!(parser.errors().is_empty(), "{:?}", parser.errors());

        if program.statements.len() != 1 {
            panic!("expected 1 statement. Got {}", program.statements.len());
        }

        let Statement::ExpressionStatement(ident) = &program.statements[0] else {
            panic!("expected an ExpressionStatement. Got {}", program.statements[0]);
        };

        assert_eq!(ident.to_string(), "5");
    }

    #[test]
    fn test_parsing_prefix_expressions() {
        let inputs: Vec<(&str, &str, usize)> = vec![("!5;", "!", 5), ("-15", "-", 15)];

        for input in inputs {
            let mut parser = Parser::new(Lexer::new(input.0));
            let program = parser.parse_program();

            assert!(parser.errors().is_empty(), "{:?}", parser.errors());

            if program.statements.len() != 1 {
                panic!("expected 1 statement. Got {}", program.statements.len());
            }

            let Statement::ExpressionStatement(expression) = &program.statements[0] else {
            panic!("expected an ExpressionStatement. Got {}", program.statements[0]);
            };

            let Expression::Prefix(token_kind, expression) = expression else {
                panic!("expected a PrefixExpression. Got: {:?}", expression);
            };
            assert_eq!(token_kind.to_string(), input.1);

            let Expression::IntegerLiteral(value) = **expression else {
                panic!("expected an IntegerLiteral. Got: {:?}", expression);
            };

            assert_eq!(value, input.2);
        }
    }
}
