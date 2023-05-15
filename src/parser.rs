use crate::{
    ast::{Expression, Identifier, Program, Statement},
    lexer::Lexer,
    token::{Token, TokenKind},
};

#[derive(Debug, PartialEq, Eq, PartialOrd, Ord, Clone, Copy)]
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
            if let Some(statement) = self.parse_statement() {
                program.statements.push(statement);
            }
            self.next_token();
        }

        program
    }

    fn parse_statement(&mut self) -> Option<Statement> {
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

    fn parse_block_statement(&mut self) -> Result<Statement, String> {
        let mut statements = Vec::new();
        self.next_token();

        while !self.current_token_is(&TokenKind::Rbrace) && !self.current_token_is(&TokenKind::Eof)
        {
            if let Some(statement) = self.parse_statement() {
                statements.push(Box::new(statement));
            }
            self.next_token();
        }
        Ok(Statement::BlockStatement(statements))
    }

    fn parse_function_params(&mut self) -> Result<Vec<Expression>, String> {
        let mut identifiers = Vec::new();
        if self.peek_token_is(&TokenKind::Rparen) {
            self.next_token();
            return Ok(identifiers);
        }

        identifiers.push(self.parse_identifier()?);
        while self.peek_token_is(&TokenKind::Comma) {
            self.next_token();
            identifiers.push(self.parse_identifier()?);
        }
        self.expect_peek(&TokenKind::Rparen)?;

        Ok(identifiers)
    }

    fn parse_identifier(&mut self) -> Result<Expression, String> {
        self.next_token();
        let TokenKind::Ident(value) = &self.current_token.kind else {
            return Err(format!("Expected an identifier. Got {:?}", self.current_token));
        };
        Ok(Expression::Identifier(Identifier(value.clone())))
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
            if !Parser::has_parse_infix_fn(&self.peeked_token.kind) {
                return Ok(expression);
            };

            self.next_token();
            expression = self.parse_infix(expression)?;
        }

        Ok(expression)
    }

    fn has_parse_prefix_fn(kind: &TokenKind) -> bool {
        matches!(
            kind,
            TokenKind::Ident(_)
                | TokenKind::Int(_)
                | TokenKind::True
                | TokenKind::False
                | TokenKind::Bang
                | TokenKind::Minus
                | TokenKind::Lparen
                | TokenKind::If
                | TokenKind::Function
        )
    }

    fn has_parse_infix_fn(kind: &TokenKind) -> bool {
        matches!(
            kind,
            TokenKind::Plus
                | TokenKind::Minus
                | TokenKind::Asterisk
                | TokenKind::Slash
                | TokenKind::LessThan
                | TokenKind::GreaterThan
                | TokenKind::Equal
                | TokenKind::NotEqual
        )
    }

    fn parse_prefix(&mut self) -> Result<Expression, String> {
        let expr = match &self.current_token.kind {
            TokenKind::Ident(value) => Expression::Identifier(Identifier(value.clone())),
            TokenKind::Int(value) => Expression::Integer(*value),
            TokenKind::True => Expression::Boolean(true),
            TokenKind::False => Expression::Boolean(false),
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
            TokenKind::Lparen => {
                self.next_token();
                let expression = self.parse_expression(Precedence::Lowest);
                self.expect_peek(&TokenKind::Rparen)?;
                return expression;
            }
            TokenKind::If => {
                self.expect_peek(&TokenKind::Lparen)?;
                self.next_token();
                let condition = self.parse_expression(Precedence::Lowest)?;

                self.expect_peek(&TokenKind::Rparen)?;
                self.expect_peek(&TokenKind::Lbrace)?;

                let consequence = self.parse_block_statement()?;

                let mut alternative = None;
                if self.peek_token_is(&TokenKind::Else) {
                    self.next_token();
                    self.expect_peek(&TokenKind::Lbrace)?;

                    alternative = Some(Box::new(self.parse_block_statement()?));
                }
                Expression::If(Box::new(condition), Box::new(consequence), alternative)
            }
            TokenKind::Function => {
                self.expect_peek(&TokenKind::Lparen)?;
                let params = self.parse_function_params()?;
                self.expect_peek(&TokenKind::Lbrace)?;
                let body = self.parse_block_statement()?;

                Expression::Function(params, Box::new(body))
            }
            _ => unimplemented!(),
        };

        Ok(expr)
    }

    fn parse_infix(&mut self, left: Expression) -> Result<Expression, String> {
        let token = self.current_token.kind.clone();
        let precedence = self.current_precedence();
        self.next_token();

        let right = self.parse_expression(precedence)?;
        Ok(Expression::Infix(Box::new(left), token, Box::new(right)))
    }

    fn peek_precedence(&self) -> Precedence {
        let peeked_kind = &self.peeked_token.kind;
        peeked_kind.into()
    }

    fn current_precedence(&self) -> Precedence {
        let current_kind = &self.current_token.kind;
        current_kind.into()
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::ast::Statement;

    trait TestExpression {
        fn test_expression(&self, expression: &Expression) -> bool;
    }

    impl TestExpression for &str {
        fn test_expression(&self, expression: &Expression) -> bool {
            let Expression::Identifier(ident) = expression else {
            eprintln!("expression is not Identifier(_). Got: {:?}", expression);
            return false;
        };

            if &ident.0 != self {
                eprintln!("identifier value is not {self}. Got {:?}", ident);
                return false;
            }

            true
        }
    }

    impl TestExpression for usize {
        fn test_expression(&self, expression: &Expression) -> bool {
            let Expression::Integer(int) = expression else {
                eprintln!("expression is not Integer(_). Got {:?}", expression);
                return false;
            };

            if int != self {
                eprintln!("integer value is not {self}. Got {:?}", int);
                return false;
            }
            true
        }
    }

    impl TestExpression for bool {
        fn test_expression(&self, expression: &Expression) -> bool {
            let Expression::Boolean(bool) = expression else {
                eprintln!("expression is not Boolean(_). Got: {:?}", expression);
                return false;
            };

            if bool != self {
                eprintln!("boolean value is not {self}. Got {:?}", bool);
                return false;
            }

            true
        }
    }

    fn test_literal_expression<T: TestExpression + ?Sized>(
        expression: &Expression,
        value: &T,
    ) -> bool {
        value.test_expression(expression)
    }

    fn test_infix_expression<T: TestExpression + ?Sized>(
        infix: &Expression,
        left: &T,
        operator: &str,
        right: &T,
    ) -> bool {
        let Expression::Infix(left_expression, op, right_expresssion) = infix else {
            eprintln!("expression is not Infix(_,_,_). Got: {:?}", infix);
            return false;
        };

        if !left.test_expression(left_expression) {
            return false;
        };

        if op.to_string() != operator {
            eprintln!("expected operator '{operator}'. Got: {:?}", op);
            return false;
        }

        if !right.test_expression(right_expresssion) {
            return false;
        }

        true
    }

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

        assert!(test_literal_expression(ident, &"foobar"))
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

        assert!(test_literal_expression(ident, &5))
    }

    #[test]
    fn test_parsing_prefix_expressions() {
        let inputs: Vec<(&str, &str, &dyn TestExpression)> = vec![
            ("!5;", "!", &5),
            ("-15", "-", &15),
            ("!true", "!", &true),
            ("!false", "!", &false),
        ];

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

            assert!(test_literal_expression(expression, input.2))
        }
    }

    #[test]
    fn test_parsing_infix_expressions() {
        let inputs: Vec<(&str, &dyn TestExpression, &str, &dyn TestExpression)> = vec![
            ("5 + 5;", &5, "+", &5),
            ("5 - 5;", &5, "-", &5),
            ("5 * 5;", &5, "*", &5),
            ("5 / 5;", &5, "/", &5),
            ("5 < 5;", &5, "<", &5),
            ("5 > 5;", &5, ">", &5),
            ("5 == 5;", &5, "==", &5),
            ("5 != 5;", &5, "!=", &5),
            ("true == true", &true, "==", &true),
            ("true != false", &true, "!=", &false),
            ("false == false", &false, "==", &false),
        ];

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

            assert!(test_infix_expression(expression, input.1, input.2, input.3))
        }
    }

    #[test]
    fn test_parsing_operator_precedence() {
        let inputs: Vec<(&str, &str)> = vec![
            ("-a * b", "((-a) * b)"),
            ("!-a", "(!(-a))"),
            ("a + b + c", "((a + b) + c)"),
            ("a + b - c", "((a + b) - c)"),
            ("a * b * c", "((a * b) * c)"),
            ("a * b / c", "((a * b) / c)"),
            ("a + b / c", "(a + (b / c))"),
            ("a + b * c + d / e - f", "(((a + (b * c)) + (d / e)) - f)"),
            ("3 + 4; -5 * 5", "(3 + 4)((-5) * 5)"),
            ("5 > 4 == 3 < 4", "((5 > 4) == (3 < 4))"),
            ("5 < 4 != 3 > 4", "((5 < 4) != (3 > 4))"),
            (
                "3 + 4 * 5 == 3 * 1 + 4 * 5",
                "((3 + (4 * 5)) == ((3 * 1) + (4 * 5)))",
            ),
            ("1 + (2 + 3) + 4", "((1 + (2 + 3)) + 4)"),
            ("(5 + 5) * 2", "((5 + 5) * 2)"),
            ("2 / (5 + 5)", "(2 / (5 + 5))"),
            ("-(5 + 5)", "(-(5 + 5))"),
            ("!(true == true)", "(!(true == true))"),
        ];

        for input in inputs {
            let mut parser = Parser::new(Lexer::new(input.0));
            let program = parser.parse_program();

            assert!(parser.errors().is_empty());

            assert_eq!(program.to_string(), input.1);
        }
    }

    #[test]
    fn test_boolean_expressions() {
        let inputs: Vec<(&str, bool)> = vec![("true;", true), ("false;", false)];

        for input in inputs {
            let mut parser = Parser::new(Lexer::new(input.0));
            let program = parser.parse_program();

            assert!(parser.errors().is_empty());

            let Statement::ExpressionStatement(expression) = &program.statements[0] else {
                panic!("expected an ExpressionStatement. Got {:?}", program.statements[0]);
            };

            assert!(test_literal_expression(expression, &input.1));
        }
    }

    #[test]
    fn test_if_expression() {
        let input = "if (x < y) { x }";
        let mut parser = Parser::new(Lexer::new(input));
        let program = parser.parse_program();

        assert!(parser.errors().is_empty(), "{:?}", parser.errors());

        assert_eq!(program.statements.len(), 1);

        let Statement::ExpressionStatement(if_expression) = &program.statements[0] else {
            panic!("expected an ExpressionStatement. Got {:?}", program.statements[0]);
        };

        let Expression::If(condition, consequence, alternative) = if_expression else {
            panic!("expected an If(_, _, _). Got {:?}", if_expression);
        };

        assert!(test_infix_expression(condition, &"x", "<", &"y"));

        let Statement::BlockStatement(block_statements) = &**consequence else {
            panic!("expected a BlockStatement(_). Got {:?}", consequence);
        };

        assert_eq!(block_statements.len(), 1);
        let Statement::ExpressionStatement(consequence_expression) = &*block_statements[0] else {
            panic!("expected an ExpressionStatement. Got {:?}", block_statements[0]);
        };
        assert!(test_literal_expression(consequence_expression, &"x"));

        assert!(alternative.is_none())
    }

    #[test]
    fn test_if_else_expression() {
        let input = "if (x < y) { x } else { y }";
        let mut parser = Parser::new(Lexer::new(input));
        let program = parser.parse_program();

        assert!(parser.errors().is_empty(), "{:?}", parser.errors());

        assert_eq!(program.statements.len(), 1);

        let Statement::ExpressionStatement(if_expression) = &program.statements[0] else {
            panic!("expected an ExpressionStatement. Got {:?}", program.statements[0]);
        };

        let Expression::If(condition, consequence, alternative) = if_expression else {
            panic!("expected an If(_, _, _). Got {:?}", if_expression);
        };

        assert!(test_infix_expression(condition, &"x", "<", &"y"));

        let Statement::BlockStatement(block_statements) = &**consequence else {
            panic!("expected a BlockStatement(_). Got {:?}", consequence);
        };

        assert_eq!(block_statements.len(), 1);
        let Statement::ExpressionStatement(consequence_expression) = &*block_statements[0] else {
            panic!("expected an ExpressionStatement. Got {:?}", block_statements[0]);
        };
        assert!(test_literal_expression(consequence_expression, &"x"));

        let Some(alternative) = alternative else {
            panic!("expected an else block. Got {:?}", alternative);
        };

        let Statement::BlockStatement(else_statements) = &**alternative else {
            panic!("expected a BlockStatement(_). Got {:?}", alternative);
        };

        let Statement::ExpressionStatement(alterative_expression) = &*else_statements[0] else {
            panic!("expected an ExpressionStaement. Got {:?}", else_statements[0]);
        };

        assert!(test_literal_expression(alterative_expression, &"y"));
    }

    fn test_function_parsing() {
        let input = "fn(x, y) { x + y }";
        let mut parser = Parser::new(Lexer::new(input));
        let program = parser.parse_program();

        assert!(parser.errors().is_empty(), "{:?}", parser.errors());

        assert_eq!(program.statements.len(), 1, "{:?}", program.statements);

        let Statement::ExpressionStatement(expression) = &program.statements[0] else {
            panic!("expected an ExpressionStatement. Got {:?}", program.statements[0]);
        };

        let Expression::Function(params, body) = expression else {
            panic!("exptected a Function(_,_). Got {:?}", expression);
        };

        assert_eq!(params.len(), 2, "{:?}", params);
        assert!(test_literal_expression(&params[0], &"x"));
        assert!(test_literal_expression(&params[1], &"y"));

        let Statement::BlockStatement(block) = &**body else {
            panic!("expected a BlockStatement. Got {:?}", body);
        };

        assert_eq!(block.len(), 1, "{:?}", block);
        let Statement::ExpressionStatement(infix) = &*block[0] else {
            panic!("expected an ExpressionStatement. Got {:?}", block[0]);
        };
        assert!(test_infix_expression(infix, &"x", "+", &"y"));
    }

    fn test_function_param_parsing() {
        let inputs: Vec<(&str, Vec<&str>)> = vec![
            ("fn() {}", vec![]),
            ("fn(x) {}", vec!["x"]),
            ("fn(x,y,z) {}", vec!["x", "y", "z"]),
        ];
        for input in inputs {
            let mut parser = Parser::new(Lexer::new(input.0));
            let program = parser.parse_program();

            assert!(parser.errors().is_empty(), "{:?}", parser.errors());

            assert_eq!(program.statements.len(), 1, "{:?}", program.statements);

            let Statement::ExpressionStatement(expression) = &program.statements[0] else {
                panic!("expected an ExpressionStatement. Got {:?}", program.statements[0]);
            };

            let Expression::Function(params, _) = expression else {
                panic!("exptected a Function(_,_). Got {:?}", expression);
            };

            assert_eq!(params.len(), input.1.len(), "{:?}", params);
            for (i, param) in input.1.iter().enumerate() {
                assert!(test_literal_expression(&params[i], param));
            }
        }
    }
}
