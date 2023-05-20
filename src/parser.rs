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
    Index,
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
            TokenKind::Lbracket => Precedence::Index,
            _ => Precedence::Lowest,
        }
    }
}

#[derive(Debug)]
pub struct Parser {
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
        self.expect_peek(&TokenKind::Assign)?;
        self.next_token();

        let value = self.parse_expression(Precedence::Lowest)?;
        if self.peek_token_is(&TokenKind::Semicolon) {
            self.next_token();
        }

        Ok(Statement::Let(Identifier(name), value))
    }

    fn parse_return_statement(&mut self) -> Result<Statement, String> {
        self.next_token();

        let return_value = self.parse_expression(Precedence::Lowest)?;
        if self.peek_token_is(&TokenKind::Semicolon) {
            self.next_token();
        }

        Ok(Statement::Return(return_value))
    }

    fn parse_block_statement(&mut self) -> Result<Statement, String> {
        let mut statements = Vec::new();
        self.next_token();

        while !self.current_token_is(&TokenKind::Rbrace) && !self.current_token_is(&TokenKind::Eof)
        {
            if let Some(statement) = self.parse_statement() {
                statements.push(statement);
            }
            self.next_token();
        }
        Ok(Statement::Block(statements))
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

        Ok(Statement::Expression(expression))
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
                | TokenKind::String(_)
                | TokenKind::True
                | TokenKind::False
                | TokenKind::Bang
                | TokenKind::Minus
                | TokenKind::Lparen
                | TokenKind::If
                | TokenKind::Function
                | TokenKind::Lbracket
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
                | TokenKind::Lparen
                | TokenKind::Lbracket
        )
    }

    fn parse_prefix(&mut self) -> Result<Expression, String> {
        let expr = match &self.current_token.kind {
            TokenKind::Ident(value) => Expression::Identifier(Identifier(value.clone())),
            TokenKind::Int(value) => Expression::Integer(*value),
            TokenKind::String(string) => Expression::String(string.clone()),
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
            TokenKind::Lbracket => {
                Expression::Array(self.parse_expression_list(&TokenKind::Rbracket)?)
            }
            _ => unimplemented!(),
        };

        Ok(expr)
    }

    fn parse_infix(&mut self, left: Expression) -> Result<Expression, String> {
        let token = self.current_token.kind.clone();
        match token {
            TokenKind::Lparen => {
                let right = self.parse_expression_list(&TokenKind::Rparen)?;
                Ok(Expression::Call(Box::new(left), right))
            }
            TokenKind::Lbracket => {
                self.next_token();
                let index = self.parse_expression(Precedence::Lowest)?;
                self.expect_peek(&TokenKind::Rbracket)?;
                Ok(Expression::Index(Box::new(left), Box::new(index)))
            }
            _ => {
                let precedence = self.current_precedence();
                self.next_token();

                let right = self.parse_expression(precedence)?;
                Ok(Expression::Infix(Box::new(left), token, Box::new(right)))
            }
        }
    }

    fn parse_expression_list(&mut self, end: &TokenKind) -> Result<Vec<Expression>, String> {
        let mut list = Vec::new();
        if self.peek_token_is(end) {
            self.next_token();
            return Ok(list);
        }

        self.next_token();
        list.push(self.parse_expression(Precedence::Lowest)?);

        while self.peek_token_is(&TokenKind::Comma) {
            self.next_token();
            self.next_token();
            list.push(self.parse_expression(Precedence::Lowest)?);
        }

        self.expect_peek(end)?;
        Ok(list)
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

    impl TestExpression for i64 {
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
        let inputs: Vec<(&str, &str, &dyn TestExpression)> = vec![
            ("let x = 5;", "x", &5),
            ("let y = true;", "y", &true),
            ("let foobar = y;", "foobar", &"y"),
        ];

        for input in inputs {
            let mut parser = Parser::new(Lexer::new(input.0));
            let program = parser.parse_program();

            assert!(parser.errors().is_empty(), "{:?}", parser.errors());
            assert_eq!(program.statements.len(), 1, "{:?}", program.statements);

            let Statement::Let(ident, value) = &program.statements[0] else {
                panic!("expected a LetStatement(_,_). Got {:?}", program.statements[0]);
            };

            assert_eq!(&ident.0, input.1);
            assert!(test_literal_expression(value, input.2));
        }
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
            let Statement::Return(_) = statement else {
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

        let Statement::Expression(ident) = &program.statements[0] else {
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

        let Statement::Expression(ident) = &program.statements[0] else {
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

            let Statement::Expression(expression) = &program.statements[0] else {
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

            let Statement::Expression(expression) = &program.statements[0] else {
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
            ("a + add(b * c) + d", "((a + add((b * c))) + d)"),
            (
                "add(a, b, 1, 2 * 3, 4 + 5, add(6, 7 * 8))",
                "add(a, b, 1, (2 * 3), (4 + 5), add(6, (7 * 8)))",
            ),
            (
                "add(a + b + c * d / f + g)",
                "add((((a + b) + ((c * d) / f)) + g))",
            ),
            (
                "a * [1, 2, 3, 4][b * c] * d",
                "((a * ([1, 2, 3, 4][(b * c)])) * d)",
            ),
            (
                "add(a * b[2], b[1], 2 * [1, 2][1])",
                "add((a * (b[2])), (b[1]), (2 * ([1, 2][1])))",
            ),
        ];

        for input in inputs {
            let mut parser = Parser::new(Lexer::new(input.0));
            let program = parser.parse_program();

            assert!(
                parser.errors().is_empty(),
                "input: {}, {:?}",
                input.0,
                parser.errors()
            );

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

            let Statement::Expression(expression) = &program.statements[0] else {
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

        let Statement::Expression(if_expression) = &program.statements[0] else {
            panic!("expected an ExpressionStatement. Got {:?}", program.statements[0]);
        };

        let Expression::If(condition, consequence, alternative) = if_expression else {
            panic!("expected an If(_, _, _). Got {:?}", if_expression);
        };

        assert!(test_infix_expression(condition, &"x", "<", &"y"));

        let Statement::Block(block_statements) = &**consequence else {
            panic!("expected a BlockStatement(_). Got {:?}", consequence);
        };

        assert_eq!(block_statements.len(), 1);
        let Statement::Expression(consequence_expression) = &block_statements[0] else {
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

        let Statement::Expression(if_expression) = &program.statements[0] else {
            panic!("expected an ExpressionStatement. Got {:?}", program.statements[0]);
        };

        let Expression::If(condition, consequence, alternative) = if_expression else {
            panic!("expected an If(_, _, _). Got {:?}", if_expression);
        };

        assert!(test_infix_expression(condition, &"x", "<", &"y"));

        let Statement::Block(block_statements) = &**consequence else {
            panic!("expected a BlockStatement(_). Got {:?}", consequence);
        };

        assert_eq!(block_statements.len(), 1);
        let Statement::Expression(consequence_expression) = &block_statements[0] else {
            panic!("expected an ExpressionStatement. Got {:?}", block_statements[0]);
        };
        assert!(test_literal_expression(consequence_expression, &"x"));

        let Some(alternative) = alternative else {
            panic!("expected an else block. Got {:?}", alternative);
        };

        let Statement::Block(else_statements) = &**alternative else {
            panic!("expected a BlockStatement(_). Got {:?}", alternative);
        };

        let Statement::Expression(alterative_expression) = &else_statements[0] else {
            panic!("expected an ExpressionStaement. Got {:?}", else_statements[0]);
        };

        assert!(test_literal_expression(alterative_expression, &"y"));
    }

    #[test]
    fn test_function_parsing() {
        let input = "fn(x, y) { x + y }";
        let mut parser = Parser::new(Lexer::new(input));
        let program = parser.parse_program();

        assert!(parser.errors().is_empty(), "{:?}", parser.errors());

        assert_eq!(program.statements.len(), 1, "{:?}", program.statements);

        let Statement::Expression(expression) = &program.statements[0] else {
            panic!("expected an ExpressionStatement. Got {:?}", program.statements[0]);
        };

        let Expression::Function(params, body) = expression else {
            panic!("exptected a Function(_,_). Got {:?}", expression);
        };

        assert_eq!(params.len(), 2, "{:?}", params);
        assert!(test_literal_expression(&params[0], &"x"));
        assert!(test_literal_expression(&params[1], &"y"));

        let Statement::Block(block) = &**body else {
            panic!("expected a BlockStatement. Got {:?}", body);
        };

        assert_eq!(block.len(), 1, "{:?}", block);
        let Statement::Expression(infix) = &block[0] else {
            panic!("expected an ExpressionStatement. Got {:?}", block[0]);
        };
        assert!(test_infix_expression(infix, &"x", "+", &"y"));
    }

    #[test]
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

            let Statement::Expression(expression) = &program.statements[0] else {
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

    #[test]
    fn test_call_parsing() {
        let input = "add(1, 2 * 3, 4 + 5);";
        let mut parser = Parser::new(Lexer::new(input));
        let program = parser.parse_program();

        assert!(parser.errors().is_empty(), "{:?}", parser.errors());
        assert_eq!(program.statements.len(), 1, "{:?}", program.statements);

        let Statement::Expression(expression) = &program.statements[0] else {
            panic!("expected an ExpressionStament. Got {:?}", program.statements[0]);
        };

        let Expression::Call(ident, args) = expression else {
            panic!("expected a Call(_,_). Got {:?}", expression);
        };

        assert!(test_literal_expression(ident, &"add"));
        assert_eq!(args.len(), 3, "{:?}", args);
        assert!(test_literal_expression(&args[0], &1));
        assert!(test_infix_expression(&args[1], &2, "*", &3));
        assert!(test_infix_expression(&args[2], &4, "+", &5));
    }

    #[test]
    fn test_string_literal_expression() {
        let input = r#""hello world""#;

        let mut parser = Parser::new(Lexer::new(input));
        let program = parser.parse_program();

        assert_eq!(parser.errors().len(), 0, "{:?}", parser.errors());

        let Statement::Expression(expr) = &program.statements[0] else {
            panic!("Expected an Expression Statement. Got {:?}", program.statements[0]);
        };

        let Expression::String(string) = expr else {
            panic!("Expected a String Expression. Got {:?}", expr);
        };

        assert_eq!(string.as_str(), "hello world");
    }

    #[test]
    fn test_parsing_arrays() {
        let input = "[1, 2 * 2, 3 + 3]";
        let mut parser = Parser::new(Lexer::new(input));
        let program = parser.parse_program();

        assert_eq!(parser.errors().len(), 0, "{:?}", parser.errors());

        let Some(Statement::Expression(expression)) =  program.statements.get(0) else {
            panic!("Expected an Expression Statement. Got {:?}", program.statements[0]);
        };

        let Expression::Array(elements) = expression else {
            panic!("Expected an Array. Got {:?}", expression);
        };

        1i64.test_expression(&elements[0]);
        test_infix_expression(&elements[1], &2, "*", &2);
        test_infix_expression(&elements[2], &3, "+", &3);
    }

    #[test]
    fn test_index_expressions() {
        let input = "myArray[1 + 1]";

        let mut parser = Parser::new(Lexer::new(input));
        let program = parser.parse_program();

        assert_eq!(parser.errors().len(), 0, "{:?}", parser.errors());

        let Some(Statement::Expression(expression)) =  program.statements.get(0) else {
            panic!("Expected an Expression Statement. Got {:?}", program.statements[0]);
        };

        let Expression::Index(left, index) = expression else {
            panic!("Expected an Array. Got {:?}", expression);
        };

        "myArray".test_expression(left);
        test_infix_expression(index, &1, "+", &1);
    }
}
