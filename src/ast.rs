use std::fmt::Display;

use crate::token::TokenKind;

#[derive(Debug)]
pub struct Identifier(pub String);

#[derive(Debug)]
pub enum Statement {
    LetStatement(Identifier, Expression),
    ReturnStatement(Expression),
    ExpressionStatement(Expression),
    BlockStatement(Vec<Box<Statement>>),
}

impl Display for Statement {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let matched = match self {
            Statement::LetStatement(ident, value) => format!("let {} = {value};", ident.0),
            Statement::ReturnStatement(value) => format!("return {value};"),
            Statement::ExpressionStatement(value) => value.to_string(),
            Statement::BlockStatement(statements) => {
                let mut buf = String::new();
                for statement in statements {
                    buf.push_str(&statement.to_string())
                }
                buf
            }
        };
        write!(f, "{matched}")
    }
}

#[derive(Debug)]
pub enum Expression {
    Identifier(Identifier),
    Integer(usize),
    Boolean(bool),

    If(Box<Expression>, Box<Statement>, Option<Box<Statement>>),

    Prefix(TokenKind, Box<Expression>),
    Infix(Box<Expression>, TokenKind, Box<Expression>),

    Placeholder,
}

impl Display for Expression {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let matched = match self {
            Expression::Placeholder => "PLACEHOLDER".to_string(),
            Expression::Identifier(ident) => ident.0.clone(),
            Expression::Integer(int) => int.to_string(),
            Expression::Boolean(bool) => bool.to_string(),
            Expression::If(condition, consequence, maybe_alterative) => {
                let mut buf = format!("if {condition} {consequence}");
                if let Some(alternative) = maybe_alterative {
                    buf.push_str("else ");
                    buf.push_str(&alternative.to_string())
                }
                buf
            }
            Expression::Prefix(token, expr) => format!("({token}{expr})"),
            Expression::Infix(left, token, right) => format!("({left} {token} {right})"),
        };
        write!(f, "{matched}")
    }
}

pub struct Program {
    pub statements: Vec<Statement>,
}

impl Program {
    pub fn new() -> Self {
        Self {
            statements: Vec::new(),
        }
    }
}

impl Program {
    pub fn token_literal(&self) -> String {
        let Some(statement) = self.statements.get(0) else {
            return String::new();
        };
        todo!()
    }
}

impl Display for Program {
    fn fmt(&self, f: &mut core::fmt::Formatter<'_>) -> core::fmt::Result {
        let statements = self
            .statements
            .iter()
            .map(|s| s.to_string())
            .collect::<Vec<_>>()
            .join("");

        write!(f, "{}", statements)
    }
}
