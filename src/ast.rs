use std::fmt::Display;

use crate::token::TokenKind;

#[derive(Debug, Hash, PartialEq, Eq, Clone)]
pub struct Identifier(pub String);

impl Display for Identifier {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.0)
    }
}

#[derive(Debug)]
pub enum Statement {
    Let(Identifier, Expression),
    Return(Expression),
    Expression(Expression),
    Block(Vec<Statement>),
}

impl Display for Statement {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let matched = match self {
            Statement::Let(ident, value) => format!("let {ident} = {value};"),
            Statement::Return(value) => format!("return {value};"),
            Statement::Expression(value) => value.to_string(),
            Statement::Block(statements) => {
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
    Integer(i64),
    Boolean(bool),

    If(Box<Expression>, Box<Statement>, Option<Box<Statement>>),
    Function(Vec<Expression>, Box<Statement>),
    Call(Box<Expression>, Vec<Expression>),

    Prefix(TokenKind, Box<Expression>),
    Infix(Box<Expression>, TokenKind, Box<Expression>),
}

impl Display for Expression {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let matched = match self {
            Expression::Identifier(ident) => ident.to_string(),
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
            Expression::Function(params, body) => {
                let params = params
                    .iter()
                    .map(|expr| expr.to_string())
                    .collect::<Vec<_>>()
                    .join(", ");
                format!("fn({params}) {body}")
            }
            Expression::Call(ident, args) => {
                let args = args
                    .iter()
                    .map(|expr| expr.to_string())
                    .collect::<Vec<_>>()
                    .join(", ");
                format!("{ident}({args})")
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
