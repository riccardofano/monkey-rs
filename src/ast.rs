use crate::token::Token;

#[derive(Debug)]
struct Identifier {
    token: Token,
}

#[derive(Debug)]
pub enum Statement {
    LetStatement(Token, Identifier, Expression),
}

#[derive(Debug)]
pub enum Expression {}

pub struct Program {
    statements: Vec<Statement>,
}

impl Program {
    pub fn token_literal(&self) -> String {
        let Some(statement) = self.statements.get(0) else {
            return String::new();
        };
        todo!()
    }
}
