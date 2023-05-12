#[derive(Debug)]
pub struct Identifier(pub String);

#[derive(Debug)]
pub enum Statement {
    LetStatement(Identifier, Expression),
    ReturnStatement(Expression),
    ExpressionStatement,
}

#[derive(Debug)]
pub enum Expression {
    Placeholder,
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
