use crate::ast::{Expression, Program, Statement};
use crate::object::{Object, ObjectKind};

trait Eval {
    fn eval(&self) -> Option<Object>;
}

impl Eval for Program {
    fn eval(&self) -> Option<Object> {
        let mut result = None;
        for statement in &self.statements {
            result = statement.eval()
        }
        result
    }
}

impl Eval for Expression {
    fn eval(&self) -> Option<Object> {
        let obj = match self {
            Expression::Integer(int) => Object::new(ObjectKind::Integer(*int)),
            _ => todo!(),
        };

        Some(obj)
    }
}

impl Eval for Statement {
    fn eval(&self) -> Option<Object> {
        match self {
            Statement::Expression(expr) => expr.eval(),
            _ => todo!(),
        }
    }
}

#[cfg(test)]
mod tests {
    use crate::{lexer::Lexer, parser::Parser};

    use super::*;

    fn test_eval(input: &str) -> Object {
        let mut parser = Parser::new(Lexer::new(input));
        let program = parser.parse_program();

        program
            .eval()
            .unwrap_or_else(|| Object::new(ObjectKind::Null))
    }

    fn test_integer_object(object: &Object, expected: usize) -> bool {
        let ObjectKind::Integer(int) = object.kind else {
            eprintln!("object is not an Integer. Got {:?}", object.kind);
            return false;
        };

        int == expected
    }

    #[test]
    fn test_eval_integer_expressions() {
        let inputs: Vec<(&str, usize)> = vec![("5", 5), ("10", 10)];

        for input in inputs {
            let evaluated = test_eval(input.0);
            assert!(test_integer_object(&evaluated, input.1));
        }
    }
}
