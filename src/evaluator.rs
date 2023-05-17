use crate::ast::{Expression, Program, Statement};
use crate::object::{Object, FALSE, TRUE};
use crate::token::TokenKind;

pub trait Eval {
    fn eval(&self) -> Object;
}

impl Eval for Program {
    fn eval(&self) -> Object {
        let mut result = Object::Null;
        for statement in &self.statements {
            result = statement.eval()
        }
        result
    }
}

impl Eval for Expression {
    fn eval(&self) -> Object {
        match self {
            Expression::Integer(int) => Object::Integer(*int),
            Expression::Boolean(bool) => bool.into(),
            Expression::Prefix(op, value) => {
                let value = value.eval();
                eval_prefix_expression(op, value)
            }
            _ => todo!(),
        }
    }
}

fn eval_prefix_expression(operator: &TokenKind, value: Object) -> Object {
    match operator {
        TokenKind::Bang => eval_bang_operator(value),
        TokenKind::Minus => eval_minus_operator(value),
        _ => Object::Null,
    }
}

fn eval_bang_operator(value: Object) -> Object {
    match value {
        Object::Boolean(true) => FALSE,
        Object::Boolean(false) => TRUE,
        Object::Null => TRUE,
        _ => FALSE,
    }
}

fn eval_minus_operator(value: Object) -> Object {
    let Object::Integer(int) = value else {
        return Object::Null;
    };

    Object::Integer(-int)
}

impl Eval for Statement {
    fn eval(&self) -> Object {
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

        program.eval()
    }

    fn assert_integer_object(object: &Object, expected: i64) {
        let Object::Integer(int) = object else {
            panic!("object is not an Integer. Got {:?}", object);
        };

        assert_eq!(int, &expected);
    }

    fn assert_boolean_object(object: &Object, expected: bool) {
        let Object::Boolean(bool) = object else {
            panic!("object is not a Boolean. Got {:?}", object);
        };

        assert_eq!(bool, &expected);
    }

    #[test]
    fn test_eval_integer_expressions() {
        let inputs: Vec<(&str, i64)> = vec![("5", 5), ("10", 10)];

        for input in inputs {
            let evaluated = test_eval(input.0);
            assert_integer_object(&evaluated, input.1);
        }
    }

    #[test]
    fn test_eval_boolean_expression() {
        let inputs: Vec<(&str, bool)> = vec![("true", true), ("false", false)];

        for input in inputs {
            let evaluated = test_eval(input.0);
            assert_boolean_object(&evaluated, input.1);
        }
    }

    #[test]
    fn test_bang_operator() {
        let inputs: Vec<(&str, bool)> = vec![
            ("!true", false),
            ("!false", true),
            ("!5", false),
            ("!!true", true),
            ("!!false", false),
            ("!!5", true),
        ];

        for input in inputs {
            let evaluated = test_eval(input.0);
            assert_boolean_object(&evaluated, input.1)
        }
    }

    #[test]
    fn test_minus_operator() {
        let inputs: Vec<(&str, i64)> = vec![("5", 5), ("10", 10), ("-5", -5), ("-10", -10)];

        for input in inputs {
            let evaluated = test_eval(input.0);
            assert_integer_object(&evaluated, input.1)
        }
    }
}
