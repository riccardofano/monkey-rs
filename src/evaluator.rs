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
            Expression::Infix(left, op, right) => {
                eval_infix_expression(left.eval(), op, right.eval())
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

fn eval_infix_expression(left: Object, operator: &TokenKind, right: Object) -> Object {
    match (left, right) {
        (Object::Integer(left_int), Object::Integer(right_int)) => {
            eval_integer_infix_expression(left_int, operator, right_int)
        }
        (_, _) => Object::Null,
    }
}

fn eval_integer_infix_expression(left: i64, operator: &TokenKind, right: i64) -> Object {
    let result = match operator {
        TokenKind::Plus => left + right,
        TokenKind::Minus => left - right,
        TokenKind::Asterisk => left * right,
        TokenKind::Slash => left / right,
        _ => return Object::Null,
    };
    Object::Integer(result)
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
        let inputs: Vec<(&str, i64)> = vec![
            ("5", 5),
            ("10", 10),
            ("-5", -5),
            ("-10", -10),
            ("5 + 5 + 5 + 5 - 10", 10),
            ("2 * 2 * 2 * 2 * 2", 32),
            ("-50 + 100 + -50", 0),
            ("5 * 2 + 10", 20),
            ("5 + 2 * 10", 25),
            ("20 + 2 * -10", 0),
            ("50 / 2 * 2 + 10", 60),
            ("2 * (5 + 10)", 30),
            ("3 * 3 * 3 + 10", 37),
            ("3 * (3 * 3) + 10", 37),
            ("(5 + 10 * 2 + 15 / 3) * 2 + -10", 50),
        ];

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
}
