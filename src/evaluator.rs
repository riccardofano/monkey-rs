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
            result = statement.eval();

            match result {
                Object::ReturnValue(value) => return *value,
                Object::Error(_) => return result,
                _ => {}
            }
        }
        result
    }
}

impl Eval for Statement {
    fn eval(&self) -> Object {
        match self {
            Statement::Expression(expr) => expr.eval(),
            Statement::Block(statements) => eval_block_statement(statements),
            Statement::Return(expr) => {
                let value = expr.eval();
                if value.is_error() {
                    return value;
                }
                Object::ReturnValue(Box::new(value))
            }
            _ => todo!(),
        }
    }
}

impl Eval for Expression {
    fn eval(&self) -> Object {
        match self {
            Expression::Integer(int) => Object::Integer(*int),
            Expression::Boolean(bool) => (*bool).into(),
            Expression::Prefix(op, value) => {
                let value = value.eval();
                if value.is_error() {
                    return value;
                }
                eval_prefix_expression(op, value)
            }
            Expression::Infix(left, op, right) => {
                let left = left.eval();
                if left.is_error() {
                    return left;
                }
                let right = right.eval();
                if right.is_error() {
                    return right;
                }
                eval_infix_expression(left, op, right)
            }
            Expression::If(cond, cons, alt) => eval_if_expression(cond, cons, alt),
            _ => todo!(),
        }
    }
}

fn new_error(reason: String) -> Object {
    Object::Error(reason)
}

fn eval_block_statement(statements: &[Statement]) -> Object {
    let mut result = Object::Null;
    for statement in statements {
        result = statement.eval();
        if matches!(result, Object::ReturnValue(_) | Object::Error(_)) {
            return result;
        }
    }
    result
}

fn eval_prefix_expression(operator: &TokenKind, value: Object) -> Object {
    match operator {
        TokenKind::Bang => eval_bang_operator(value),
        TokenKind::Minus => eval_minus_operator(value),
        _ => new_error(format!("unknown operator: {operator}{value}")),
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
        return new_error(format!("unknown operator: -{value}"))
    };

    Object::Integer(-int)
}

fn eval_infix_expression(left: Object, operator: &TokenKind, right: Object) -> Object {
    if std::mem::discriminant(&left) != std::mem::discriminant(&right) {
        return new_error(format!("type mismatch: {left} {operator} {right}"));
    }

    match (&left, &right) {
        (Object::Integer(left), Object::Integer(right)) => {
            eval_integer_infix_expression(*left, operator, *right)
        }
        (Object::Boolean(left), Object::Boolean(right)) => {
            eval_boolean_infix_expression(*left, operator, *right)
        }
        (_, _) => new_error(format!("unknown operator: {left} {operator} {right}")),
    }
}

fn eval_integer_infix_expression(left: i64, operator: &TokenKind, right: i64) -> Object {
    match operator {
        TokenKind::Plus => Object::Integer(left + right),
        TokenKind::Minus => Object::Integer(left - right),
        TokenKind::Asterisk => Object::Integer(left * right),
        TokenKind::Slash => Object::Integer(left / right),
        TokenKind::LessThan => (left < right).into(),
        TokenKind::GreaterThan => (left > right).into(),
        TokenKind::Equal => (left == right).into(),
        TokenKind::NotEqual => (left != right).into(),
        _ => new_error(format!(
            "unknown operator: {} {operator} {}",
            Object::Integer(left),
            Object::Integer(right)
        )),
    }
}

fn eval_boolean_infix_expression(left: bool, operator: &TokenKind, right: bool) -> Object {
    match operator {
        TokenKind::Equal => (left == right).into(),
        TokenKind::NotEqual => (left != right).into(),
        _ => new_error(format!(
            "unknown operator: {} {operator} {}",
            Object::Boolean(left),
            Object::Boolean(right)
        )),
    }
}

fn eval_if_expression(
    condition: &Expression,
    consequence: &Statement,
    alternative: &Option<Box<Statement>>,
) -> Object {
    let condition = condition.eval();
    if condition.is_error() {
        return condition;
    }

    if condition.is_truthy() {
        return consequence.eval();
    };

    match alternative {
        Some(alternative) => alternative.eval(),
        None => Object::Null,
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

    trait TestObject {
        fn assert_object(&self, object: &Object);
    }

    impl TestObject for i64 {
        fn assert_object(&self, object: &Object) {
            let Object::Integer(int) = object else {
                panic!("object is not an Integer. Got {:?}", object);
            };
            assert_eq!(int, self);
        }
    }

    impl TestObject for bool {
        fn assert_object(&self, object: &Object) {
            let Object::Boolean(bool) = object else {
                panic!("object is not a Boolean. Got {:?}", object);
            };
            assert_eq!(bool, self);
        }
    }

    // NOTE: any option value let it be Some(5) or None just checks if the object is null
    impl TestObject for Option<i64> {
        fn assert_object(&self, object: &Object) {
            let Object::Null = object else {
                panic!("object is not Null. Got {:?}", object);
            };
        }
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
            input.1.assert_object(&evaluated);
        }
    }

    #[test]
    fn test_eval_boolean_expression() {
        let inputs: Vec<(&str, bool)> = vec![
            ("true", true),
            ("false", false),
            ("1 < 2", true),
            ("1 > 2", false),
            ("1 < 1", false),
            ("1 > 1", false),
            ("1 == 1", true),
            ("1 != 1", false),
            ("1 == 2", false),
            ("1 != 2", true),
            ("true == true", true),
            ("false == false", true),
            ("true == false", false),
            ("true != false", true),
            ("false != true", true),
            ("(1 < 2) == true", true),
            ("(1 < 2) == false", false),
            ("(1 > 2) == true", false),
            ("(1 > 2) == false", true),
        ];

        for input in inputs {
            let evaluated = test_eval(input.0);
            input.1.assert_object(&evaluated);
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
            input.1.assert_object(&evaluated);
        }
    }

    #[test]
    fn test_if_else_expressions() {
        let inputs: Vec<(&str, &dyn TestObject)> = vec![
            ("if (true) { 10 }", &10),
            ("if (false) { 10 }", &None),
            ("if (1) { 10 }", &10),
            ("if (1 < 2) { 10 }", &10),
            ("if (1 > 2) { 10 }", &None),
            ("if (1 > 2) { 10 } else { 20 }", &20),
            ("if (1 < 2) { 10 } else { 20 }", &10),
        ];

        for input in inputs {
            let evaluated = test_eval(input.0);
            input.1.assert_object(&evaluated);
        }
    }

    #[test]
    fn test_return_statements() {
        let inputs: Vec<(&str, i64)> = vec![
            ("return 10;", 10),
            ("return 10; 9;", 10),
            ("return 2 * 5; 9;", 10),
            ("9; return 2 * 5; 9;", 10),
            ("if (10 > 1) { if (10 > 1) { return 10; } return 1; }", 10),
        ];

        for input in inputs {
            let evaluated = test_eval(input.0);
            input.1.assert_object(&evaluated);
        }
    }

    #[test]
    fn test_error_handling() {
        let inputs: Vec<(&str, &str)> = vec![
            ("5 + true;", "type mismatch: INTEGER + BOOLEAN"),
            ("5 + true; 5;", "type mismatch: INTEGER + BOOLEAN"),
            ("-true", "unknown operator: -BOOLEAN"),
            ("true + false;", "unknown operator: BOOLEAN + BOOLEAN"),
            ("5; true + false; 5", "unknown operator: BOOLEAN + BOOLEAN"),
            (
                "if (10 > 1) { true + false; }",
                "unknown operator: BOOLEAN + BOOLEAN",
            ),
            (
                " if (10 > 1) { if (10 > 1) { return true + false; } return 1; }",
                "unknown operator: BOOLEAN + BOOLEAN",
            ),
        ];

        for input in inputs {
            let evaluated = test_eval(input.0);
            let Object::Error(error_message) = evaluated else {
                panic!("Expected Error Object. Got {:?} from {}", evaluated, input.0);
            };

            assert_eq!(&error_message, input.1);
        }
    }
}
