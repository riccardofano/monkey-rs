use std::collections::HashMap;

use crate::ast::{Expression, Identifier, Literal, Program, Statement};
use crate::builtins::BuiltinFunction;
use crate::object::{new_error, Env, Environment, Object, FALSE, TRUE};
use crate::token::TokenKind;

pub trait Eval {
    fn eval(&self, env: Env) -> Object;
}

impl Eval for Program {
    fn eval(&self, env: Env) -> Object {
        let mut result = Object::Null;
        for statement in &self.statements {
            result = statement.eval(env.clone());

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
    fn eval(&self, env: Env) -> Object {
        match self {
            Statement::Expression(expr) => expr.eval(env),
            Statement::Block(statements) => eval_block_statement(statements, env),
            Statement::Return(expr) => {
                let value = expr.eval(env);
                if value.is_error() {
                    return value;
                }
                Object::ReturnValue(Box::new(value))
            }
            Statement::Let(ident, expr) => {
                let value = expr.eval(env.clone());
                if value.is_error() {
                    return value;
                }
                env.borrow_mut().set(ident.clone(), value);
                Object::Null
            }
        }
    }
}

impl Eval for Expression {
    fn eval(&self, env: Env) -> Object {
        match self {
            Expression::Literal(literal) => literal.eval(env),
            Expression::Identifier(ident) => eval_identifier(ident, env),
            Expression::If(cond, cons, alt) => eval_if_expression(cond, cons, alt, env),
            Expression::Prefix(op, value) => {
                let value = value.eval(env);
                if value.is_error() {
                    return value;
                }
                eval_prefix_expression(op, value)
            }
            Expression::Infix(left, op, right) => {
                let left = left.eval(env.clone());
                if left.is_error() {
                    return left;
                }
                let right = right.eval(env);
                if right.is_error() {
                    return right;
                }
                eval_infix_expression(left, op, right)
            }
            Expression::Function(params, body) => {
                Object::Function(params.clone(), *body.clone(), env)
            }
            Expression::Call(ident, arguments) => {
                let function = ident.eval(env.clone());
                if function.is_error() {
                    return function;
                }
                let args = eval_expressions(arguments, env);
                match args {
                    Err(e) => e,
                    Ok(args) => apply_function(function, &args),
                }
            }
            Expression::Array(elements) => {
                let elements = eval_expressions(elements, env);
                match elements {
                    Err(e) => e,
                    Ok(elements) => Object::Array(elements),
                }
            }
            Expression::Hash(pairs) => {
                let mut map = HashMap::new();
                for (key, value) in pairs {
                    let key = key.eval(env.clone());
                    if key.is_error() {
                        return key;
                    }
                    let value = value.eval(env.clone());
                    if value.is_error() {
                        return value;
                    }
                    map.insert(key, value);
                }

                Object::Hash(map)
            }
            Expression::Index(left, index) => {
                let left = left.eval(env.clone());
                if left.is_error() {
                    // TODO: gonna change this to make use of the ? operator,
                    // we're going Go style for now.
                    return left;
                }
                let index = index.eval(env);
                if index.is_error() {
                    return index;
                }
                eval_index_expression(left, index)
            }
        }
    }
}

impl Eval for Literal {
    fn eval(&self, _env: Env) -> Object {
        match self {
            Literal::Integer(int) => Object::Integer(*int),
            Literal::String(string) => Object::String(string.clone()),
            Literal::Boolean(bool) => (*bool).into(),
        }
    }
}

fn eval_block_statement(statements: &[Statement], env: Env) -> Object {
    let mut result = Object::Null;
    for statement in statements {
        result = statement.eval(env.clone());
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

    let created = match (&left, &right) {
        (Object::Integer(left), Object::Integer(right)) => {
            eval_integer_infix_expression(*left, operator, *right)
        }
        (Object::Boolean(left), Object::Boolean(right)) => {
            eval_boolean_infix_expression(*left, operator, *right)
        }
        (Object::String(left), Object::String(right)) => {
            eval_string_infix_expression(left, operator, right)
        }
        (_, _) => None,
    };

    match created {
        Some(obj) => obj,
        None => new_error(format!("unknown operator: {left} {operator} {right}")),
    }
}

fn eval_integer_infix_expression(left: i64, operator: &TokenKind, right: i64) -> Option<Object> {
    match operator {
        TokenKind::Plus => Some(Object::Integer(left + right)),
        TokenKind::Minus => Some(Object::Integer(left - right)),
        TokenKind::Asterisk => Some(Object::Integer(left * right)),
        TokenKind::Slash => Some(Object::Integer(left / right)),
        TokenKind::LessThan => Some((left < right).into()),
        TokenKind::GreaterThan => Some((left > right).into()),
        TokenKind::Equal => Some((left == right).into()),
        TokenKind::NotEqual => Some((left != right).into()),
        _ => None,
    }
}

fn eval_boolean_infix_expression(left: bool, operator: &TokenKind, right: bool) -> Option<Object> {
    match operator {
        TokenKind::Equal => Some((left == right).into()),
        TokenKind::NotEqual => Some((left != right).into()),
        _ => None,
    }
}

fn eval_string_infix_expression(left: &str, operator: &TokenKind, right: &str) -> Option<Object> {
    match operator {
        TokenKind::Plus => Some(Object::String(format!("{left}{right}"))),
        _ => None,
    }
}

fn eval_if_expression(
    condition: &Expression,
    consequence: &Statement,
    alternative: &Option<Box<Statement>>,
    env: Env,
) -> Object {
    let condition = condition.eval(env.clone());
    if condition.is_error() {
        return condition;
    }

    if condition.is_truthy() {
        return consequence.eval(env);
    };

    match alternative {
        Some(alternative) => alternative.eval(env),
        None => Object::Null,
    }
}

fn eval_index_expression(left: Object, index: Object) -> Object {
    match (&left, &index) {
        (Object::Array(arr), Object::Integer(i)) => {
            arr.get(*i as usize).unwrap_or(&Object::Null).clone()
        }
        (Object::Hash(map), _) => {
            if !index.is_hashable() {
                return new_error(format!("unusable as hash key: {}", &index));
            }

            map.get(&index).unwrap_or(&Object::Null).clone()
        }
        (_, _) => new_error(format!("index operator not supported: {left}")),
    }
}

fn eval_expressions(expressions: &[Expression], env: Env) -> Result<Vec<Object>, Object> {
    let mut result = Vec::with_capacity(expressions.len());
    for expression in expressions {
        let evaluated = expression.eval(env.clone());
        if evaluated.is_error() {
            return Err(evaluated);
        }
        result.push(evaluated);
    }
    Ok(result)
}

fn apply_function(func: Object, args: &[Object]) -> Object {
    match func {
        Object::Function(params, body, env) => {
            let extended_env = extend_function_env(env, &params, args);
            let evaluated = body.eval(extended_env);
            if let Object::ReturnValue(value) = evaluated {
                return *value;
            }
            evaluated
        }
        Object::Builtin(func) => func.call(args),
        _ => new_error(format!("not a function: {func}")),
    }
}

fn extend_function_env(func_env: Env, params: &[Expression], args: &[Object]) -> Env {
    let mut env = Environment::new_enclosed(func_env);
    for (i, param) in params.iter().enumerate() {
        if let Expression::Identifier(name) = param {
            env.set(name.clone(), args[i].clone());
        }
    }
    env.into_env()
}

fn eval_identifier(identifier: &Identifier, env: Env) -> Object {
    if let Some(value) = env.borrow().get(identifier) {
        return value;
    }
    if let Some(builtin) = get_builtin(identifier) {
        return builtin;
    }

    new_error(format!("identifier not found: {identifier}"))
}

fn get_builtin(identifier: &Identifier) -> Option<Object> {
    let function = match identifier.0.as_str() {
        "len" => BuiltinFunction::Len,
        "first" => BuiltinFunction::First,
        "last" => BuiltinFunction::Last,
        "rest" => BuiltinFunction::Rest,
        "push" => BuiltinFunction::Push,
        "puts" => BuiltinFunction::Puts,
        _ => return None,
    };

    Some(Object::Builtin(function))
}

#[cfg(test)]
mod tests {
    use crate::{lexer::Lexer, object::Environment, parser::Parser};

    use super::*;

    fn test_eval(input: &str) -> Object {
        let mut parser = Parser::new(Lexer::new(input));
        let program = parser.parse_program();
        let env = Environment::new().into_env();

        program.eval(env)
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

    impl TestObject for &str {
        fn assert_object(&self, object: &Object) {
            match object {
                Object::Error(message) => assert_eq!(message, self),
                Object::String(string) => assert_eq!(string, self),
                _ => panic!("Expected Error or String Object. Got {:?}", object),
            }
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
            ("foobar", "identifier not found: foobar"),
            (r#""Hello" - "World""#, "unknown operator: STRING - STRING"),
            (
                r#"{"name": "Monkey"}[fn(x) { x }];"#,
                "unusable as hash key: FUNCTION",
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

    #[test]
    fn test_let_statements() {
        let inputs: Vec<(&str, i64)> = vec![
            ("let a = 5; a;", 5),
            ("let a = 5 * 5; a;", 25),
            ("let a = 5; let b = a; b;", 5),
            ("let a = 5; let b = a; let c = a + b + 5; c;", 15),
        ];

        for input in inputs {
            let evaluated = test_eval(input.0);
            input.1.assert_object(&evaluated);
        }
    }

    #[test]
    fn test_function_object() {
        let input = "fn(x) { x + 2; };";
        let evaluated = test_eval(input);

        let Object::Function(params, body, _env) = evaluated else {
            panic!("Expected Function Object. Got {:?}", evaluated);
        };

        if params.len() != 1 {
            panic!("Function has wrong parameters. Got {:?}", params);
        }

        assert_eq!(&params[0].to_string(), "x", "{:?}", params);
        assert_eq!(&body.to_string(), "(x + 2)", "{:?}", body);
    }

    #[test]
    fn test_function_application() {
        let inputs: Vec<(&str, i64)> = vec![
            ("let identity = fn(x) { x; }; identity(5);", 5),
            ("let identity = fn(x) { return x; }; identity(5);", 5),
            ("let double = fn(x) { x * 2; }; double(5);", 10),
            ("let add = fn(x, y) { x + y; }; add(5, 5);", 10),
            ("let add = fn(x, y) { x + y; }; add(5 + 5, add(5, 5));", 20),
            ("fn(x) { x; }(5)", 5),
        ];

        for input in inputs {
            input.1.assert_object(&test_eval(input.0));
        }
    }

    #[test]
    fn test_closures() {
        let input: (&str, i64) = (
            r#"
let newAdder = fn(x) {
  fn(y) { x + y };
};

let addTwo = newAdder(2);
addTwo(2);"#,
            4,
        );

        input.1.assert_object(&test_eval(input.0));
    }

    #[test]
    fn parse_string_literal() {
        let input = "\"Hello World!\"";

        let evaluated = test_eval(input);
        let Object::String(string) = evaluated else {
            panic!("Expected String Object. Got {:?}", evaluated);
        };

        assert_eq!(string, "Hello World!")
    }

    #[test]
    fn test_string_concatenation() {
        let input = r#""Hello" + " " + "World!""#;
        let evaluated = test_eval(input);

        let Object::String(string) = evaluated else {
            panic!("Expected String Object. Got {:?}", evaluated);
        };

        assert_eq!(string, "Hello World!")
    }

    #[test]
    fn test_builtin_functions() {
        let inputs: Vec<(&str, &dyn TestObject)> = vec![
            (r#"len("")"#, &0),
            (r#"len("four")"#, &4),
            (r#"len("hello world")"#, &11),
            (r#"len(1)"#, &"argument to `len` not supported, got INTEGER"),
            (
                r#"len("one", "two")"#,
                &"wrong number of arguments. got=2, want=1",
            ),
        ];

        for input in inputs {
            let evaluated = test_eval(input.0);
            input.1.assert_object(&evaluated)
        }
    }

    #[test]
    fn test_parsing_arrays() {
        let input = "[1, 2 * 2, 3 + 3]";
        let evaluated = test_eval(input);

        let Object::Array(elements) = evaluated else {
            panic!("Expected Array Object. Got {:?}", evaluated);
        };

        1i64.assert_object(&elements[0]);
        4i64.assert_object(&elements[1]);
        6i64.assert_object(&elements[2]);
    }

    #[test]
    fn test_array_index_expressions() {
        let inputs: Vec<(&str, &dyn TestObject)> = vec![
            ("[1, 2, 3][0]", &1),
            ("[1, 2, 3][1]", &2),
            ("[1, 2, 3][2]", &3),
            ("let i = 0; [1][i];", &1),
            ("[1, 2, 3][1 + 1];", &3),
            ("let myArray = [1, 2, 3]; myArray[2];", &3),
            (
                "let myArray = [1, 2, 3]; myArray[0] + myArray[1] + myArray[2];",
                &6,
            ),
            (
                "let myArray = [1, 2, 3]; let i = myArray[0]; myArray[i]",
                &2,
            ),
            ("[1, 2, 3][3]", &None),
            ("[1, 2, 3][-1]", &None),
        ];

        for input in inputs {
            let evaluated = test_eval(input.0);
            input.1.assert_object(&evaluated)
        }
    }

    #[test]
    fn test_hash_literal_values() {
        let input = r#"{
            "one": 10 - 9,
            "two": 1 + 1,
            "three": 6 / 2,
            4: 4,
            true: 5,
            false: 6
        }"#;

        let evaluated = test_eval(input);
        let Object::Hash(map) = evaluated else {
            panic!("Eval didn't return Hash. Got: {:?}", evaluated);
        };

        let expected: Vec<(Object, i64)> = vec![
            (Object::String("one".into()), 1),
            (Object::String("two".into()), 2),
            (Object::String("three".into()), 3),
            (Object::Integer(4), 4),
            (TRUE, 5),
            (FALSE, 6),
        ];

        assert_eq!(map.len(), expected.len());

        for (key, value) in expected {
            let actual_value = &map[&key];

            value.assert_object(actual_value);
        }
    }

    #[test]
    fn test_hash_expression_values() {
        let input = r#"let two = "two";
        {
            "one": 10 - 9,
            two: 1 + 1,
            "th" + "ree": 6 / 2,
            4: 4,
            true: 5,
            false: 6
        }"#;

        let evaluated = test_eval(input);
        let Object::Hash(map) = evaluated else {
            panic!("Eval didn't return Hash. Got: {:?}", evaluated);
        };

        let expected: Vec<(Object, i64)> = vec![
            (Object::String("one".into()), 1),
            (Object::String("two".into()), 2),
            (Object::String("three".into()), 3),
            (Object::Integer(4), 4),
            (TRUE, 5),
            (FALSE, 6),
        ];

        assert_eq!(map.len(), expected.len());

        for (key, value) in expected {
            let actual_value = &map[&key];

            value.assert_object(actual_value);
        }
    }

    #[test]
    fn test_hash_index_expressions() {
        let inputs: Vec<(&str, &dyn TestObject)> = vec![
            (r#"{"foo": 5}["foo"]"#, &5),
            (r#"{"foo": 5}["bar"]"#, &None),
            (r#"let key = "foo"; {"foo": 5}[key]"#, &5),
            (r#"{}["foo"]"#, &None),
            (r#"{5: 5}[5]"#, &5),
            (r#"{true: 5}[true]"#, &5),
            (r#"{false: 5}[false]"#, &5),
        ];

        for input in inputs {
            let evaluated = test_eval(input.0);
            input.1.assert_object(&evaluated);
        }
    }
}
