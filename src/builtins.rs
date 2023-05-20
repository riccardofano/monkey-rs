use crate::object::{new_error, Object};

#[derive(Debug, Clone, Copy)]
pub enum BuiltinFunction {
    Len,
    First,
}

impl BuiltinFunction {
    pub fn call(&self, args: &[Object]) -> Object {
        match self {
            BuiltinFunction::Len => BuiltinFunction::len(args),
            BuiltinFunction::First => BuiltinFunction::first(args),
        }
    }

    fn len(args: &[Object]) -> Object {
        if args.len() != 1 {
            return new_error(format!(
                "wrong number of arguments. got={}, want=1",
                args.len()
            ));
        }

        match &args[0] {
            Object::String(string) => Object::Integer(string.len() as i64),
            Object::Array(arr) => Object::Integer(arr.len() as i64),
            got => new_error(format!("argument to `len` not supported, got {got}")),
        }
    }

    fn first(args: &[Object]) -> Object {
        if args.len() != 1 {
            return new_error(format!(
                "wrong number of arguments. got={}, want=1",
                args.len()
            ));
        }

        match &args[0] {
            Object::Array(arr) => arr.get(0).unwrap_or(&Object::Null).clone(),
            got => new_error(format!("argument to `first` must be ARRAY, got {got}",)),
        }
    }
}
