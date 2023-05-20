use crate::object::{new_error, Object};

#[derive(Debug, Clone, Copy)]
pub enum BuiltinFunction {
    Len,
}

impl BuiltinFunction {
    pub fn call(&self, args: &[Object]) -> Object {
        match self {
            BuiltinFunction::Len => BuiltinFunction::len(args),
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
            got => new_error(format!("argument to `len` not supported, got {got}")),
        }
    }
}
