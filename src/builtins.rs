use crate::object::{new_error, Object};

#[derive(Debug, Clone, Copy)]
pub enum BuiltinFunction {
    Len,
    First,
    Last,
    Rest,
    Push,
}

impl BuiltinFunction {
    pub fn call(&self, args: &[Object]) -> Object {
        match self {
            BuiltinFunction::Len => BuiltinFunction::len(args),
            BuiltinFunction::First => BuiltinFunction::first(args),
            BuiltinFunction::Last => BuiltinFunction::last(args),
            BuiltinFunction::Rest => BuiltinFunction::rest(args),
            BuiltinFunction::Push => BuiltinFunction::push(args),
        }
    }

    fn is_wrong_arg_amount(len: usize, want: usize) -> Option<Object> {
        if len != want {
            return Some(new_error(format!(
                "wrong number of arguments. got={len}, want=1",
            )));
        }
        None
    }

    fn len(args: &[Object]) -> Object {
        if let Some(error) = Self::is_wrong_arg_amount(args.len(), 1) {
            return error;
        }

        match &args[0] {
            Object::String(string) => Object::Integer(string.len() as i64),
            Object::Array(arr) => Object::Integer(arr.len() as i64),
            got => new_error(format!("argument to `len` not supported, got {got}")),
        }
    }

    fn first(args: &[Object]) -> Object {
        if let Some(error) = Self::is_wrong_arg_amount(args.len(), 1) {
            return error;
        }

        match &args[0] {
            Object::Array(arr) => arr.first().unwrap_or(&Object::Null).clone(),
            got => new_error(format!("argument to `first` must be ARRAY, got {got}",)),
        }
    }

    fn last(args: &[Object]) -> Object {
        if let Some(error) = Self::is_wrong_arg_amount(args.len(), 1) {
            return error;
        }

        match &args[0] {
            Object::Array(arr) => arr.last().unwrap_or(&Object::Null).clone(),
            got => new_error(format!("argument to `first` must be ARRAY, got {got}",)),
        }
    }

    fn rest(args: &[Object]) -> Object {
        if let Some(error) = Self::is_wrong_arg_amount(args.len(), 1) {
            return error;
        }

        match &args[0] {
            Object::Array(arr) => {
                if arr.is_empty() {
                    return Object::Null;
                }

                let mut new_arr = vec![Object::Null; arr.len() - 1];
                new_arr.clone_from_slice(&arr[1..]);
                Object::Array(new_arr)
            }
            got => new_error(format!("argument to `first` must be ARRAY, got {got}",)),
        }
    }

    fn push(args: &[Object]) -> Object {
        if let Some(error) = Self::is_wrong_arg_amount(args.len(), 2) {
            return error;
        }

        match &args[0] {
            Object::Array(arr) => {
                let mut new_arr = arr.clone();
                new_arr.push(args[1].clone());
                Object::Array(new_arr)
            }
            got => new_error(format!("argument to `first` must be ARRAY, got {got}",)),
        }
    }
}
