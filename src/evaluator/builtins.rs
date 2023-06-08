use super::object::{new_error, Object};

pub type BuiltinFunction = fn(&[Object]) -> Object;

pub fn is_wrong_arg_amount(len: usize, want: usize) -> Option<Object> {
    if len != want {
        return Some(new_error(format!(
            "wrong number of arguments. got={len}, want=1",
        )));
    }
    None
}

pub fn len(args: &[Object]) -> Object {
    if let Some(error) = is_wrong_arg_amount(args.len(), 1) {
        return error;
    }

    match &args[0] {
        Object::String(string) => Object::Integer(string.len() as i64),
        Object::Array(arr) => Object::Integer(arr.len() as i64),
        got => new_error(format!("argument to `len` not supported, got {got}")),
    }
}

pub fn first(args: &[Object]) -> Object {
    if let Some(error) = is_wrong_arg_amount(args.len(), 1) {
        return error;
    }

    match &args[0] {
        Object::Array(arr) => arr.first().unwrap_or(&Object::Null).clone(),
        got => new_error(format!("argument to `first` must be ARRAY, got {got}",)),
    }
}

pub fn last(args: &[Object]) -> Object {
    if let Some(error) = is_wrong_arg_amount(args.len(), 1) {
        return error;
    }

    match &args[0] {
        Object::Array(arr) => arr.last().unwrap_or(&Object::Null).clone(),
        got => new_error(format!("argument to `first` must be ARRAY, got {got}",)),
    }
}

pub fn rest(args: &[Object]) -> Object {
    if let Some(error) = is_wrong_arg_amount(args.len(), 1) {
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

pub fn push(args: &[Object]) -> Object {
    if let Some(error) = is_wrong_arg_amount(args.len(), 2) {
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

pub fn puts(args: &[Object]) -> Object {
    args.iter().for_each(|arg| println!("{}", arg.inspect()));

    Object::Null
}
