use std::{
    cell::RefCell,
    collections::{hash_map::DefaultHasher, HashMap},
    fmt::{Debug, Display},
    hash::{Hash, Hasher},
    rc::Rc,
};

use super::builtins::BuiltinFunction;
use crate::parser::ast::{Expression, Identifier, Statement};

pub const TRUE: Object = Object::Boolean(true);
pub const FALSE: Object = Object::Boolean(false);

#[derive(Clone)]
pub enum Object {
    Null,
    Error(String),
    Boolean(bool),
    Integer(i64),
    String(String),
    Array(Vec<Object>),
    Hash(HashMap<Object, Object>),
    ReturnValue(Box<Object>),
    Builtin(BuiltinFunction),
    Function(Vec<Expression>, Statement, Env),
}

impl Object {
    pub fn inspect(&self) -> String {
        match self {
            Object::Null => "null".to_string(),
            Object::Error(message) => format!("ERROR: {message}"),
            Object::Boolean(bool) => bool.to_string(),
            Object::Integer(int) => int.to_string(),
            Object::String(string) => string.clone(),
            Object::Array(elements) => {
                let elements = elements
                    .iter()
                    .map(|p| p.inspect())
                    .collect::<Vec<_>>()
                    .join(", ");
                format!("[{elements}]")
            }
            Object::Hash(map) => {
                let pairs = map
                    .iter()
                    .map(|(key, value)| format!("{}: {}", key.inspect(), value.inspect()))
                    .collect::<Vec<_>>()
                    .join(", ");
                format!("{{{pairs}}}")
            }
            Object::ReturnValue(value) => value.to_string(),
            Object::Builtin(_) => "builtin function".to_string(),
            Object::Function(params, body, _) => {
                let params = params
                    .iter()
                    .map(|p| p.to_string())
                    .collect::<Vec<_>>()
                    .join(", ");
                format!("fn({params}) {{\n{body}\n}}")
            }
        }
    }

    pub fn is_truthy(&self) -> bool {
        !matches!(self, Object::Boolean(false) | Object::Null)
    }

    pub fn is_error(&self) -> bool {
        matches!(self, Object::Error(_))
    }

    pub fn is_hashable(&self) -> bool {
        matches!(
            self,
            Object::String(_) | Object::Integer(_) | Object::Boolean(_)
        )
    }
}

impl Display for Object {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let kind = match self {
            Object::Null => "NULL",
            Object::Error(_) => "ERROR",
            Object::Boolean(_) => "BOOLEAN",
            Object::Integer(_) => "INTEGER",
            Object::String(_) => "STRING",
            Object::Array(_) => "ARRAY",
            Object::Hash(_) => "HASH",
            Object::ReturnValue(_) => "RETURN_VALUE",
            Object::Builtin(_) => "BUILTIN",
            Object::Function(_, _, _) => "FUNCTION",
        };
        write!(f, "{kind}")
    }
}

impl Debug for Object {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Null => write!(f, "Null"),
            Self::Error(arg0) => f.debug_tuple("Error").field(arg0).finish(),
            Self::Boolean(arg0) => f.debug_tuple("Boolean").field(arg0).finish(),
            Self::Integer(arg0) => f.debug_tuple("Integer").field(arg0).finish(),
            Self::String(arg0) => f.debug_tuple("String").field(arg0).finish(),
            Self::Array(arg0) => f.debug_tuple("Array").field(arg0).finish(),
            Self::Hash(arg0) => f.debug_tuple("Hash").field(arg0).finish(),
            Self::ReturnValue(arg0) => f.debug_tuple("ReturnValue").field(arg0).finish(),
            Self::Builtin(arg0) => f
                .debug_tuple("Builtin")
                .field(&(arg0 as *const BuiltinFunction))
                .finish(),
            Self::Function(arg0, arg1, arg2) => f
                .debug_tuple("Function")
                .field(arg0)
                .field(arg1)
                .field(arg2)
                .finish(),
        }
    }
}

impl From<bool> for Object {
    fn from(value: bool) -> Self {
        if value {
            return TRUE;
        }
        FALSE
    }
}

#[allow(clippy::all)]
impl Hash for Object {
    fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
        match self {
            Object::Boolean(bool) => bool.hash(state),
            Object::Integer(int) => int.hash(state),
            Object::String(string) => string.hash(state),
            Object::Null => 0.hash(state),
            Object::Error(error) => error.hash(state),
            Object::Array(elements) => elements.hash(state),
            Object::ReturnValue(value) => value.hash(state),
            Object::Hash(_) | Object::Builtin(_) | Object::Function(_, _, _) => "".hash(state),
        }
    }
}

impl PartialEq for Object {
    fn eq(&self, other: &Self) -> bool {
        let mut hasher = DefaultHasher::new();
        self.hash(&mut hasher);
        let hash_self = hasher.finish();

        let mut hasher = DefaultHasher::new();
        other.hash(&mut hasher);
        let hash_other = hasher.finish();

        hash_self == hash_other
    }
}
impl Eq for Object {}

pub fn new_error(reason: String) -> Object {
    Object::Error(reason)
}

pub type Env = Rc<RefCell<Environment>>;

#[derive(Debug)]
pub struct Environment {
    store: HashMap<Identifier, Object>,
    outer: Option<Env>,
}

impl Environment {
    pub fn new() -> Self {
        Self {
            store: HashMap::new(),
            outer: None,
        }
    }

    pub fn new_enclosed(outer: Env) -> Self {
        Self {
            store: HashMap::new(),
            outer: Some(outer),
        }
    }

    pub fn into_env(self) -> Env {
        Rc::new(RefCell::new(self))
    }

    pub fn get(&self, name: &Identifier) -> Option<Object> {
        let inner_value = self.store.get(name);
        match (inner_value, &self.outer) {
            (None, Some(outer)) => outer.borrow().get(name),
            (_, _) => inner_value.cloned(),
        }
    }

    pub fn set(&mut self, name: Identifier, value: Object) {
        self.store.insert(name, value);
    }
}

#[cfg(test)]
mod tests {
    use std::{collections::hash_map::DefaultHasher, hash::Hasher};

    use super::*;

    fn calculate_hash<T: Hash>(t: &T) -> u64 {
        let mut s = DefaultHasher::new();

        t.hash(&mut s);
        s.finish()
    }

    #[test]
    fn test_string_hashkey() {
        let hello1 = Object::String("Hello World".into());
        let hello2 = Object::String("Hello World".into());

        let diff1 = Object::String("My name is johnny".into());
        let diff2 = Object::String("My name is johnny".into());

        assert_eq!(calculate_hash(&hello1), calculate_hash(&hello2));
        assert_eq!(calculate_hash(&diff1), calculate_hash(&diff2));
    }
}
