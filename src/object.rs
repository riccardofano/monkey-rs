use std::fmt::Display;

pub const TRUE: Object = Object::Boolean(true);
pub const FALSE: Object = Object::Boolean(false);

#[derive(Debug)]
pub enum Object {
    Error(String),
    ReturnValue(Box<Object>),
    Integer(i64),
    Boolean(bool),
    Null,
}

impl Object {
    pub fn inspect(&self) -> String {
        match self {
            Object::Error(message) => format!("ERROR: {message}"),
            Object::ReturnValue(value) => value.to_string(),
            Object::Integer(int) => int.to_string(),
            Object::Boolean(bool) => bool.to_string(),
            Object::Null => "null".to_string(),
        }
    }

    pub fn is_truthy(&self) -> bool {
        !matches!(self, Object::Boolean(false) | Object::Null)
    }

    pub fn is_error(&self) -> bool {
        matches!(self, Object::Error(_))
    }
}

impl Display for Object {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let kind = match self {
            Object::Error(_) => "ERROR",
            Object::ReturnValue(_) => "RETURN_VALUE",
            Object::Integer(_) => "INTEGER",
            Object::Boolean(_) => "BOOLEAN",
            Object::Null => "NULL",
        };
        write!(f, "{kind}")
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
