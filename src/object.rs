use std::fmt::Display;

pub const TRUE: Object = Object::Boolean(true);
pub const FALSE: Object = Object::Boolean(false);

#[derive(Debug)]
pub enum Object {
    Integer(i64),
    Boolean(bool),
    Null,
}

impl Object {
    pub fn inspect(&self) -> String {
        match self {
            Object::Integer(int) => int.to_string(),
            Object::Boolean(bool) => bool.to_string(),
            Object::Null => "null".to_string(),
        }
    }
}

impl Display for Object {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let kind = match self {
            Object::Integer(_) => "INTEGER",
            Object::Boolean(_) => "BOOLEAN",
            Object::Null => "NULL",
        };
        write!(f, "{kind}")
    }
}

impl From<&bool> for Object {
    fn from(value: &bool) -> Self {
        if *value {
            return TRUE;
        }
        FALSE
    }
}
