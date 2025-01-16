use crate::bytecode;
use crate::bytecode_interpreter;
use crate::gc;

use std::cell::RefCell;
use std::fmt;
use std::rc::Rc;

#[derive(Clone)]
pub enum Upvalue {
    Open(usize),
    Closed(Value),
}

impl Upvalue {
    pub fn is_open(&self) -> bool {
        match self {
            Upvalue::Open(_) => true,
            Upvalue::Closed(_) => false,
        }
    }

    pub fn is_open_with_index(&self, index: usize) -> bool {
        match self {
            Upvalue::Open(idx) => index == *idx,
            Upvalue::Closed(_) => false,
        }
    }
}

#[derive(Default, Clone)]
pub struct Closure {
    pub function: bytecode::Function,
    pub upvalues: Vec<Rc<RefCell<Upvalue>>>,
}

#[derive(Clone, Debug)]
pub struct NativeFunction {
    pub arity: u8,
    pub name: String,
    pub func: fn(&mut bytecode_interpreter::Interpreter, &[Value]) -> Result<Value, String>,
}

#[derive(Clone, Debug)]
pub enum Value {
    Number(f64),
    String(gc::HeapId),
    Function(gc::HeapId),
    NativeFunction(NativeFunction),
    Nil,
}

impl fmt::Display for Value {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Value::Number(n) => write!(f, "{}", n),
            Value::String(id) => write!(f, "<string {:?}>", id), // Adjust formatting as needed
            Value::Function(id) => write!(f, "<function {:?}>", id), // Adjust formatting as needed
            Value::NativeFunction(_) => write!(f, "<native function>"),
            Value::Nil => write!(f, "nil"),
        }
    }
}

#[derive(Copy, Clone, Debug, Eq, PartialEq)]
pub enum Type {
    Number,
    String,
    Function,
    NativeFunction,
    Nil,
}

pub fn type_of(value: &Value) -> Type {
    match value {
        Value::Number(_) => Type::Number,
        Value::String(_) => Type::String,
        Value::Function(_) => Type::Function,
        Value::NativeFunction(_) => Type::NativeFunction,
        Value::Nil => Type::Nil,
    }
}
