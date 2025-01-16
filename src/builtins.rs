use std::time::{SystemTime, UNIX_EPOCH};

use crate::bytecode_interpreter;
use crate::value;

/*
Arity checking is done in the interpreter prior to calling a builtin function.
*/

pub fn exp(
    _interp: &mut bytecode_interpreter::Interpreter,
    args: &[value::Value],
) -> Result<value::Value, String> {
    match args[0] {
        value::Value::Number(num) => Ok(value::Value::Number(num.exp())),
        _ => Err(format!(
            "Invalid call: expected number, got {:?}.",
            value::type_of(&args[0])
        )),
    }
}

pub fn sqrt(
    _interp: &mut bytecode_interpreter::Interpreter,
    args: &[value::Value],
) -> Result<value::Value, String> {
    match args[0] {
        value::Value::Number(num) => Ok(value::Value::Number(num.sqrt())),
        _ => Err(format!(
            "Invalid call: expected number, got {:?}.",
            value::type_of(&args[0])
        )),
    }
}

pub fn clock(
    _interp: &mut bytecode_interpreter::Interpreter,
    _args: &[value::Value],
) -> Result<value::Value, String> {
    let start = SystemTime::now();
    let since_the_epoch = start.duration_since(UNIX_EPOCH).unwrap();

    Ok(value::Value::Number(since_the_epoch.as_millis() as f64))
}
