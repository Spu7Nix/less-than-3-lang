use fnv::FnvHashMap;
use internment::Intern;

use crate::{
    builtin::{run_builtin, Builtin},
    parser::{Expression, Item, Token},
};

#[derive(Debug, Clone, PartialEq)]
enum Function {
    Builtin(Builtin),
    Plus(Value),
    Minus(Value),
    Mult(Value),
    Div(Value),

    Gt(Value),
    Lt(Value),
    Gte(Value),
    Lte(Value),
    Eq(Value),
    Neq(Value),

    Apply(Value),
}

impl Function {
    fn call(&self, arg: &Value, state: &mut State) -> Value {
        match (arg, self) {
            (arg, Function::Builtin(builtin)) => run_builtin(*builtin, arg),
            (Value::Number(n), Function::Plus(Value::Number(n2))) => Value::Number(n + n2),
            (Value::Number(n), Function::Minus(Value::Number(n2))) => Value::Number(n - n2),
            (Value::Number(n), Function::Mult(Value::Number(n2))) => Value::Number(n * n2),
            (Value::Number(n), Function::Div(Value::Number(n2))) => Value::Number(n / n2),

            (Value::Number(n), Function::Gt(Value::Number(n2))) => Value::Bool(n > n2),
            (Value::Number(n), Function::Lt(Value::Number(n2))) => Value::Bool(n < n2),
            (Value::Number(n), Function::Gte(Value::Number(n2))) => Value::Bool(n >= n2),
            (Value::Number(n), Function::Lte(Value::Number(n2))) => Value::Bool(n <= n2),

            (arg, Function::Eq(v)) => Value::Bool(arg == v),
            (arg, Function::Neq(v)) => Value::Bool(arg == v),

            (Value::Function(f), Function::Apply(val)) => f.call(val, state),

            a => panic!("Invalid function call: {:?} {:?}", a.0, a.1),
        }
    }
}

#[derive(Debug, Clone, PartialEq)]
pub enum Value {
    Number(f64),
    String(String),
    Bool(bool),
    Symbol(Intern<String>),
    Tuple(Vec<Value>),
    Function(Box<Function>),
}

#[derive(Debug, Clone, PartialEq)]
pub enum LazyValue {
    Evaled(Value),
    NotEvaled(Expression),
}

impl LazyValue {
    pub fn to_value(self, state: &mut State) -> Value {
        match self {
            LazyValue::Evaled(v) => v,
            LazyValue::NotEvaled(e) => eval_expression(e, state),
        }
    }
}

fn eval_expression(e: Expression, state: &mut State) -> Value {
    use Expression::*;
    match e {
        Symbol(s) => state.definitions[&s].clone().to_value(state),
        Number(n) => Value::Number(n),
        StringLiteral(s) => Value::String(s),
        Boolean(b) => Value::Bool(b),
        List(_) => todo!(),
        FunctionCall(arg, func) => {
            let arg = eval_expression(*arg, state);
            let func = eval_expression(*func, state);
            match func {
                Value::Function(f) => f.call(&arg, state),
                _ => panic!("Expected function"),
            }
        }
        Operator(op, expr) => {
            use Function::*;
            let val = eval_expression(*expr, state);
            Value::Function(Box::new(match op {
                Token::Plus => Plus(val),
                Token::Minus => Minus(val),
                Token::Slash => Div(val),
                Token::Star => Mult(val),

                Token::MoreThan => Gt(val),
                Token::LessThan => Lt(val),
                Token::MoreOrEqual => Gte(val),
                Token::LessOrEqual => Lte(val),
                Token::Equal => Eq(val),
                Token::NotEqual => Neq(val),

                Token::Colon => Apply(val),

                _ => panic!("Invalid operator"),
            }))
        }
    }
}

#[derive(Default)]
pub struct State {
    pub definitions: FnvHashMap<Intern<String>, LazyValue>,
}

pub fn interpret(items: Vec<Item>) -> Value {
    let mut state = State::default();
    for item in items {
        match item {
            Item::Definition { name, value } => {
                state.definitions.insert(name, LazyValue::NotEvaled(value));
            }
        }
    }
    state.definitions[&Intern::new("main".to_string())]
        .clone()
        .to_value(&mut state)
}
