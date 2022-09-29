use fnv::FnvHashMap;
use internment::Intern;

use crate::{
    builtin::{run_builtin, Builtin},
    parser::{Expression, Item, Token},
};

#[derive(Debug, Clone, PartialEq)]
pub enum Function {
    Builtin(Builtin),
    Plus(Value),
    Minus(Value),
    Mult(Value),
    Div(Value),
    Mod(Value),

    Gt(Value),
    Lt(Value),
    Gte(Value),
    Lte(Value),
    Eq(Value),
    Neq(Value),

    And(Value),
    Or(Value),

    //Compose(Value),
    Composit(Box<Function>, Box<Function>),

    Apply(Value),
}

impl Function {
    fn call(&self, arg: &Value, state: &mut State) -> Value {
        let temp = state.arg.clone();
        state.arg = Some(arg.clone());
        let out = match (arg, self) {
            (arg, Function::Builtin(builtin)) => run_builtin(*builtin, arg),
            (Value::Number(n), Function::Plus(Value::Number(n2))) => Value::Number(n + n2),

            (Value::String(s), Function::Plus(Value::String(s2))) => Value::String(s.clone() + s2),
            (Value::String(s), Function::Mult(Value::Number(n))) => {
                Value::String(s.repeat(*n as usize))
            }
            (Value::String(s), Function::Div(Value::String(s2))) => {
                Value::Tuple(s.split(s2).map(|s| Value::String(s.to_string())).collect())
            }

            (Value::Tuple(t), Function::Plus(Value::Tuple(t2))) => Value::Tuple(
                t.clone()
                    .into_iter()
                    .chain(t2.clone().into_iter())
                    .collect(),
            ),
            (Value::Tuple(t), Function::Mult(Value::Function(f))) => {
                Value::Tuple(t.clone().into_iter().map(|v| f.call(&v, state)).collect())
            }
            (Value::Tuple(t), Function::Minus(Value::Function(f))) => Value::Tuple(
                t.clone()
                    .into_iter()
                    .filter(|v| match f.call(&v, state) {
                        Value::Bool(b) => !b,
                        _ => false,
                    })
                    .collect(),
            ),
            // reduce
            (Value::Tuple(t), Function::Div(Value::Function(f))) => {
                let mut iter = t.clone().into_iter();
                let mut acc = iter.next().unwrap();
                for v in iter {
                    acc = match f.call(&v, state) {
                        Value::Function(f) => f.call(&acc, state),
                        _ => panic!("Expected function"),
                    }
                }
                acc
            }

            (Value::Number(n), Function::Minus(Value::Number(n2))) => Value::Number(n - n2),
            (Value::Number(n), Function::Mult(Value::Number(n2))) => Value::Number(n * n2),
            (Value::Number(n), Function::Div(Value::Number(n2))) => Value::Number(n / n2),
            (Value::Number(n), Function::Mod(Value::Number(n2))) => Value::Number(n % n2),

            (Value::Number(n), Function::Gt(Value::Number(n2))) => Value::Bool(n > n2),
            (Value::Number(n), Function::Lt(Value::Number(n2))) => Value::Bool(n < n2),
            (Value::Number(n), Function::Gte(Value::Number(n2))) => Value::Bool(n >= n2),
            (Value::Number(n), Function::Lte(Value::Number(n2))) => Value::Bool(n <= n2),

            (Value::Bool(b1), Function::And(Value::Bool(b2))) => Value::Bool(*b1 && *b2),
            (Value::Bool(b1), Function::Or(Value::Bool(b2))) => Value::Bool(*b1 || *b2),

            (arg, Function::Eq(v)) => Value::Bool(arg == v),
            (arg, Function::Neq(v)) => Value::Bool(arg == v),

            (Value::Function(f), Function::Apply(val)) => f.call(val, state),
            (Value::Function(f), Function::Plus(Value::Function(f2))) => {
                Value::Function(Box::from(Function::Composit(f.clone(), f2.clone())))
            }

            (arg, Function::Composit(f1, f2)) => f2.call(&f1.call(arg, state), state),

            a => panic!("Invalid function call: {:?} {:?}", a.0, a.1),
        };
        state.arg = temp;
        out
    }
}

#[derive(Debug, Clone, PartialEq)]
pub enum Value {
    Number(f64),
    String(String),
    Bool(bool),
    //Symbol(Intern<String>),
    Tuple(Vec<Value>),
    Function(Box<Function>),
}

impl std::fmt::Display for Value {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Value::Number(n) => write!(f, "{}", n),
            Value::String(s) => write!(f, "\"{}\"", s),
            Value::Bool(b) => write!(f, "{}", b),
            Value::Tuple(t) => {
                write!(f, "(")?;
                for (i, v) in t.iter().enumerate() {
                    write!(f, "{}", v)?;
                    if i != t.len() - 1 {
                        write!(f, ", ")?;
                    }
                }
                write!(f, ")")
            }
            Value::Function(box Function::Plus(v)) => write!(f, "+{}", v),
            Value::Function(box Function::Minus(v)) => write!(f, "-{}", v),
            Value::Function(box Function::Mult(v)) => write!(f, "*{}", v),
            Value::Function(box Function::Div(v)) => write!(f, "/{}", v),
            Value::Function(box Function::Mod(v)) => write!(f, "%{}", v),
            Value::Function(box Function::Gt(v)) => write!(f, ">{}", v),
            Value::Function(box Function::Lt(v)) => write!(f, "<{}", v),
            Value::Function(box Function::Gte(v)) => write!(f, ">={}", v),
            Value::Function(box Function::Lte(v)) => write!(f, "<={}", v),
            Value::Function(box Function::Eq(v)) => write!(f, "=={}", v),
            Value::Function(box Function::Neq(v)) => write!(f, "!={}", v),
            Value::Function(box Function::Apply(v)) => write!(f, "apply({})", v),
            Value::Function(box Function::Composit(f1, f2)) => {
                write!(
                    f,
                    "({})+({})",
                    Value::Function(f1.clone()),
                    Value::Function(f2.clone())
                )
            }
            _ => todo!(),
        }
    }
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
        Tuple(t) => {
            let t = t.into_iter().map(|e| eval_expression(e, state)).collect();
            Value::Tuple(t)
        }
        Argument => state.arg.clone().expect("No argument for this function"),
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
                Token::Modulo => Mod(val),

                Token::MoreThan => Gt(val),
                Token::LessThan => Lt(val),
                Token::MoreOrEqual => Gte(val),
                Token::LessOrEqual => Lte(val),
                Token::Equal => Eq(val),
                Token::NotEqual => Neq(val),

                Token::Colon => Apply(val),

                Token::And => And(val),
                Token::Or => Or(val),

                //Token::Period => Compose(val),
                a => panic!("Invalid operator: {:?}", a),
            }))
        }
    }
}

#[derive(Default)]
pub struct State {
    pub definitions: FnvHashMap<Intern<String>, LazyValue>,
    pub arg: Option<Value>,
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
