use fnv::FnvHashMap;
use internment::Intern;

use crate::{
    builtin::{run_builtin, Builtin},
    parser::{Expression, Item, Symbol, Token},
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

    Replace(Value),
    Range(Value),

    Cases(Vec<ArgCase>),

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
            (arg, Function::Cases(cases)) => {
                let mut out = Value::Tuple(Vec::new());
                for case in cases {
                    match case {
                        ArgCase::Value(val, body) => {
                            if arg == val {
                                out = body.clone().to_value(state);
                                break;
                            }
                        }
                        ArgCase::General(symbol, body) => {
                            let temp = state.definitions.get(symbol).cloned();
                            state
                                .definitions
                                .insert(*symbol, LazyValue::Evaled(arg.clone()));
                            out = eval_expression(body.clone(), state);
                            if let Some(temp) = temp {
                                state.definitions.insert(*symbol, temp.clone());
                            } else {
                                state.definitions.remove(symbol);
                            }
                            break;
                        }
                    }
                }
                out
            }

            (_, Function::Replace(val)) => val.clone(),
            (Value::Number(n), Function::Plus(Value::Number(n2))) => Value::Number(n + n2),

            (Value::String(s), Function::Plus(Value::String(s2))) => Value::String(s.clone() + s2),
            (Value::String(s), Function::Mult(Value::Number(n))) => {
                Value::String(s.repeat(*n as usize))
            }
            (Value::String(s), Function::Div(Value::String(s2))) => {
                if s2.is_empty() {
                    Value::Tuple(s.chars().map(|c| Value::String(c.to_string())).collect())
                } else {
                    Value::Tuple(s.split(s2).map(|s| Value::String(s.to_string())).collect())
                }
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
            (Value::Number(n), Function::Range(Value::Number(n2))) => {
                // check that numbers are ints
                if *n != n.floor() || *n2 != n2.floor() {
                    panic!("Range must be integers");
                }
                Value::Tuple(
                    ((*n as i32)..(*n2 as i32))
                        .map(|i| Value::Number(i as f64))
                        .collect(),
                )
            }

            (Value::Bool(b1), Function::And(Value::Bool(b2))) => Value::Bool(*b1 && *b2),
            (Value::Bool(b1), Function::Or(Value::Bool(b2))) => Value::Bool(*b1 || *b2),

            (arg, Function::Eq(v)) => Value::Bool(arg == v),
            (arg, Function::Neq(v)) => Value::Bool(arg == v),

            (Value::Function(f), Function::Apply(val)) => f.call(val, state),
            (Value::Function(f), Function::Plus(Value::Function(f2))) => {
                Value::Function(Box::from(Function::Composit(f.clone(), f2.clone())))
            }

            (Value::Function(f), Function::Mult(Value::Number(n))) => {
                let mut f2 = f.clone();
                for _ in 1..*n as usize {
                    f2 = Box::from(Function::Composit(f.clone(), f2.clone()));
                }
                Value::Function(f2)
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
            Value::Function(box Function::Apply(v)) => write!(f, ":{}", v),
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

                Token::Replace => Replace(val),

                Token::Range => Range(val),

                //Token::Period => Compose(val),
                a => panic!("Invalid operator: {:?}", a),
            }))
        }
    }
}
#[derive(Debug, Clone, PartialEq)]
pub enum ArgCase {
    General(Symbol, Expression),
    Value(Value, LazyValue),
}
#[derive(Debug, Clone, PartialEq)]
pub enum Definition {
    Single,
    Cases(Vec<ArgCase>),
}
impl Definition {
    fn unwrap_cases(self) -> Vec<ArgCase> {
        match self {
            Definition::Single => panic!("Expected cases"),
            Definition::Cases(cases) => cases,
        }
    }
}

#[derive(Default)]
pub struct State {
    pub definitions: FnvHashMap<Symbol, LazyValue>,
    pub arg: Option<Value>,
}

pub fn interpret(items: Vec<Item>) -> Value {
    let mut state = State::default();

    let mut definitions = FnvHashMap::<Symbol, Definition>::default();
    for item in items {
        match item {
            Item::Definition { name, value } => {
                state.definitions.insert(name, LazyValue::NotEvaled(value));
                definitions.insert(name, Definition::Single);
            }
            Item::CaseDefinition { val, name, body } => {
                match val {
                    Expression::Symbol(s) if !state.definitions.contains_key(&s) => {
                        match definitions.get_mut(&name) {
                            Some(Definition::Single) => panic!("This function is already defined"),
                            None => {
                                definitions.insert(
                                    name,
                                    Definition::Cases(vec![ArgCase::General(s, body)]),
                                );
                            }
                            Some(Definition::Cases(cases)) => {
                                for case in cases.iter() {
                                    if let ArgCase::General(_, _) = case {
                                        panic!("This case is already defined")
                                    }
                                }
                                cases.push(ArgCase::General(s, body));
                            }
                        };
                    }
                    _ => {
                        let val = eval_expression(val, &mut state);

                        match definitions.get_mut(&name) {
                            Some(Definition::Single) => panic!("This function is already defined"),
                            None => {
                                definitions.insert(
                                    name,
                                    Definition::Cases(vec![ArgCase::Value(
                                        val,
                                        LazyValue::NotEvaled(body),
                                    )]),
                                );
                            }
                            Some(Definition::Cases(cases)) => {
                                for case in cases.iter() {
                                    if let ArgCase::Value(v, _) = case {
                                        if v == &val {
                                            panic!("This case is already defined");
                                        }
                                    } else {
                                        panic!("This case is already defined")
                                    }
                                }
                                cases.push(ArgCase::Value(val, LazyValue::NotEvaled(body)));
                            }
                        };
                    }
                };
                state.definitions.insert(
                    name,
                    LazyValue::Evaled(Value::Function(Box::new(Function::Cases(
                        definitions[&name].clone().unwrap_cases(),
                    )))),
                );
            }
        }
    }

    state.definitions[&Intern::new("main".to_string())]
        .clone()
        .to_value(&mut state)
}
