use internment::Intern;
use logos::Logos;

#[derive(Logos, Debug, PartialEq, Clone)]
pub enum Token {
    // operators
    #[token("|")]
    Or,

    #[token("&")]
    And,

    #[token("==")]
    Equal,

    #[token("!=")]
    NotEqual,

    #[token(">=")]
    MoreOrEqual,

    #[token("<=")]
    LessOrEqual,

    #[token(">")]
    MoreThan,

    #[token("<")]
    LessThan,

    #[token("*")]
    Star,

    #[token("%")]
    Modulo,

    #[token("^")]
    Power,

    #[token("+")]
    Plus,

    #[token("-")]
    Minus,

    #[token("/")]
    Slash,

    #[token("!")]
    Exclamation,

    #[token("=")]
    Assign,

    #[token("<~")]
    Replace,

    #[token("..")]
    Range,

    // values
    #[regex(r"([a-zA-Z_][a-zA-Z0-9_]*)|\$", |lexer| lexer.slice().to_string())]
    Symbol(String),

    #[regex(r"[0-9][_0-9]*(\.[0-9][_0-9]*)?", |lexer| lexer.slice().to_string().parse())]
    Number(f64),

    #[regex(r#"[a-z0-9]*("(?:\\.|[^\\"])*"|'(?:\\.|[^\\'])*')"#, |lexer| { 
        let mut s = lexer.slice().to_string();
        s.remove(0);
        s.pop();
        s
    })]
    StringLiteral(String),

    #[token("true")]
    True,

    #[token("false")]
    False,

    #[token("$")]
    Dollar,

    //TERMINATORS
    #[token(",")]
    Comma,

    #[token("{")]
    OpenCurlyBracket,

    #[token("}")]
    ClosingCurlyBracket,

    #[token("[")]
    OpenSquareBracket,

    #[token("]")]
    ClosingSquareBracket,

    #[token("(")]
    OpenBracket,

    #[token(")")]
    ClosingBracket,

    #[token(":")]
    Colon,

    // #[token(".")]
    // Period,
    #[token(";")]
    Semicolon,

    #[error]
    #[regex(r"[ \t\f\n\r]+|/\*[^*]*\*(([^/\*][^\*]*)?\*)*/|//[^\n]*", logos::skip)]
    Error,
}

macro_rules! operator_pat {
    () => {
        Token::Or
            | Token::And
            | Token::Equal
            | Token::NotEqual
            | Token::MoreOrEqual
            | Token::LessOrEqual
            | Token::MoreThan
            | Token::LessThan
            | Token::Star
            | Token::Modulo
            | Token::Power
            | Token::Plus
            | Token::Minus
            | Token::Slash
            | Token::Exclamation
            //| Token::Comma
            | Token::Colon
            | Token::Replace
            | Token::Range
        //| Token::Period
    };
}

fn precedence(op: &Token) -> u8 {
    match op {
        //Token::Period => 9,
        Token::Or => 8,
        Token::And => 8,
        Token::Equal => 7,
        Token::NotEqual => 7,
        Token::MoreOrEqual => 7,
        Token::LessOrEqual => 7,
        Token::MoreThan => 7,
        Token::LessThan => 7,
        Token::Plus => 6,
        Token::Minus => 6,
        Token::Star => 5,
        Token::Modulo => 5,
        Token::Slash => 5,
        Token::Power => 4,
        Token::Range => 4,
        Token::Colon => 3,
        Token::Replace => 2,
        _ => 0,
    }
}
pub type Symbol = Intern<String>;

#[derive(Debug, PartialEq, Clone)]
pub enum Item {
    Definition {
        name: Symbol,
        value: Expression,
    },
    CaseDefinition {
        val: Expression,
        name: Symbol,
        body: Expression,
    },
}

#[derive(Debug, PartialEq, Clone)]
pub enum Expression {
    Symbol(Symbol),
    Number(f64),
    StringLiteral(String),
    Boolean(bool),
    Tuple(Vec<Expression>),

    FunctionCall(Box<Expression>, Box<Expression>),
    Operator(Token, Box<Expression>),
    Argument,
}

struct Tokens {
    tokens: Vec<Token>,
    position: usize,
}

impl Tokens {
    fn new(tokens: Vec<Token>) -> Self {
        Self {
            tokens,
            position: 0,
        }
    }

    fn next(&mut self) -> Option<Token> {
        self.position += 1;
        self.tokens.get(self.position - 1).cloned()
    }

    fn peek(&self) -> Option<Token> {
        self.tokens.get(self.position).cloned()
    }

    fn previous(&mut self) -> Option<Token> {
        self.position -= 1;
        self.tokens.get(self.position).cloned()
    }
}

#[derive(Debug, PartialEq, Clone)]
pub enum SyntaxError {
    Message(String),
}

pub fn parse(unparsed: String) -> Result<Vec<Item>, SyntaxError> {
    let lexer = Token::lexer(&unparsed);
    let mut tokens = Tokens::new(lexer.collect());
    let mut items = Vec::new();

    while tokens.peek().is_some() {
        items.push(parse_item(&mut tokens)?)
    }

    Ok(items)
}

fn parse_symbol(tokens: &mut Tokens) -> Result<Symbol, SyntaxError> {
    let token = tokens
        .next()
        .ok_or_else(|| SyntaxError::Message("Unexpected end of file".to_string()))?;
    match token {
        Token::Symbol(symbol) => Ok(Intern::from(&symbol)),
        _ => Err(SyntaxError::Message("Expected symbol".to_string())),
    }
}

fn parse_item(tokens: &mut Tokens) -> Result<Item, SyntaxError> {
    let two_first = (tokens.next(), tokens.next());
    match two_first {
        (Some(Token::Symbol(symbol)), Some(Token::Assign)) => {
            let value = parse_expression(tokens, !0, true)?;
            if let Some(Token::Semicolon) = tokens.next() {
            } else {
                return Err(SyntaxError::Message("Expected ;".to_string()));
            }

            Ok(Item::Definition {
                name: Intern::from(&symbol),
                value,
            })
        }
        _ => {
            tokens.previous();
            tokens.previous();
            let base = parse_operand(tokens)?;
            let name = parse_symbol(tokens)?;
            if let Some(Token::Assign) = tokens.next() {
            } else {
                return Err(SyntaxError::Message("Expected =".to_string()));
            }
            let value = parse_expression(tokens, !0, true)?;

            if let Some(Token::Semicolon) = tokens.next() {
            } else {
                return Err(SyntaxError::Message("Expected ;".to_string()));
            }

            Ok(Item::CaseDefinition {
                val: base,
                name,
                body: value,
            })
        }
    }
}

fn parse_expression(
    tokens: &mut Tokens,
    max_precedence: u8,
    check_for_call: bool,
) -> Result<Expression, SyntaxError> {
    let mut base = parse_operand(tokens)?;
    if check_for_call {
        loop {
            match tokens.peek() {
                Some(
                    Token::ClosingBracket
                    | Token::ClosingSquareBracket
                    | Token::Semicolon
                    | Token::Comma,
                )
                | None => break,
                a => {
                    if let Some(a) = a {
                        if is_operator(&a) && precedence(&a) >= max_precedence {
                            break;
                        }
                    }
                    // function call
                    let function = parse_expression(tokens, max_precedence, false)?;
                    base = Expression::FunctionCall(Box::new(base), Box::new(function))
                }
            }
        }
    }
    Ok(base)
}

fn parse_operand(tokens: &mut Tokens) -> Result<Expression, SyntaxError> {
    let token = tokens
        .next()
        .ok_or_else(|| SyntaxError::Message("Unexpected end of file".to_string()))?;
    Ok(match token {
        Token::Symbol(symbol) => Expression::Symbol(Intern::from(&symbol)),
        Token::Number(number) => Expression::Number(number),
        Token::StringLiteral(string) => Expression::StringLiteral(string),
        Token::True => Expression::Boolean(true),
        Token::False => Expression::Boolean(false),
        Token::Dollar => Expression::Argument,
        Token::OpenBracket => {
            let mut expr = parse_expression(tokens, !0, true)?;
            match tokens.peek() {
                Some(Token::ClosingBracket) => {
                    tokens.next();
                }
                Some(Token::Comma) => {
                    let mut list = vec![expr];
                    tokens.next();

                    loop {
                        //dbg!("hi");
                        list.push(parse_expression(tokens, !0, true)?);
                        match tokens.peek() {
                            Some(Token::Comma) => {
                                tokens.next();
                            }
                            Some(Token::ClosingBracket) => {
                                tokens.next();
                                break;
                            }
                            _ => {
                                return Err(SyntaxError::Message("Expected , or )".to_string()));
                            }
                        }
                    }
                    expr = Expression::Tuple(list)
                }
                _ => return Err(SyntaxError::Message("Expected )".to_string())),
            }
            expr
        }

        _ => {
            if is_operator(&token) {
                let operator = token;
                let right = parse_expression(tokens, precedence(&operator), true)?;
                Expression::Operator(operator, Box::new(right))
            } else {
                return Err(SyntaxError::Message(format!(
                    "Expected expression, found {token:?}"
                )));
            }
        }
    })
}

fn is_operator(token: &Token) -> bool {
    matches!(token, operator_pat!())
}
