use std::collections::HashMap;
use std::fmt;

#[derive(Debug, PartialEq)]
enum RaspError {
    SyntaxError(String),
    TypeError(String),
    SymbolError(String),
}

#[derive(Debug, PartialEq)]
enum Token {
    LParen,
    RParen,
    Number(f32),
    Symbol(String),
}

#[derive(Clone)]
enum Expr {
    List(Vec<Expr>),
    Number(f32),
    Symbol(String),
    Func(fn(&[Expr]) -> Result<Expr, RaspError>),
}

struct EvalEnv {
    data: HashMap<String, Expr>,
}

impl Default for EvalEnv {
    fn default() -> Self {
        let mut data = HashMap::<String, Expr>::new();
        data.insert(
            "+".into(),
            Expr::Func(|args: &[Expr]| Ok(numeric_list(args)?.iter().sum::<f32>().into())),
        );
        Self { data }
    }
}

fn numeric_list(expr_list: &[Expr]) -> Result<Vec<f32>, RaspError> {
    expr_list
        .iter()
        .map(|e| match e {
            Expr::Number(n) => Ok(*n),
            _ => Err(RaspError::TypeError(
                format!("{:?} is not a number", e).into(),
            )),
        })
        .collect()
}

impl From<f32> for Expr {
    fn from(val: f32) -> Self {
        Expr::Number(val)
    }
}

impl fmt::Debug for Expr {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Expr::List(val) => f.write_fmt(format_args!("Expr::List( {:#?} )", val)),
            Expr::Number(val) => f.write_fmt(format_args!("Expr::Number( {} )", val)),
            Expr::Symbol(val) => f.write_fmt(format_args!("Expr::Symbol( {} )", val)),
            Expr::Func(_) => f.write_str("Expr::Func"),
        }
    }
}

impl From<&str> for Token {
    fn from(str_token: &str) -> Self {
        match str_token {
            "(" => Self::LParen,
            ")" => Self::RParen,
            _ => match str_token.parse::<f32>() {
                Ok(num) => Self::Number(num),
                Err(_) => Self::Symbol(str_token.into()),
            },
        }
    }
}

fn tokenize(input: &str) -> Result<Vec<Token>, RaspError> {
    if input.is_empty() {
        return Err(RaspError::SyntaxError("Unexpected EOF".into()));
    }
    let tokens = input
        .replace('(', " ( ")
        .replace(')', " ) ")
        .split(' ')
        .filter(|s| !s.is_empty())
        .map(Token::from)
        .collect::<Vec<Token>>();
    Ok(tokens)
}

fn parse(input: &str) -> Result<Vec<Expr>, RaspError> {
    let tokens = tokenize(input)?;
    let mut token_stream = tokens.as_slice();
    let mut expr_stream = Vec::<Expr>::new();
    while !token_stream.is_empty() {
        let (expr, rest) = parse_expr(&token_stream)?;
        expr_stream.push(expr);
        token_stream = rest;
    }
    return Ok(expr_stream);
}

fn parse_expr<'a>(tokens: &'a [Token]) -> Result<(Expr, &'a [Token]), RaspError> {
    // Okay to unwrap, tokenize is guaranteed to error if there are no tokens
    let (token, rest) = tokens.split_first().unwrap();
    match token {
        Token::LParen => read_list(rest),
        Token::RParen => Err(RaspError::SyntaxError("Unexpected ')'".into())),
        Token::Number(num) => Ok((Expr::Number(num.clone()), rest)),
        Token::Symbol(sym) => Ok((Expr::Symbol(sym.clone()), rest)),
    }
}

fn read_list<'a>(tokens: &'a [Token]) -> Result<(Expr, &'a [Token]), RaspError> {
    let mut list: Vec<Expr> = vec![];
    let mut tokens = tokens;
    loop {
        let (token, rest) = tokens
            .split_first()
            .ok_or(RaspError::SyntaxError("Unexpected EOF".into()))?;
        if token == &Token::RParen {
            return Ok((Expr::List(list), rest));
        }
        let (expr, next_tokens) = parse_expr(&tokens)?;
        list.push(expr);
        tokens = next_tokens;
    }
}

fn eval(expr: &Expr, env: &mut EvalEnv) -> Result<Expr, RaspError> {
    match expr {
        Expr::Symbol(k) => env.data.get(k).map_or(
            Err(RaspError::SymbolError(format!("Undefined symbol: {}", k))),
            |e| Ok(e.clone()),
        ),
        Expr::Number(n) => Ok(expr.clone()),
        Expr::List(l) => {
            let (proc, args) = l.split_first().map_or(
                Err(RaspError::SyntaxError("Unexpected empty list".into())),
                |v| Ok(v),
            )?;
            match eval(proc, env)? {
                Expr::Func(f) => {
                    let args_eval = args
                        .iter()
                        .map(|arg| eval(arg, env))
                        .collect::<Result<Vec<Expr>, RaspError>>();
                    f(&args_eval?)
                }
                _ => Err(RaspError::TypeError("Expected procedure".into())),
            }
        }
        Expr::Func(_) => Err(RaspError::TypeError("".into())),
    }
}

fn main() -> Result<(), RaspError> {
    let input = "(+ 1 1 1)";
    dbg!(parse(input)?);
    let mut env = EvalEnv::default();
    dbg!(eval(&parse(input)?[0], &mut env));
    Ok(())
}

#[test]
fn test_parse() {
    //assert_eq!(tokenize("(+ 1 1)").unwrap(), vec!["(", "+", "1", "1", ")"]);
    assert_eq!(
        tokenize("").unwrap_err(),
        RaspError::SyntaxError("Unexpected EOF".into())
    )
}
