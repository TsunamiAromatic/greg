/*
 * GREGEX Grammar
 *
 * start: expression
 * expression: (grouping | alternation | concatenation | repetition | optional | escape | TERMINAL)
 * grouping: '(' expression ')'
 * alternation_group: '[' (range | TERMINAL)+ ']'
 * range: TERMINAL '-' TERMINAL
 * alternation: expression '|' expression
 * concatenation: expression expression
 * repetition: expression repetition_quantifier 
 * repetition_quantifier: '*' | '+' | repetition_range
 * repetition_range: '{' [0-9]+ ',' [0-9]+ '}'
 * optional: expression '?'
 * escape: '\' TERMINAL
 * TERMINAL: .
 *
 * **TERMINAL is any member of the set of all generatable characters
 */

use std::{char, collections::VecDeque, error::Error, fmt::Display, ops::RangeInclusive, rc::Rc};
use crate::lex::Token;

#[derive(Debug, Clone, PartialEq)]
pub enum Expression {
    Grouping(Rc<Expression>),
    Alternation{expr_a: Rc<Expression>, expr_b: Rc<Expression>},
    AlternationGroup(Rc<Expression>),
    Concatenation{expr_a: Rc<Expression>, expr_b: Rc<Expression>},
    RepetitionRange{expr: Rc<Expression>, min: u32, max: Option<u32>},
    CharacterRange(RangeInclusive<char>),
    Escape(String),
    Terminal(char),
    None
}

impl Display for Expression {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Expression::Grouping(e) => write!(f, "({})", e),
            Expression::AlternationGroup(e) => write!(f, "[{}]", e),
            Expression::Alternation { expr_a, expr_b } => write!(f, "{}|{}", expr_a, expr_b),
            Expression::Concatenation { expr_a, expr_b } => write!(f, "{}{}", expr_a, expr_b),
            Expression::RepetitionRange { expr, min, max } => write!(f, "{} {{{} ,{:?}}}", expr, min, max),
            Expression::Escape(e) => write!(f, "\\{}", e),
            Expression::Terminal(c) => write!(f, "{}", c),
            Expression::None => write!(f, "Missing Expression"),
            Expression::CharacterRange(r) => write!(f, "{}-{}", r.clone().min().unwrap(), r.clone().max().unwrap()),
        }
    }
}

pub struct ParseOptions {
    pub greedy: bool
}

#[derive(Debug, PartialEq)]
pub struct ParseErr {
    current_state: Expression,
    failed_at: Token,
    reason: String
}

impl Display for ParseErr {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let mut msg = format!("Parsing Error: {}", self.reason);
        if self.failed_at != Token::None {
            msg += &format!(" {}", self.failed_at);
        }

        if self.current_state != Expression::None {
            msg += &format!("\nTrace: {}", self.current_state);
        }

        write!(f, "{}", msg)
    }
}

impl Error for ParseErr {}

macro_rules! expect {
    ($expression:expr, $pattern:pat, $state:expr) => {
        if let $pattern = $expression {
            Ok($expression)
        } else {
            Err(error("Unexpected token", $state, $expression))
        }
    };
}

pub fn error(reason: &str, state: &Expression, at: Token) -> ParseErr {
    ParseErr {
        reason: reason.to_string(),
        current_state: state.clone(),
        failed_at: at
    }
}

pub fn take(tokens: &mut VecDeque<Token>) -> Token {
    tokens.pop_front().unwrap_or(Token::None)
}

pub fn peek(tokens: &VecDeque<Token>) -> Token {
    *tokens.front().unwrap_or(&Token::None)
}

pub fn parse_gregex(tokens: &mut VecDeque<Token>, options: &ParseOptions) -> Result<Expression, ParseErr> {
    let mut expr = Rc::new(parse_expression(tokens, options, Rc::new(Expression::None))?);
    while !tokens.is_empty() {
        let next = Rc::new(parse_expression(tokens, options, expr.clone())?);
        let consume_prev = !matches!(*next, 
            Expression::Grouping(_) | 
            Expression::Escape(_) | 
            Expression::Terminal(_) |
            Expression::AlternationGroup(_));

        if consume_prev { 
            expr = next;
        } else {
            expr = Rc::new(Expression::Concatenation { expr_a: expr.clone(), expr_b: next.clone() })
        }
    }

    Ok((*expr).clone())
}

fn parse_expression(tokens: &mut VecDeque<Token>, options: &ParseOptions, previous: Rc<Expression>) -> Result<Expression, ParseErr> {
    let token = peek(tokens); 
    if token == Token::None { return Err(error("Unexpected end of tokens", &previous, Token::None)); }

    match token {
        Token::LParen(_) => parse_grouping(tokens, options),
        Token::Pipe(_) => parse_alteration(tokens, options, previous),
        Token::Star(_) => parse_repeat(tokens, 0, None, options, previous),
        Token::Plus(_) => parse_repeat(tokens, 1, None, options, previous),
        Token::Question(_) => parse_repeat(tokens, 0, Some(1), options, previous),
        Token::LCurl(_) => parse_repeat_range(tokens, options, previous),
        Token::Digit(_) => parse_terminal(tokens, &previous),
        Token::BSlash(_) => parse_escape(tokens, &previous),
        Token::Dash(_) => parse_character_range(tokens, &previous),
        Token::Char(_) => parse_terminal(tokens, &previous),
        Token::LBracket(_) => parse_alternation_group(tokens, options, &previous),
        _ => Err(error("Unexpected Token", &previous, token))
    }
}

fn parse_grouping(tokens: &mut VecDeque<Token>, options: &ParseOptions) -> Result<Expression, ParseErr> {
    expect!(peek(tokens), Token::LParen(_), &Expression::None)?;
    let mut group_tokens: VecDeque<Token> = VecDeque::new();
    while !matches!(peek(tokens), Token::None | Token::RParen(_)) {
        group_tokens.push_back(take(tokens));
    }

    if !matches!(peek(tokens), Token::RParen(_)) { return Err(error("Expected )", &Expression::None, peek(tokens))) }

    let expr = Rc::new(parse_gregex(&mut group_tokens, options)?);
    Ok(Expression::Grouping(expr))
}

fn parse_alternation_group(tokens: &mut VecDeque<Token>, options: &ParseOptions, previous: &Expression) -> Result<Expression, ParseErr> {
    expect!(peek(tokens), Token::LBracket(_), previous)?;
    take(tokens);
    // Convert all tokens to characters as only characters and ranges are allowed effectively
    // escaping all special characters
    let mut group = VecDeque::new();
    while peek(tokens) != Token::None && !matches!(peek(tokens), Token::RBracket(_)) {
        let gtoken = match peek(tokens) {
            Token::Dash(_) => take(tokens),
            _ => Token::Char(take(tokens).data()) 
        };
        group.push_back(gtoken);
    }
    expect!(peek(tokens), Token::RBracket(_), previous)?;
    take(tokens);

    let mut expr = Rc::new(parse_expression(&mut group, options, Expression::None.into())?);
    while peek(&group) != Token::None {
        let next = parse_expression(&mut group, options, expr.clone())?;
        println!("expr {:?}", expr);
        println!("next {:?}", next);
        if matches!(next, Expression::CharacterRange(_) | Expression::Alternation {..}) {
            expr = next.into();
        }else {
            expr = Expression::Alternation { expr_a: expr.clone(), expr_b: next.into() }.into()
        }
    }
    println!("{:?}", expr);

    Ok(Expression::AlternationGroup(expr))
}

fn parse_alteration(tokens: &mut VecDeque<Token>, options: &ParseOptions, previous: Rc<Expression>) -> Result<Expression, ParseErr> {
    take(tokens);
    Ok(Expression::Alternation { 
        expr_a: previous.clone(), 
        expr_b: Rc::new(parse_gregex(tokens, options).map_err(|e| 
                error(&e.reason,
                    &Expression::Alternation { 
                        expr_a: previous.clone(), 
                        expr_b: Rc::new(e.current_state)
                    }, 
                    e.failed_at))?)
        })
            
}

fn parse_repeat(tokens: &mut VecDeque<Token>, min: u32, max: Option<u32>, options: &ParseOptions, previous: Rc<Expression>) -> Result<Expression, ParseErr> {
    let operator = take(tokens);

    if !matches!(operator, Token::Star(_) | Token::Plus(_) | Token::Question(_) | Token::RCurl(_)) { 
        return Err(error("Invalid repetition operator", &previous, peek(tokens)));
    }

    if *previous == Expression::None {
        return Err(error("Missing expression to repeat", &previous, peek(tokens)));
    }

    let next = peek(tokens);
    let toggle = matches!(next, Token::Question(_));
    if toggle { take(tokens); }
    
    let greedy = (!options.greedy && toggle) || (options.greedy && !toggle);

    if greedy {
        return Ok(Expression::RepetitionRange { expr: previous.clone(), min, max })
    }
    
    Ok(match (*previous).clone() {
        Expression::Grouping(_) => Expression::RepetitionRange { expr: previous, min, max },
        Expression::Alternation { expr_a, expr_b } => 
            Expression::Alternation { 
                expr_a,
                expr_b: Rc::new(Expression::RepetitionRange { 
                    expr: expr_b, 
                    min, 
                    max
                })
            },
        Expression::Concatenation { expr_a, expr_b } => 
            Expression::Concatenation { 
                expr_a, 
                expr_b: Rc::new(Expression::RepetitionRange { 
                    expr: expr_b, 
                    min, 
                    max 
                }) 
            },
        Expression::RepetitionRange { .. } => Expression::RepetitionRange { expr: previous, min, max },
        Expression::Escape(_) => Expression::RepetitionRange { expr: previous, min, max },
        Expression::Terminal(_) => Expression::RepetitionRange { expr: previous, min, max },
        _ => Expression::None
    })
}


fn parse_repeat_range(tokens: &mut VecDeque<Token>, options: &ParseOptions, previous: Rc<Expression>) -> Result<Expression, ParseErr> {
    expect!(peek(tokens), Token::LCurl(_), &previous)?;
    take(tokens);

    let min = parse_number(tokens);
    let mut max = min;

    if min.is_none() { 
        return Err(error("Failed to parse minumum for range", &previous, peek(tokens)));
    }

    if let Token::Comma(_) = peek(tokens) {
        take(tokens);
        max = parse_number(tokens);
        if max.is_none() {
            return Err(error("Failed to parse maximum for range", &previous, peek(tokens)));
        }
        if min > max { 
            return Err(error("Minimum is greater than maximum in repetition range", &previous, peek(tokens)));
        }
    }

    parse_repeat(tokens, min.unwrap(), max, options, previous)
}

fn parse_hex(tokens: &mut VecDeque<Token>, len: Option<usize>, previous: &Expression) -> Result<u32, ParseErr> {
    let mut digits: Vec<char> = Vec::new(); 

    if let Some(len) = len {
        for _ in 0..len {
            expect!(peek(tokens), Token::Digit(_) | Token::Char(_), previous)?;
            let d = take(tokens);
            digits.push(d.data().0);
        }
    } else {
        while matches!(peek(tokens), Token::Digit(_) | Token::Char(_)) {
            let token = take(tokens);
            digits.push(token.data().0);
        }

        if digits.is_empty() {
            return Err(error("Expected number but none found", previous, peek(tokens)));
        }
    }
    

    let digits = digits.iter().collect::<String>();
    match u32::from_str_radix(&digits, 16) {
        Ok(num) => Ok(num),
        Err(parse_err) => Err(error(format!("Failed to parse number: {}", parse_err).as_str(), previous, peek(tokens))),
    }
}

fn parse_number(tokens: &mut VecDeque<Token>) -> Option<u32> {
    let mut digits: Vec<char> = Vec::new(); 
    while matches!(peek(tokens), Token::Digit(_) | Token::Char(_)) {
        let token = take(tokens);
        digits.push(token.data().0);
    }
    
    if digits.is_empty() {
        return None;
    }

    let digits = digits.iter().collect::<String>();
    if let Ok(num) = digits.parse() {
        return Some(num);
    } else if let Ok(num) = u32::from_str_radix(&digits, 16) { 
        return Some(num)
    }
    None
}

fn parse_terminal(tokens: &mut VecDeque<Token>, previous: &Expression) -> Result<Expression, ParseErr>{
    if matches!(peek(tokens), Token::Char(_) | Token::Digit(_)) {
        let token = take(tokens);
        return Ok(Expression::Terminal(token.data().0));
    }

    Err(error("Expected a character or digit token", previous, peek(tokens)))
}

fn parse_character_range(tokens: &mut VecDeque<Token>, previous: &Expression) -> Result<Expression, ParseErr>{
    expect!(peek(tokens), Token::Dash(_), previous)?;
    take(tokens);

    if !matches!(previous, Expression::Terminal(_) | Expression::Concatenation {..} | Expression::Alternation {..}) {
        return Ok(Expression::Terminal('-'));
    }

    let max = parse_terminal(tokens, previous);
    if max.is_err() {
        return Ok(Expression::Terminal('-'));
    };

    println!("previous: {}", previous);
    match (*previous).clone() {
        Expression::Alternation { expr_a, expr_b } => 
            match *expr_b {
                Expression::Terminal(min) => { 
                    if let Ok(Expression::Terminal(max)) = max {
                        return Ok(Expression::Alternation { 
                            expr_a, 
                            expr_b: Rc::new(Expression::CharacterRange(min..=max)) 
                        });
                    }
                },
                _ => return Ok(Expression::Terminal('-')),
            },
        Expression::Concatenation { expr_a, expr_b } => 
            match *expr_b {
                Expression::Terminal(min) => 
                    if let Ok(Expression::Terminal(max)) = max {
                        return Ok(Expression::Concatenation { 
                            expr_a, 
                            expr_b: Rc::new(Expression::CharacterRange(min..=max)) 
                        });
                    },
                _ => return Ok(Expression::Terminal('-')),
            },
        Expression::Terminal(min) => 
            if let Ok(Expression::Terminal(max)) = max {
                return Ok(Expression::CharacterRange(min..=max));
            },
        _ => return Ok(Expression::Terminal('-'))
    };

    Err(error("Invalid range maximum", previous, peek(tokens)))
}

fn parse_escape(tokens: &mut VecDeque<Token>, previous: &Expression) -> Result<Expression, ParseErr> {
    expect!(peek(tokens), Token::BSlash(_), &previous)?;
    take(tokens);

    match peek(tokens) {
        Token::Char(_) | Token::Digit(_) => parse_escape_sequence(tokens, previous),
        Token::None => Err(error( "Unexpected end of pattern", previous, peek(tokens))),
        _ => Ok(Expression::Escape(take(tokens).data().0.to_string())),
    }
}

fn parse_escape_sequence(tokens: &mut VecDeque<Token>, previous: &Expression) -> Result<Expression, ParseErr> {
    if let Token::Char((c, _)) = take(tokens) {
        return match c {
            'd' | 's' | 'w' => Ok(Expression::Escape(c.to_string())),
            'x' => {
                if matches!(peek(tokens), Token::LCurl(_)){
                    take(tokens);
                    let code = parse_hex(tokens, Some(4), previous)?;
                    let expr = Expression::Escape(char::from_u32(code).unwrap().to_string());
                    expect!(peek(tokens), Token::RCurl(_), previous)?;
                    take(tokens);
                    return Ok(expr);
                }
                else{
                    let code = parse_hex(tokens, Some(2), previous)?;
                    Ok(Expression::Escape(char::from_u32(code).unwrap().to_string()))
                }
            },
            'u' => todo!(),
            _ => Err(error("Unrecognized escape sequence", previous, peek(tokens)))
        }
    }
    Ok(Expression::Escape(take(tokens).data().0.to_string()))
}

#[cfg(test)]
mod test {
    use super::*;

    #[test]
    pub fn test_take_none(){
        let mut tokens = VecDeque::from(vec![]);
        let taken = take(&mut tokens);
        assert_eq!(taken, Token::None);
    }

    #[test]
    pub fn test_take_last(){
        let mut tokens = VecDeque::from(vec![Token::RParen((')', 0))]);
        let taken = take(&mut tokens);
        assert_eq!(taken, Token::RParen((')', 0)));
        assert!(tokens.is_empty())
    }
    
    #[test]
    pub fn test_take(){
        let mut tokens = VecDeque::from(vec![Token::LParen(('(', 0)), Token::RParen((')', 1))]);
        let taken = take(&mut tokens);
        assert_eq!(taken, Token::LParen(('(', 0)));
        assert_eq!(tokens, VecDeque::from(vec![Token::RParen((')', 1))]))
    }

    #[test]
    pub fn test_peek_empty(){
        let tokens = VecDeque::from(vec![]);

        assert_eq!(peek(&tokens), Token::None)
    }
    
    #[test]
    pub fn test_peek(){
        let tokens = VecDeque::from(vec![Token::LParen(('(', 0))]);

        assert_eq!(peek(&tokens), Token::LParen(('(', 0)))
    }

    #[test]
    pub fn test_expr_terminal() {
        let mut tokens = VecDeque::from(vec![Token::Char(('a', 0))]);
        let expr = parse_expression(&mut tokens, &ParseOptions { greedy: false }, Rc::new(Expression::None));

        assert_eq!(expr, Ok(Expression::Terminal('a')))
    }
}
