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

use std::{collections::VecDeque, error::Error, fmt::Display, rc::Rc};
use crate::lex::Token;

#[derive(Debug, Clone, PartialEq)]
pub enum Expression {
    Grouping(Rc<Expression>),
    Alternation{expr_a: Rc<Expression>, expr_b: Rc<Expression>},
    Concatenation{expr_a: Rc<Expression>, expr_b: Rc<Expression>},
    RepetitionRange{expr: Rc<Expression>, min: u32, max: Option<u32>},
    Escape(char),
    Terminal(char)
}

impl Display for Expression {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Expression::Grouping(e) => write!(f, "({})", e),
            Expression::Alternation { expr_a, expr_b } => write!(f, "{}|{}", expr_a, expr_b),
            Expression::Concatenation { expr_a, expr_b } => write!(f, "{}{}", expr_a, expr_b),
            Expression::RepetitionRange { expr, min, max } => write!(f, "{} {{{} ,{:?}}}", expr, min, max),
            Expression::Escape(e) => write!(f, "\\{}", e),
            Expression::Terminal(c) => write!(f, "{}", c),
        }
    }
}

pub struct ParseOptions {
    pub greedy: bool
}

#[derive(Debug, PartialEq)]
pub struct ParseErr {
    current_state: Option<Expression>,
    failed_at: Option<Token>,
    reason: String
}

impl Display for ParseErr {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        if let Some(state) = &self.current_state {
            write!(f, "Parsing Error: \"{}\" at {:?}\nTrace: {}", self.reason, self.failed_at, state)
        } else {
            write!(f, "Parsing Error: \"{}\" at {:?}", self.reason, self.failed_at)
        }
    }
}

impl Error for ParseErr {
}


pub fn take(tokens: &mut VecDeque<Token>) -> Option<Token> {
    tokens.pop_front()
}

pub fn expect(tokens: &mut VecDeque<Token>, expected: Token) -> Option<Token> {
    if take(tokens)? == expected {
        return Some(expected);
    }
    None
}

pub fn peek(tokens: &VecDeque<Token>) -> Option<Token> {
    Some(*tokens.front()?)    
}

pub fn parse_gregex(tokens: &mut VecDeque<Token>, options: &ParseOptions) -> Result<Expression, ParseErr> {
    let mut expr = Rc::new(parse_expression(tokens, options, None)?);
    while !tokens.is_empty() {
        let next = Rc::new(parse_expression(tokens, options, Some(expr.clone()))?);
        let consume_prev = !matches!(*next, 
            Expression::Grouping(_) | 
            Expression::Escape(_) | 
            Expression::Terminal(_));

        if consume_prev { 
            expr = next;
        } else {
            expr = Rc::new(Expression::Concatenation { expr_a: expr.clone(), expr_b: next.clone() })
        }
    }

    Ok((*expr).clone())
}

fn parse_expression(tokens: &mut VecDeque<Token>, options: &ParseOptions, previous: Option<Rc<Expression>>) -> Result<Expression, ParseErr> {
    let token = peek(tokens); 
    if token.is_none() { return Err(ParseErr { current_state: previous.map(|e| (*e).clone()), failed_at: None, reason: String::from("Unexpected end of tokens") }); }
    match token.unwrap() {
        Token::LParen => parse_grouping(tokens, options),
        Token::Pipe => parse_alteration(tokens, options, previous),
        Token::Star => parse_repeat(tokens, 0, None, options, previous),
        Token::Plus => parse_repeat(tokens, 1, None, options, previous),
        Token::Question => parse_repeat(tokens, 0, Some(1), options, previous),
        Token::LCurl => parse_repeat_range(tokens, options, previous),
        Token::Digit(d) => {
            take(tokens);
            Ok(Expression::Terminal(d))
        },
        Token::BSlash => parse_escape(tokens),
        Token::Char(c) => {
            take(tokens);
            Ok(Expression::Terminal(c))
        },
        _ => Err(ParseErr { current_state: previous.map(|e| (*e).clone()), failed_at: token, reason: String::from("Unexpected Token") })
    }
}

fn parse_grouping(tokens: &mut VecDeque<Token>, options: &ParseOptions) -> Result<Expression, ParseErr> {
    expect(tokens, Token::LParen);

    let mut group_tokens: VecDeque<Token> = VecDeque::new();
    let mut token = peek(tokens);
    while token != Some(Token::RParen) && token.is_some() {
        group_tokens.push_back(tokens.pop_front().unwrap());
        token = peek(tokens);
    }
    expect(tokens, Token::RParen);

    let expr = Rc::new(parse_gregex(&mut group_tokens, options)?);
    Ok(Expression::Grouping(expr))
}

fn parse_alteration(tokens: &mut VecDeque<Token>, options: &ParseOptions, previous: Option<Rc<Expression>>) -> Result<Expression, ParseErr> {
    expect(tokens, Token::Pipe);

    previous.clone().map(|prev| 
        Ok(Expression::Alternation { 
            expr_a: prev, 
            expr_b: Rc::new(parse_gregex(tokens, options)?) 
        })).ok_or(ParseErr {
            failed_at: peek(tokens),
            current_state: previous.map(|e|(*e).clone()),
            reason: String::from("Missing left alternation branch expression")
        })?
}

fn parse_repeat(tokens: &mut VecDeque<Token>, min: u32, max: Option<u32>, options: &ParseOptions, previous: Option<Rc<Expression>>) -> Result<Expression, ParseErr> {
    let operator = take(tokens);

    if let Some(operator) = operator {
        if !matches!(operator, Token::Star | Token::Plus | Token::Question | Token::RCurl) { 
            return Err(ParseErr { current_state: None, failed_at: None, reason: "Invalid repetition operator".to_string() }) 
        }
    } else {
        return Err(ParseErr { current_state: None, failed_at: None, reason: "Invalid repetition operator".to_string() }) 
    }

    if previous.is_none() {
        return Err(ParseErr {
            current_state: None,
            failed_at: peek(tokens),
            reason: "Missing expression to repeat".to_string()
        });
    }

    let toggle = peek(tokens) == Some(Token::Question);
    if toggle { take(tokens); }
    
    let greedy = (!options.greedy && toggle) || (options.greedy && !toggle);

    if greedy {
        return Ok(Expression::RepetitionRange { expr: previous.unwrap(), min, max })
    }
    
    let expr = previous.clone().unwrap();
    Ok(match (*expr).clone() {
        Expression::Grouping(_) => Expression::RepetitionRange { expr: previous.unwrap(), min, max },
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
        Expression::RepetitionRange { .. } => Expression::RepetitionRange { expr: previous.unwrap(), min, max },
        Expression::Escape(_) => Expression::RepetitionRange { expr: previous.unwrap(), min, max },
        Expression::Terminal(_) => Expression::RepetitionRange { expr: previous.unwrap(), min, max },
    })
}

fn parse_repeat_range(tokens: &mut VecDeque<Token>, options: &ParseOptions, previous: Option<Rc<Expression>>) -> Result<Expression, ParseErr> {
    expect(tokens, Token::LCurl);
    let min = parse_number(tokens);
    let mut max = min;

    if min.is_none() { return Err(ParseErr { current_state: previous.map(|e|(*e).clone()), failed_at: peek(tokens), reason: String::from("No minumum specified for repetition") }) }
    if peek(tokens) == Some(Token::Comma) {
        take(tokens);
        max = parse_number(tokens);
        if min > max { return Err(ParseErr { current_state: previous.map(|e|(*e).clone()), failed_at: peek(tokens), reason: String::from("Minimum is greater than maximum in repetition range") }) }
    }

    parse_repeat(tokens, min.unwrap(), max, options, previous)
}

fn parse_number(tokens: &mut VecDeque<Token>) -> Option<u32> {
    let mut digits: Vec<char> = Vec::new(); 

    let digit = take(tokens)?;
    if let Token::Digit(d) = digit {
        digits.push(d);
    }

    while let Token::Digit(d) = peek(tokens)? {
        take(tokens);
        digits.push(d);
    }

    digits.iter().collect::<String>().parse().ok()
}

fn parse_escape(tokens: &mut VecDeque<Token>) -> Result<Expression, ParseErr> {
    expect(tokens, Token::BSlash);
    let escaped = take(tokens);
    if escaped.is_none() {
        return Err(ParseErr {
            reason: "Unexpected end of pattern".to_string(),
            failed_at: None,
            current_state: None
        })
    }

    match escaped.unwrap() {
        Token::LParen => Ok(Expression::Terminal('(')),
        Token::RParen => Ok(Expression::Terminal(')')),
        Token::Pipe => Ok(Expression::Terminal('|')),
        Token::Star => Ok(Expression::Terminal('*')),
        Token::Plus => Ok(Expression::Terminal('+')),
        Token::Question => Ok(Expression::Terminal('?')),
        Token::LCurl => Ok(Expression::Terminal('{')),
        Token::Comma => Ok(Expression::Terminal(',')),
        Token::RCurl => Ok(Expression::Terminal('}')),
        Token::BSlash => Ok(Expression::Terminal('\\')),
        Token::LBracket => Ok(Expression::Terminal('[')),
        Token::Dash => Ok(Expression::Terminal('-')),
        Token::RBracket => Ok(Expression::Terminal(']')),
        Token::Digit(d) => Ok(Expression::Escape(d)),
        Token::Char(c) => Ok(Expression::Escape(c)), // todo: parse special sequences
    }
}

#[cfg(test)]
mod test {
    use super::*;

    #[test]
    pub fn test_take_none(){
        let mut tokens = VecDeque::from(vec![]);
        let taken = take(&mut tokens);
        assert_eq!(taken, None);
    }

    #[test]
    pub fn test_take_last(){
        let mut tokens = VecDeque::from(vec![Token::RParen]);
        let taken = take(&mut tokens);
        assert_eq!(taken, Some(Token::RParen));
        assert!(tokens.is_empty())
    }
    
    #[test]
    pub fn test_take(){
        let mut tokens = VecDeque::from(vec![Token::LParen, Token::RParen]);
        let taken = take(&mut tokens);
        assert_eq!(taken, Some(Token::LParen));
        assert_eq!(tokens, VecDeque::from(vec![Token::RParen]))
    }
    
    #[test]
    pub fn test_expect_match(){
        let mut tokens = VecDeque::from(vec![Token::LParen]);
        let expect = expect(&mut tokens, Token::LParen);

        assert_eq!(expect, Some(Token::LParen));
        assert!(tokens.is_empty());
    }
    
    #[test]
    pub fn test_expect_mismatch(){
        let mut tokens = VecDeque::from(vec![Token::LParen]);
        let expect = expect(&mut tokens, Token::RParen);

        assert_eq!(expect, None);
        assert!(tokens.is_empty());
    }

    #[test]
    pub fn test_peek_empty(){
        let tokens = VecDeque::from(vec![]);

        assert_eq!(peek(&tokens), None)
    }
    
    #[test]
    pub fn test_peek(){
        let tokens = VecDeque::from(vec![Token::LParen]);

        assert_eq!(peek(&tokens), Some(Token::LParen))
    }

    #[test]
    pub fn test_expr_terminal() {
        let mut tokens = VecDeque::from(vec![Token::Char('a')]);
        let expr = parse_expression(&mut tokens, &ParseOptions { greedy: false }, None);

        assert_eq!(expr, Ok(Expression::Terminal('a')))
    }
}
