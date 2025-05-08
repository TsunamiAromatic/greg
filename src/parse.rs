/*
 * GREGEX Grammar
 *
 * start: expression
 * expression: (grouping | alternation | concatenation | repetition | optional | escape | TERMINAL)
 * grouping: '(' expression ')'
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

use std::{collections::VecDeque, rc::Rc};
use crate::lex::Token;

#[derive(Debug, Clone)]
pub enum Expression {
    Grouping(Rc<Expression>),
    Alternation{expr_a: Rc<Expression>, expr_b: Rc<Expression>},
    Concatenation{expr_a: Rc<Expression>, expr_b: Rc<Expression>},
    RepetitionRange{expr: Rc<Expression>, min: u32, max: Option<u32>},
    Escape(char),
    Terminal(char)
}

pub struct ParseOptions {
    pub(crate) greedy: bool
}

pub fn parse_gregex(tokens: &mut VecDeque<Token>, options: &ParseOptions) -> Option<Expression> {
    let mut expr = Rc::new(parse_expression(tokens, options, None)?);
    while tokens.front().is_some() {
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

    Some((*expr).clone())
}

fn parse_expression(tokens: &mut VecDeque<Token>, options: &ParseOptions, previous: Option<Rc<Expression>>) -> Option<Expression> {
    let token = *tokens.front()?;
    let expr = match token {
        Token::LParen => parse_grouping(tokens, options),
        Token::RParen => return None,
        Token::Pipe => {
            tokens.pop_front();
            return previous.map(|prev| Some(Expression::Alternation { expr_a: prev, expr_b: Rc::new(parse_gregex(tokens, options)?) }))?
        },
        Token::Star => return parse_repeat(tokens, 0, None, options, previous),
        Token::Plus => return parse_repeat(tokens, 1, None, options, previous),
        Token::Question => return parse_repeat(tokens, 0, Some(1), options, previous),
        Token::LCurl => None,
        Token::Comma => None,
        Token::RCurl => None,
        Token::Digit(d) => Some(Expression::Terminal(d)),
        Token::BSlash => parse_escape(tokens),
        Token::Char(c) => Some(Expression::Terminal(c)),

    }?;
    
    tokens.pop_front();
    Some(expr)
}

fn parse_grouping(tokens: &mut VecDeque<Token>, options: &ParseOptions) -> Option<Expression> {
    tokens.pop_front();
    let mut group_tokens: VecDeque<Token> = VecDeque::new();
    while *tokens.front()? != Token::RParen {
       group_tokens.push_back(tokens.pop_front()?) 
    }

    let expr = Rc::new(parse_gregex(&mut group_tokens, options)?);
    
    Some(Expression::Grouping(expr))
}

fn parse_repeat(tokens: &mut VecDeque<Token>, min: u32, max: Option<u32>, options: &ParseOptions, previous: Option<Rc<Expression>>) -> Option<Expression> {
    tokens.pop_front();
    if options.greedy || tokens.front() == Some(&Token::Question) {
        return previous.map(|expr| Expression::RepetitionRange { expr, min, max })
    }

    previous.map(|expr| {
        match (*expr).clone() {
            Expression::Grouping(_) => Expression::RepetitionRange { expr, min, max },
            Expression::Alternation { expr_a, expr_b } => 
                Expression::Alternation { 
                    expr_a, 
                    expr_b: Rc::new(Expression::RepetitionRange { 
                        expr: expr_b.clone(), 
                        min, 
                        max
                    })
                },
            Expression::Concatenation { expr_a, expr_b } => 
                Expression::Concatenation { 
                    expr_a, 
                    expr_b: Rc::new(Expression::RepetitionRange { 
                        expr: expr_b.clone(), 
                        min, 
                        max 
                    }) 
                },
            Expression::RepetitionRange { min, max, ..} => Expression::RepetitionRange { expr, min, max },
            Expression::Escape(_) => Expression::RepetitionRange { expr, min, max },
            Expression::Terminal(_) => Expression::RepetitionRange { expr, min, max },
        }
    })
}

fn parse_escape(tokens: &mut VecDeque<Token>) -> Option<Expression> {
    tokens.pop_front();
    match *tokens.front()? {
        Token::LParen => Some(Expression::Terminal('(')),
        Token::RParen => Some(Expression::Terminal(')')),
        Token::Pipe => Some(Expression::Terminal('|')),
        Token::Star => Some(Expression::Terminal('*')),
        Token::Plus => Some(Expression::Terminal('+')),
        Token::Question => Some(Expression::Terminal('?')),
        Token::LCurl => Some(Expression::Terminal('{')),
        Token::Comma => Some(Expression::Terminal(',')),
        Token::RCurl => Some(Expression::Terminal('}')),
        Token::BSlash => Some(Expression::Terminal('\\')),
        Token::Digit(d) => Some(Expression::Terminal(d)),
        Token::Char(c) => Some(Expression::Escape(c)), // todo: parse special sequences
    }
}
