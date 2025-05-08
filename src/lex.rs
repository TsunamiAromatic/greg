use std::collections::VecDeque;


#[derive(Debug, PartialEq, Copy, Clone)]
pub enum Token {
    LParen,
    RParen,
    Pipe,
    Star,
    Plus,
    Question,
    LCurl,
    Comma,
    RCurl,
    Digit(char),
    BSlash,
    Char(char)
}

pub fn lex_gregex(gregex: &str) -> VecDeque<Token> {
    let mut tokens: VecDeque<Token> = VecDeque::new();
    for c in gregex.chars() {
        match c {
            '(' => tokens.push_back(Token::LParen),
            ')' => tokens.push_back(Token::RParen),
            '|' => tokens.push_back(Token::Pipe),
            '*' => tokens.push_back(Token::Star),
            '+' => tokens.push_back(Token::Plus),
            '?' => tokens.push_back(Token::Question),
            '{' => tokens.push_back(Token::LCurl),
            ',' => tokens.push_back(Token::Comma),
            '}' => tokens.push_back(Token::RCurl),
            '\\' => tokens.push_back(Token::BSlash),
            _ if c.is_ascii_digit() => tokens.push_back(Token::Digit(c)),
            _ => tokens.push_back(Token::Char(c))
       };
    }

    tokens
}
