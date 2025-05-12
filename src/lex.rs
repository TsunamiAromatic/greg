use std::{collections::VecDeque, fmt::Display};


#[derive(Debug, PartialEq, Copy, Clone)]
pub enum Token {
    LParen((char, usize)),
    RParen((char, usize)),
    Pipe((char, usize)),
    Star((char, usize)),
    Plus((char, usize)),
    Question((char, usize)),
    LCurl((char, usize)),
    Comma((char, usize)),
    RCurl((char, usize)),
    LBracket((char, usize)),
    Dash((char, usize)),
    RBracket((char, usize)),
    Digit((char, usize)),
    BSlash((char, usize)),
    Char((char, usize)),
    None
}

impl Token {
    pub fn data(&self) -> (char, usize) {
        match *self {
            Token::LParen(d) => d,
            Token::RParen(d) => d,
            Token::Pipe(d) => d,
            Token::Star(d) => d,
            Token::Plus(d) => d,
            Token::Question(d) => d,
            Token::LCurl(d) => d,
            Token::Comma(d) => d,
            Token::RCurl(d) => d,
            Token::LBracket(d) => d,
            Token::Dash(d) => d,
            Token::RBracket(d) => d,
            Token::Digit(d) => d,
            Token::BSlash(d) => d,
            Token::Char(d) => d,
            Token::None => ('\0', 0),
        }
    }
}

impl Display for Token {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Token::LParen(data) => write!(f, "'{}':{}", data.0, data.1),
            Token::RParen(data) => write!(f, "'{}':{}", data.0, data.1),
            Token::Pipe(data) => write!(f, "'{}':{}", data.0, data.1),
            Token::Star(data) => write!(f, "'{}':{}", data.0, data.1),
            Token::Plus(data) => write!(f, "'{}':{}", data.0, data.1),
            Token::Question(data) => write!(f, "'{}':{}", data.0, data.1),
            Token::LCurl(data) => write!(f, "'{}':{}", data.0, data.1),
            Token::Comma(data) => write!(f, "'{}':{}", data.0, data.1),
            Token::RCurl(data) => write!(f, "'{}':{}", data.0, data.1),
            Token::LBracket(data) => write!(f, "'{}':{}", data.0, data.1),
            Token::Dash(data) => write!(f, "'{}':{}", data.0, data.1),
            Token::RBracket(data) => write!(f, "'{}':{}", data.0, data.1),
            Token::Digit(data) => write!(f, "'{}':{}", data.0, data.1),
            Token::BSlash(data) => write!(f, "'{}':{}", data.0, data.1),
            Token::Char(data) => write!(f, "'{}':{}", data.0, data.1),
            Token::None => write!(f, ""),
        }
    }
}

pub fn lex_gregex(gregex: &str) -> VecDeque<Token> {
    let mut tokens: VecDeque<Token> = VecDeque::new();
    
    for (i, c) in gregex.char_indices() {
        match c {
            '(' => tokens.push_back(Token::LParen((c, i))),
            ')' => tokens.push_back(Token::RParen((c, i))),
            '|' => tokens.push_back(Token::Pipe((c, i))),
            '*' => tokens.push_back(Token::Star((c, i))),
            '+' => tokens.push_back(Token::Plus((c, i))),
            '?' => tokens.push_back(Token::Question((c, i))),
            '{' => tokens.push_back(Token::LCurl((c, i))),
            ',' => tokens.push_back(Token::Comma((c, i))),
            '}' => tokens.push_back(Token::RCurl((c, i))),
            '[' => tokens.push_back(Token::LBracket(( c, i))),
            '-' => tokens.push_back(Token::Dash((c, i))),
            ']' => tokens.push_back(Token::RBracket((c, i))),
            '\\' => tokens.push_back(Token::BSlash((c, i))),
            _ if c.is_ascii_digit() => tokens.push_back(Token::Digit((c, i))),
            _ => tokens.push_back(Token::Char((c, i)))
       };
    }

    tokens
}

#[cfg(test)]
mod test {
    use super::*;

    #[test]
    fn test_pattern() {
        let pattern = "(a|1|\\*)+?";
        let tokens = lex_gregex(pattern);
        assert_eq!(tokens, 
            VecDeque::from(vec![
                Token::LParen(('(', 0)), 
                Token::Char(('a', 1)), 
                Token::Pipe(('|', 2)), 
                Token::Digit(('1', 3)), 
                Token::Pipe(('|', 4)), 
                Token::BSlash(('\\', 5)), 
                Token::Star(('*', 6)), 
                Token::RParen((')', 7)), 
                Token::Plus(('+', 8)), 
                Token::Question(('?', 9)), 
            ]));
    }

    #[test]
    fn test_lparen() {
       let pattern = lex_gregex("(").pop_front().unwrap(); 
       assert_eq!(pattern, Token::LParen(('(', 0)))
    }
    
    #[test]
    fn test_rparen() {
       let pattern = lex_gregex(")").pop_front().unwrap(); 
       assert_eq!(pattern, Token::RParen((')', 0)))
    }
    
    #[test]
    fn test_pipe() {
       let pattern = lex_gregex("|").pop_front().unwrap(); 
       assert_eq!(pattern, Token::Pipe(('|', 0)))
    }

    #[test]
    fn test_star() {
       let pattern = lex_gregex("*").pop_front().unwrap(); 
       assert_eq!(pattern, Token::Star(('*', 0)))
    }

    #[test]
    fn test_plus() {
       let pattern = lex_gregex("+").pop_front().unwrap(); 
       assert_eq!(pattern, Token::Plus(('+', 0)))
    }

    #[test]
    fn test_question() {
       let pattern = lex_gregex("?").pop_front().unwrap(); 
       assert_eq!(pattern, Token::Question(('?', 0)))
    }
    
    #[test]
    fn test_lcurl() {
       let pattern = lex_gregex("{").pop_front().unwrap(); 
       assert_eq!(pattern, Token::LCurl(('{', 0)))
    }

    #[test]
    fn test_comma() {
       let pattern = lex_gregex(",").pop_front().unwrap(); 
       assert_eq!(pattern, Token::Comma((',', 0)))
    }

    #[test]
    fn test_rcurl() {
       let pattern = lex_gregex("}").pop_front().unwrap(); 
       assert_eq!(pattern, Token::RCurl(('}', 0)))
    }

    #[test]
    fn test_bslash() {
       let pattern = lex_gregex("\\").pop_front().unwrap(); 
       assert_eq!(pattern, Token::BSlash(('\\', 0)))
    }

    #[test]
    fn test_digit() {
       let pattern = lex_gregex("0123456789"); 
       assert_eq!(pattern, VecDeque::from(vec![
               Token::Digit(('0', 0)),
               Token::Digit(('1', 1)),
               Token::Digit(('2', 2)),
               Token::Digit(('3', 3)),
               Token::Digit(('4', 4)),
               Token::Digit(('5', 5)),
               Token::Digit(('6', 6)),
               Token::Digit(('7', 7)),
               Token::Digit(('8', 8)),
               Token::Digit(('9', 9)),
       ]))
    }
}
