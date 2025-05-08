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
    LBracket,
    Dash,
    RBracket,
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
            '[' => tokens.push_back(Token::LBracket),
            '-' => tokens.push_back(Token::Dash),
            ']' => tokens.push_back(Token::RBracket),
            '\\' => tokens.push_back(Token::BSlash),
            _ if c.is_ascii_digit() => tokens.push_back(Token::Digit(c)),
            _ => tokens.push_back(Token::Char(c))
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
                Token::LParen, 
                Token::Char('a'), 
                Token::Pipe, 
                Token::Digit('1'), 
                Token::Pipe,
                Token::BSlash,
                Token::Star,
                Token::RParen,
                Token::Plus,
                Token::Question
            ]));
    }

    #[test]
    fn test_lparen() {
       let pattern = lex_gregex("(").pop_front().unwrap(); 
       assert_eq!(pattern, Token::LParen)
    }
    
    #[test]
    fn test_rparen() {
       let pattern = lex_gregex(")").pop_front().unwrap(); 
       assert_eq!(pattern, Token::RParen)
    }
    
    #[test]
    fn test_pipe() {
       let pattern = lex_gregex("|").pop_front().unwrap(); 
       assert_eq!(pattern, Token::Pipe)
    }

    #[test]
    fn test_star() {
       let pattern = lex_gregex("*").pop_front().unwrap(); 
       assert_eq!(pattern, Token::Star)
    }

    #[test]
    fn test_plus() {
       let pattern = lex_gregex("+").pop_front().unwrap(); 
       assert_eq!(pattern, Token::Plus)
    }

    #[test]
    fn test_question() {
       let pattern = lex_gregex("?").pop_front().unwrap(); 
       assert_eq!(pattern, Token::Question)
    }
    
    #[test]
    fn test_lcurl() {
       let pattern = lex_gregex("{").pop_front().unwrap(); 
       assert_eq!(pattern, Token::LCurl)
    }

    #[test]
    fn test_comma() {
       let pattern = lex_gregex(",").pop_front().unwrap(); 
       assert_eq!(pattern, Token::Comma)
    }

    #[test]
    fn test_rcurl() {
       let pattern = lex_gregex("}").pop_front().unwrap(); 
       assert_eq!(pattern, Token::RCurl)
    }

    #[test]
    fn test_bslash() {
       let pattern = lex_gregex("\\").pop_front().unwrap(); 
       assert_eq!(pattern, Token::BSlash)
    }

    #[test]
    fn test_digit() {
       let pattern = lex_gregex("0123456789"); 
       assert_eq!(pattern, VecDeque::from(vec![
               Token::Digit('0'),
               Token::Digit('1'),
               Token::Digit('2'),
               Token::Digit('3'),
               Token::Digit('4'),
               Token::Digit('5'),
               Token::Digit('6'),
               Token::Digit('7'),
               Token::Digit('8'),
               Token::Digit('9'),
       ]))
    }
}
