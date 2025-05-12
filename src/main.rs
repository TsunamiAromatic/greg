mod lex;
mod parse;
use clap::Parser;
use parse::{parse_gregex, Expression, ParseOptions};
use rand::{random_bool, random_range};

use lex::lex_gregex;

/// GREG (Generative Regular Expressions) - Parses a regex pattern and generates a string from it
/// 
/// A gregex is a simple regex parser that will then generate a string that would satisfy the
/// expression. (with some nuance)
///
/// Example: (a|b) => 'a' or 'b' 
///
/// Example: (a)+ => 'aaaa....' or just 'a' 
///
/// Example: (ab?) => 'a' or 'ab'
///
/// Repetition - repetition will use a random chance to generate a string that satisfies the
/// preceding token or group. A question mark following will toggle between lazy and greedy
/// evaluation.
/// The chance is configurable and defaults to 75% (0.75)
///
/// *: zero or many, +: one or many, ?: zero or one
///
/// Example: a* => '' or 'aaa....'
///
/// Example: (ab)* => '' or 'ababab....'
/// 
/// Example: (ab|cd*?) => '' or 'ab' or 'cddd....' (lazy)
/// 
/// Example: (ab|cd*) => 'cdcdcdcd....' (greedy)
#[derive(Parser, Debug)]
struct Options {
    /// Repetition will be lazily evaluated
    #[arg(short, long, default_value_t = false)]
    lazy: bool,
    
    /// Print entire token stream and parse tree
    #[arg(short, long, default_value_t = false)]
    verbose: bool,

    /// Chance that a repetition will occur (0.0 - 1.0)
    #[arg(short, long, default_value_t = 0.75)]
    repeat_chance: f64,

    /// Gregex to parse
    pattern: String
}

fn main() {
    let options = Options::parse();
    if options.verbose { println!("Pattern: {:?}", options.pattern); }

    let mut tokens = lex_gregex(&options.pattern);
    if options.verbose { println!("Tokens: {:?}", tokens); }
    
    let parse_tree = parse_gregex(&mut tokens, &ParseOptions { greedy: !options.lazy });
    if options.verbose { println!("AST: {:#?}", parse_tree); }
    
    if let Ok(parse_tree) = parse_tree {
        let generated = evaluate(&parse_tree, &options);
        println!("{}", generated);
    } else {
        println!("{}", parse_tree.err().unwrap())
    }
}

fn evaluate(expression: &Expression, options: &Options) -> String {
    match expression {
        Expression::Grouping(expr) => evaluate(expr, options),
        Expression::Alternation { expr_a, expr_b } => evaluate_alternation(expr_a, expr_b, options),
        Expression::Concatenation { expr_a, expr_b } => evaluate(expr_a, options) + &evaluate(expr_b, options),
        Expression::RepetitionRange { expr, min, max } => evaluate_repeat(expr, *min, *max, options),
        Expression::Escape(_) => evaluate_escape(expression),
        Expression::Terminal(_) => evaluate_terminal(expression),
        Expression::AlternationGroup(alt) => evaluate_alternation_group(alt, options),
        Expression::None => "".to_string(),
        Expression::CharacterRange(r) => random_range((*r).clone()).to_string(),
    }
}

fn evaluate_alternation(a: &Expression, b: &Expression, options: &Options) -> String {
    if random_bool(0.5) {
        evaluate(a, options)
    } else {
        evaluate(b, options)
    }
}

fn evaluate_alternation_group(alternation: &Expression, options: &Options) -> String {
    evaluate(alternation, options)
}

fn evaluate_repeat(expression: &Expression, min: u32, max: Option<u32>, options: &Options) -> String {
    let mut result = String::new();
    for _ in 0..min {
        result += &evaluate(expression, options);
    }

    if let Some(max) = max {
        for _ in min..max {
            if random_bool(options.repeat_chance) {
                result += &evaluate(expression, options);
            }
        }
    } else {
        while random_bool(options.repeat_chance) {
            result += &evaluate(expression, options);
        }
    }

    result
}

fn generate_character() -> char {
    random_range(' '..='~')
}

fn evaluate_terminal(expression: &Expression) -> String {
    match expression {
        Expression::Terminal(c) => match c {
            '.' => generate_character().to_string(),
            _ => c.to_string()
        },
        _ => String::new()
    }
}

fn evaluate_escape(expression: &Expression) -> String {
    match expression {
        Expression::Escape(e) => match e.as_str() {
            "s" => ' '.to_string(),
            "t" => '\t'.to_string(),
            "n" => '\n'.to_string(),
            "r" => '\r'.to_string(),
            "e" => '\u{1b}'.to_string(),
            "d" => random_range('0'..='9').to_string(),
            _ => e.to_string()
        }
        _ => String::new()
    }
}
