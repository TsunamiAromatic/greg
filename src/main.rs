mod lex;
mod parse;
use clap::Parser;
use parse::{parse_gregex, Expression, ParseOptions};
use rand::random_bool;

use lex::lex_gregex;

#[derive(Parser, Debug)]
#[command(version, about)]
struct Options {
    /// Repetition applies to entire previous group or just single expression
    #[arg(short, long, default_value_t = false)]
    greedy: bool,
    
    /// Print entire token stream and parse tree
    #[arg(long, default_value_t = false)]
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
    
    let parse_tree = parse_gregex(&mut tokens, &ParseOptions { greedy: options.greedy }).unwrap();
    if options.verbose { println!("AST: {:#?}", parse_tree); }
    
    let generated = evaluate(&parse_tree, &options);
    println!("{}", generated);
}

fn evaluate(expression: &Expression, options: &Options) -> String {
    match expression {
        Expression::Grouping(expr) => evaluate(expr, options),
        Expression::Alternation { expr_a, expr_b } => evaluate_alternation(expr_a, expr_b, options),
        Expression::Concatenation { expr_a, expr_b } => evaluate(expr_a, options) + &evaluate(expr_b, options),
        Expression::RepetitionRange { expr, min, max } => evaluate_repeat(expr, *min, *max, options),
        Expression::Escape(e) => e.to_string(),
        Expression::Terminal(c) => c.to_string(),
    }
}

fn evaluate_alternation(a: &Expression, b: &Expression, options: &Options) -> String {
    if random_bool(0.5) {
        evaluate(a, options)
    } else {
        evaluate(b, options)
    }
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
