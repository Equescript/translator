pub mod bnf_parser;
pub mod lex;
pub mod lexical_analyzer;
pub mod parser;
pub mod pattern_matcher;
pub mod preprocess;
pub mod preprocessor;
pub mod primitives;
// pub mod rule_matcher;
pub mod rules;
pub mod token_matcher;

use std::io::Read;
use crate::{lexical_analyzer::{Matcher, CharLocation, LexicalRules}, pattern_matcher::TokenLocation};

fn main() {
    let path = std::path::Path::new("D:\\Code\\Code\\Rust\\Language\\translator\\source.txt");
    let mut file = match std::fs::File::open(path) {
        Ok(t) => t,
        Err(e) => {println!("{}", e); panic!()}
    };
    let mut stream = String::new();
    match file.read_to_string(&mut stream) {
        Ok(t) => t,
        Err(e) => {println!("{}", e); panic!()}
    };

    // let mut tokens: Vec<pattern_matcher::Token<char, LexicalRules, CharLocation>> = Vec::new();

    let mut stream_iter = stream.chars().into_iter();

    let mut matcher = Matcher::new(LexicalRules::any, CharLocation::default(), "".to_string());
    loop {
        match stream_iter.next() {
            Some(c) => {
                match matcher.match_token(&c) {
                    Ok(r) => match r {
                        Some((token, _)) => {
                            // tokens.push(token);
                            break;
                        },
                        None => continue,
                    }
                    Err((e, s)) => {
                        print!("{}", e.tab);
                        e.location.print_location();
                        print!("  [error] {:?} ", e.rule);
                        match e.sub_rule_matchers {
                            Some(sub_matchers) => print!("Some({})", sub_matchers.len()),
                            None => print!("None")
                        }
                        print!(" {}\n", s);
                        panic!();
                    },
                }
            },
            None => {
                // matcher.match_token(&'\n');
                break
            },
        }
    }
    if matcher.token.is_empty() {
        println!("matcher.token.is_empty()")
    }
    crate::lexical_analyzer::print_tokens(&matcher.token, '\t'.to_string())
}

/* fn match_pattern(stream_iter: &mut std::str::Chars, tokens: &mut Vec<pattern_matcher::Token<char, LexicalRules, CharLocation>>) -> Result<(), Matcher> {
    loop {
        match stream_iter.next() {
            Some(c) => {
                match matcher.match_token(&c)? {
                    Some((token, _)) => {
                        tokens.push(token);
                    },
                    None => continue,
                }
            },
            None => break,
        }
    }
    Ok(())
}
 */