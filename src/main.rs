mod tests;
mod lexer;

use std::env;
use std::fs::File;
use std::io::prelude::*;
use std::io;

use crate::lexer::Lexer;

pub struct Parser {
    repl: bool,
}

impl Parser {
    pub fn new(len: usize) -> Parser {
        let repl = if len == 1 {
            true
        } else {
            false
        };

        Parser {
            repl,
        }
    }

    pub fn repl(&self) {
        print!("Scheme-rs REPL ('exit' or 'quit' to terminate session)\n");
        loop {
            // Todo: Implement proper sighandling
            let mut input = String::new();
            print!("scheme-rs> ");
            io::stdout().flush().unwrap();
            io::stdin().read_line(&mut input).expect("Unexpected read_line() failure");
            
            let mut input: String = input.trim().to_string();
            if input == "exit" || input == "quit" {
                break;
            }
            
            let _ast = self.generate_ast(&input);
            input.clear();
            assert!(input.is_empty());
        }
    }

    pub fn compile(&self, mut input: String) {
        let f = File::open(&input);
        input.clear();
        match f {
            Ok(mut file) => {
                file.read_to_string(&mut input)
                        .expect(format!("Failed to read input file {:?}", &input).as_str());
            },
            Err(error) => {
                panic!("Failed to open input file {:?} with {:?}", &input, error);
            }
        }
    }

    pub fn generate_ast(&self, code: &str) -> () {
        let mut lexer = Lexer::new(code, code.chars());
        let _current_token = lexer.next_token().unwrap();
        /*loop {
            let current_token = match lexer.next_token() {
                Some(token) => token,
                None => {
                    // handle none condition
                }
            };

            match current_token {

            }
        }*/
    }
}

fn main() {
    let args: Vec<String> = env::args().collect();
    println!("{:?}", args);
    if args.len() > 2 {
        panic!("Usage: ./scheme-rs <path>\n       ./scheme-rs")
    }
    let parser = Parser::new(args.len());
    if parser.repl == true {
        parser.repl();
    } else {
        // error handle
        parser.compile(args[1].clone());
    }
}
