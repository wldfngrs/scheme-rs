use std::fs::File;
use std::io::prelude::*;
use std::io;

use crate::lexer::Lexer;
use crate::parser::Parser;

#[derive(Eq, PartialEq)]
enum ExecutionMode {
    Repl,
    Batch
}
pub struct Compiler {
    args: Vec<String>,
    mode: ExecutionMode
}

impl Compiler {
    pub fn new(args: Vec<String>) -> Self {
        // Default exection mode is repl
        let mut exectution_mode = ExecutionMode::Repl;
    
        if args.len() == 2 {
            exectution_mode = ExecutionMode::Batch;
        } else if args.len() > 2 {
            panic!("Usage: ./scheme-rs <path>\n       ./scheme-rs")
        }
        
        Compiler {
            args,
            mode: exectution_mode,
        }
    }

    pub fn start(&self) {
        if self.mode == ExecutionMode::Repl {
            self.repl()
        } else {
            self.batch(self.args[1].clone());
        }
    }

    fn repl(&self) {
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

            let lexer = Lexer::new(&input, input.chars());
            let mut parser = Parser::new(lexer);
            let _parse_tree = parser.generate_parse_tree();
            if parser.error_log.len() > 0 {
                for error in &parser.error_log {
                    println!("{}", error);
                }
            }
            input.clear();
        }
    }

    fn batch(&self, mut input: String) {
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
}