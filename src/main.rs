use std::env;
use std::fs::File;
use std::io::prelude::*;
use std::io;

enum TokenType {
    TNone,         // ignore
    TPoint,        // .
    TPlus,         // +
    TMinus,        // -
    TAsterisk,     // *
    TLparen,       // (
    TRparen,       // )
    TSquote,       // '
    TBquote,       // `
    TComma,        // ,
    TSeqcomma,     // ,@
    TDquote,       // "
    TBslash,       // \
    TSharp,        // #
    TSharpt,       // #t true
    TSharpf,       // #f false
    TSharpbslash,  // #\ introduces a character constant
    TSharprparen,  // #( introduces a vector constant
    TSharpe,       // #e number notation
    TSharpi,       // #i number notation
    TSharpb,       // #b number notation
    TSharpo,       // #o number notation
    TSharph,       // #d number notation
    TSharpx,       // #x number notation
    TIf,           // if
    TElse,         // else
    TCond,         // cond
    TCase,         // case
    TAnd,          // and
    TOr,           // or
    TLet,          // let
    TLetrec,       // letrec
    TLetsyntax,    // let-syntax
    TLetrecsyntax, // letrec-syntax
    TCar,          // car
    TSetcar,       // set-car!
    TStringset,    // string-set!
    TStringref,    // string-ref
    TVectorref,     // vector-ref
    TBegin,        // begin
    TDo,           // do
    TDefine,       // define
    TQuote,        // quote
    TLambda,       // lambda
    TVariable,
    TNumber,
    TString,
    // [, ], {, }, | are reserved
}

pub struct Parser {
    repl: bool,
    current_token: TokenType,
}

impl Parser {
    pub fn new(len: usize) -> Parser {
        let mut repl = false;
        if len == 1 {
            repl = true;
        }

        Parser {
            repl,
            current_token: TokenType::TNone
        }
    }

    pub fn repl(&self) {
        print!("Scheme-rs REPL ('exit' or 'quit' to terminate session)\n");
        let mut input = String::new();
        loop {
            // Todo: Implement proper sighandling
            print!(">> ");
            io::stdout().flush().unwrap();
            io::stdin().read_line(&mut input).expect("Unexpected read_line() failure");
            print!("{}", input);
            let mut input: String = input.trim().to_string();
            if input == "exit" || input == "quit" {
                break;
            }
            input.clear();
        }
    }

    pub fn compile(&self, mut input: String) {
        let f = File::open(&input);
        input.clear();
        match f {
            Ok(mut file) => {
                file.read_to_string(&mut input)
                        .expect(format!("Failed to read input file '{:?}'", &input).as_str());
            },
            Err(error) => {
                panic!("Failed to open input file '{:?}' with {:?}", &input, error);
            }
        }
    }

    /*fn next_token() -> TokenType {

    }*/

    pub fn compile_to_ast() -> () {

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