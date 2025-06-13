use std::env;
use std::fs::File;
use std::io::prelude::*;
use std::str::Chars;
use std::io;

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
enum TokenKind {
    Eof,            // end-of-file
    Error,          // error token
    Point,          // .
    Lparen,         // (
    Rparen,         // )
    Squote,         // '
    Bquote,         // `
    Comma,          // ,
    Seqcomma,       // ,@
    Bslash,         // \
    True,           // #t true
    False,          // #f false
    Sharpbslash,    // #\ introduces a character constant
    Sharplparen,    // #( introduces a vector constant
    SharpE,         // #e number notation (exactness)
    SharpI,         // #i number notation (exactness)
    SharpB,         // #b number notation (binary)
    SharpO,         // #o number notation (octal)
    SharpH,         // #d number notation (decimal)
    SharpX,         // #x number notation (hexadecimal)
    Else,           // else
    Arrow,          // =>
    Define,         // define
    Unquote,        // unquote
    UnquoteSplicing,// unquote-splicing
    Quote,          // quote
    Lambda,         // lambda
    If,             // if
    SetExPt,        // set!
    Begin,          // begin
    Cond,           // cond
    And,            // and
    Or,             // or
    Case,           // case
    Let,            // let
    LetStar,        // let*
    LetRec,         // letrec
    Do,             // do
    Delay,          // delay
    Quasiquote,     // quasiquote
    Variable,
    Number,
    String,
    // [, ], {, }, | are reserved
}

#[derive(Debug)]
pub struct Token {
    kind: TokenKind,
    start: usize,
    len: usize,
}

pub struct Parser {
    repl: bool,
}

pub struct Lexer<'a> {
    code: &'a str,
    code_itr: Chars<'a>,
    index: usize,
}

impl Lexer<'_> {
    pub fn new<'a>(code: &'a str, code_itr: Chars<'a>) -> Lexer<'a> {
        Lexer {
            code,
            code_itr,
            index: 0
        }
    }

    fn step(&mut self) -> Option<char> {
        // returns the character that follows the current iterator position,
        // advances the iterator/index
        // return None at end of file, else return char
        match self.code_itr.next() {
            Some(c) => {
                self.index = self.index + 1;
                Some(c)
            },
            None => None
        }
    }

    fn peek(&mut self) -> Option<char> {
        // return the character that follows the current iterator position,
        // but doesn't advance the iterator/index
        // return None at end of file, else return char
        match self.code_itr.clone().peekable().peek() {
            Some(c) => Some(*c),
            None => None
        }
    }

    fn syntactic_keyword_or_variable(&self, identifier: &str) -> TokenKind {
        match identifier {
            "else" => TokenKind::Else,
            "=>" => TokenKind::Arrow,
            "define" => TokenKind::Define,
            "unquote" => TokenKind::Unquote,
            "unquote-splicing" => TokenKind::UnquoteSplicing,
            "quote" => TokenKind::Quote,
            "lambda" => TokenKind::Lambda,
            "if" => TokenKind::If,
            "set!" => TokenKind::SetExPt,
            "begin" => TokenKind::Begin,
            "cond" => TokenKind::Cond,
            "and" => TokenKind::And,
            "or" => TokenKind::Or,
            "case" => TokenKind::Case,
            "let" => TokenKind::Let,
            "let*" => TokenKind::LetStar,
            "letrec" => TokenKind::LetRec,
            "do" => TokenKind::Do,
            "delay" => TokenKind::Delay,
            "quasiquote" => TokenKind::Quasiquote,
            _ => TokenKind::Variable
        }
    }

    fn make_identifier(&self, start: usize) -> Token {
        let identifier = &self.code[start..self.index];
        let identifier = self.syntactic_keyword_or_variable(identifier.to_lowercase().as_str());
        Token{kind: identifier, start: start, len: self.index - start}
    }

    fn is_subsequent(&self, c: &char) -> bool {
        matches!(c, 'A'..='Z' | 'a'..='z' | '!' | '$' | '%' | '*' | '/' | ':' | '<' | '=' | '>' | '^' | '_' | '~' | '0'..='9' | '+' | '-' | '.' | '@')
    }

    fn is_delimiter(&self, c: &char) -> bool {
        c.is_whitespace() | matches!(c, '(' | ')' | '"' | ';')
    }

    fn next_token(&mut self) -> Result<Token, String> {
        // return error message if an error, token if valid token

        /*
        https://web.stanford.edu/class/me469b/download/scheme.pdf
        See Section 7.1.1
        Note: <whitespace>, '(', ')', '"', ';' are token delimiters
        */
        
        let start = self.index;
        let c = match self.step() {
            Some(c) => c,
            None => return Ok(Token{kind: TokenKind::Eof, start, len: 0})
        };

        match c {
            '.' => Ok(Token{kind: TokenKind::Point, start, len: 1}),
            '(' => Ok(Token{kind: TokenKind::Lparen, start, len: 1}),
            ')' => Ok(Token{kind: TokenKind::Rparen, start, len: 1}),
            '\'' => Ok(Token{kind: TokenKind::Squote, start, len: 1}),
            '`' => Ok(Token{kind: TokenKind::Bquote, start, len: 1}),
            ',' => {
                match self.peek() {
                    Some('@') => {
                        _ = self.step();
                        Ok(Token{kind: TokenKind::Seqcomma, start, len: 2})
                    },
                    _ => Ok(Token{kind: TokenKind::Comma, start, len: 1})
                }
            },
            '#' => {
                match self.peek() {
                    Some('t') => {
                        _ = self.step();
                        Ok(Token{kind: TokenKind::True, start, len: 2})
                    },
                    Some('f') => {
                        _ = self.step();
                        Ok(Token{kind: TokenKind::False, start, len: 2})
                    },
                    Some('\\') => {
                        _ = self.step();
                        Ok(Token{kind: TokenKind::Sharpbslash, start, len: 2})
                    },
                    Some('(') => {
                        _ = self.step();
                        Ok(Token{kind: TokenKind::Sharplparen, start, len: 2})
                    },
                    Some('e') => {
                        _ = self.step();
                        Ok(Token{kind: TokenKind::SharpE, start, len: 2})
                    },
                    Some('i') => {
                        _ = self.step();
                        Ok(Token{kind: TokenKind::SharpI, start, len: 2})
                    },
                    Some('b') => {
                        _ = self.step();
                        Ok(Token{kind: TokenKind::SharpB, start, len: 2})
                    },
                    Some('o') => {
                        _ = self.step();
                        Ok(Token{kind: TokenKind::SharpO, start, len: 2})
                    },
                    Some('h') => {
                        _ = self.step();
                        Ok(Token{kind: TokenKind::SharpH, start, len: 2})
                    },
                    Some('x') => {
                        _ = self.step();
                        Ok(Token{kind: TokenKind::SharpX, start, len: 2})
                    },
                    _ => {
                        let char_as_str = &self.code[start..start];
                        Err(format!("Unexpected '{:?}' following a '#'", char_as_str))
                    }
                }
            }
            c if matches!(c, '+' | '-' | '*' | '/') => {
                Ok(Token{kind: TokenKind::Variable, start, len: 1})
            },
            c if matches!(c, 'A'..='Z' | 'a'..='z' | '!' | '$' | '%' | '*' | '/' | ':' | '<' | '=' | '>' | '^' | '_' | '~' ) => {
                // Take a peek of the character that follows a letter 
                // or special initial as described in Section 7.1.1
                let mut d = match self.peek() {
                    Some(d) => d,
                    None => return Ok(self.make_identifier(start))
                };

                // While the character is a subsequent and *not* a delimiter, 
                // proceed the iterator by calling step(). See Section 7.1.1 
                // for the formal description of 'subsequent'
                while !self.is_delimiter(&d) & self.is_subsequent(&d) {
                    _ = match self.step() {
                        Some(d) => d,
                        None => return Ok(self.make_identifier(start))
                    };

                    d = match self.peek() {
                        Some(d) => d,
                        None => return Ok(self.make_identifier(start))
                    }
                }

                return Ok(self.make_identifier(start));
            },
            c if matches!(c, '"') => {
                loop {
                    match self.step() {
                        Some(next) => {
                            match next {
                                '"' => return Ok(Token{kind: TokenKind::String, start, len: self.index - start + 1}),
                                _ =>  continue,
                            }
                        }
                        None => return Err(String::from("Unexpected end of file: string literal not terminated")),
                    }
                }
            },
            _ => Err("Not reachable".to_string())
            // Suggested PRs
            // TODO: Extracting number tokens for all the different possible 
            // kinds (binary, octal, decimal, hexadecimal) as specified in Section 7.1.1
            // TODO: Extracting character tokens as specified in Section 7.1.1
        }
    }
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
        let mut input = String::new();
        loop {
            // Todo: Implement proper sighandling
            print!("scheme-rs> ");
            io::stdout().flush().unwrap();
            io::stdin().read_line(&mut input).expect("Unexpected read_line() failure");
            print!("{}", input);
            
            let mut input: String = input.trim().to_string();
            if input == "exit" || input == "quit" {
                break;
            }
            
            let _ast = self.generate_ast(&input);
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

    pub fn generate_ast(&self, code: &str) -> () {
        let mut lexer = Lexer::new(code, code.chars());
        let current_token = lexer.next_token().unwrap();
        print!("{:?}", current_token)
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

mod tests;

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
