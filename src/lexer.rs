// TODO: Lose the Exactness and Radix information collected during Lexing. We'd just properly parse the numbers
// in the parser. For now just validate that they are number tokens inputted in the form that the Scheme paper
// has defined numbers to be

// TODO: Change stepped_sign to enum

use std::str::Chars;
use std::fmt;

#[derive(Debug, PartialEq, Eq)]
pub enum Exactness {
    Exact,
    Inexact,
    Empty // implied inexact
}

#[derive(Debug, PartialEq, Eq)]
pub enum Radix {
    Binary,
    Octal,
    Decimal,
    Empty, // implied decimal
    Hexadecimal
}

#[derive(Debug, PartialEq, Eq, Clone, Copy)]
pub enum TokenKind {
    Eof,            // end-of-file
    //Whitespace,     // space, newline
    Lparen,         // (
    Rparen,         // )
    Squote,         // '
    Bquote,         // `
    Comma,          // ,
    Semicolon,      // ;
    Dot,            // .
    Seqcomma,       // ,@
    Bslash,         // \
    True,           // #t true
    False,          // #f false
    Sharp,          // #
    Sharpbslash,    // #\ introduces a character constant
    Sharplparen,    // #( introduces a vector constant
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
    DefineSyntax,   // define-syntax
    Variable,
    Number,
    String,
    Character,
    // [, ], {, }, | are reserved
}

#[derive(Clone, Copy)]
pub struct Token {
    pub kind: TokenKind,
    pub start: usize,
    pub end: usize,
}

pub struct Lexer<'a> {
    pub code: &'a str,
    code_itr: Chars<'a>,
    curr_char: char,
    pub index: usize
}

impl Lexer<'_> {
    pub fn new<'a>(code: &'a str, code_itr: Chars<'a>) -> Lexer<'a> {
        Lexer {
            code,
            code_itr,
            curr_char: '0',
            index: 0
        }
    }

    fn step(&mut self) -> char {
        // returns the character that follows the current iterator position,
        // advances the iterator/index
        // return '\0' at end of file, else return char
        match self.code_itr.next() {
            Some(c) => {
                self.index = self.index + 1;
                self.curr_char = c;
                c
            },
            None => '\0'
        }
    }

    fn peek(&mut self) -> char {
        // return the character that follows the current iterator position,
        // but doesn't advance the iterator/index
        // return '\0' at end of file, else return char
        match self.code_itr.clone().peekable().peek() {
            Some(c) => *c,
            None => '\0'
        }
    }

    fn synchronize_to_delimiter_after_error(&mut self) {
        let mut c = self.peek();
        while !self.is_delimiter(&c) {
            _ = self.step();
            c = self.peek();
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
            "define-syntax" => TokenKind::DefineSyntax,
            _ => TokenKind::Variable
        }
    }

    fn make_identifier(&self, start: usize) -> Token {
        let identifier = &self.code[start..self.index];
        let identifier = self.syntactic_keyword_or_variable(identifier.to_lowercase().as_str());
        Token{kind: identifier, start: start, end: self.index}
    }

    fn step_number_exactness(&mut self, radix: Radix) -> Result<Exactness, String> {
        // at this function call, radix (#b, #d, #o or #x) has been stepped through
        let r = match radix {
            Radix::Binary => 2,
            Radix::Octal => 8,
            Radix::Decimal | Radix::Empty => 10,
            Radix::Hexadecimal => 16,
        };

        let mut c = self.peek();

        if matches!(c, '\0') {
            return Err(format!("SyntaxError: Unexpected end-of-file following radix-{} specifier", r))
        } else if matches!(c, '#') {
            _ = self.step();
            c = self.peek();

            if matches!(c, '\0') {
                return Err("SyntaxError: Unexpected end-of-file following '#' in number exactness specifier".to_string())
            } else if matches!(c, 'e') {
                _ = self.step();
                return Ok(Exactness::Exact)
            } else if matches!(c, 'i') {
                _ = self.step();
                return Ok(Exactness::Inexact)
            } else if self.is_delimiter(&c) {
                return Err(format!("SyntaxError: Unexpected '{}' delimiter character following '#' in number exactness specifier", self.get_delimiter_as_str(&c)))
            } else {
                _ = self.step();
                return Err(format!("SyntaxError: Unexpected '{}' character following '#' in number exactness specifier", c))
            }
        } else {
            match radix {
                Radix::Binary => {
                    if matches!(c, '0'..='1' | '+' | '-') {
                        return Ok(Exactness::Empty);
                    } else {
                        _ = self.step();
                        return Err(format!("SyntaxError: Unexpected '{}' character following radix-2 specifier. Expects exactness specifier or '0' or '1' or +/- sign", c));
                    }
                },
                Radix::Octal => {
                    if matches!(c, '0'..='7' | '+' | '-') {
                        return Ok(Exactness::Empty);
                    } else {
                        _ = self.step();
                        return Err(format!("SyntaxError: Unexpected '{}' character following radix-8 specifier. Expects exactness specifier or '0'..='7' or +/- sign", c));
                    }
                    },
                Radix::Hexadecimal => {
                    if matches!(c, '0'..='9' | 'a'..='f' | 'A'..='F' | '+' | '-') {
                        return Ok(Exactness::Empty);
                    } else {
                        _ = self.step();
                        return Err(format!("SyntaxError: Unexpected '{}' character following radix-16 specifier. Expects exactness specifier or '0'..='9' or 'a'..='f' (case-insensitive) or +/- sign", c));
                    }
                },
                Radix::Decimal | Radix::Empty => {
                    if matches!(c, '0'..='9' | '+' | '-' | '.') {
                        return Ok(Exactness::Empty);
                    } else {
                        return Err(format!("SyntaxError: Unexpected '{}' character following radix-16 specifier. Expects exactness specifier or '0'..='9' or '.' or +/- sign", c));
                    }
                }
            }
        }
    }

    fn step_number_radix(&mut self) -> Result<Radix, String> {
        // at this function call, exactness (#e or #i) has been stepped through
        let mut c = self.peek();
        
        if matches!(c, '\0') {
            return Err("SyntaxError: Unexpected <end-of-file> following number exactness specifier".to_string())
        } else if matches!(c, '#') {
            _ = self.step();
            c = self.peek();

            if matches!(c, '\0') {
                return Err("SyntaxError: Unexpected end-of-file following '#' in number radix specifier".to_string())
            } else if matches!(c, 'd') {
                _ = self.step();
                return Ok(Radix::Decimal)
            } else if matches!(c, 'b') {
                _ = self.step();
                return Ok(Radix::Binary)
            } else if matches!(c, 'o') {
                _ = self.step();
                return Ok(Radix::Octal)
            } else if matches!(c, 'x') {
                _ = self.step();
                return Ok(Radix::Hexadecimal)
            } else if self.is_delimiter(&c) {
                return Err(format!("SyntaxError: Unexpected '{}' delimiter character following '#' in number radix specifier", self.get_delimiter_as_str(&c)))
            } else {
                _ = self.step();
                return Err(format!("SyntaxError: Unexpected character '{}'. Expected number radix specifier following exactness specifier for non-decimal numbers", c))
            }
        } else if matches!(c, '0'..='9' | '.' | '+' | '-') {
            return Ok(Radix::Empty)
        } else if self.is_delimiter(&c) {
            _ = self.step();
            if c.is_whitespace() {
                return Err("SyntaxError: Unexpected whitespace character following number exactness specifier".to_string());
            }

            return Err(format!("SyntaxError: Unexpected '{}' delimiter character following number exactness specifier", c))
        } else {
            _ = self.step();
            return Err(format!("SyntaxError: Unexpected character '{}'. Expected number radix specifier following exactness specifier for non-decimal numbers", c))
        }
    }

    fn step_number_digits(&mut self, radix: &Radix) {
        let mut digit = self.peek();
        // <digit R>+
        while (matches!(radix, Radix::Binary) & matches!(digit, '0'..='1')) |
              (matches!(radix, Radix::Octal) & matches!(digit, '0'..='7')) |
              (matches!(radix, Radix::Decimal | Radix::Empty) &matches!(digit, '0'..='9')) |
              (matches!(radix, Radix::Hexadecimal) & matches!(digit, '0'..='9' | 'a'..='f' | 'A'..='F'))
        {
            _ = self.step();
            digit = self.peek();
        }
    }

    fn step_number_inexact_hash(&mut self) {
        let mut ch = self.peek();
        // #*
        while matches!(ch, '#') {
            _ = self.step();
            ch = self.peek();
        }
    }

    fn step_decimal_number_fractional(&mut self) -> Option<String> {
        self.step_number_digits(&Radix::Decimal);
        self.step_number_inexact_hash();
        let mut ch = self.peek();
        // switch to using pattern matching
        if matches!(ch, 'e' | 's' | 'f' | 'd' | 'l') {
            _ = self.step();
            ch = self.peek();
            if matches!(ch, '+' | '-') {
                _ = self.step();
            }
            ch = self.peek();
            if matches!(ch, '0'..='9') {
                self.step_number_digits(&Radix::Decimal);
            } else {
                return Some(format!("SyntaxError: Unexpected '{}' delimiter character. Expected radix-10 digits to follow exponent marker", self.get_delimiter_as_str(&ch)))
            }
        }
        None
    }

    fn step_decimal_number_suffix(&mut self) -> Option<String> {
        _ = self.step();
        let mut c = self.peek();
        if matches!(c, '+' | '-') {
            _ = self.step();
        }
                                        
        c = self.peek();
        if matches!(c, '0'..='9') {
            self.step_number_digits(&Radix::Decimal);
            None
        } else {
            return Some(format!("SyntaxError: Unexpected '{}' delimiter character. Expected radix-10 digits to follow exponent marker", self.get_delimiter_as_str(&c)))
        }
    }

    fn step_complex_number_operand(&mut self, radix: &Radix) -> Option<String> {
        if matches!(radix, Radix::Decimal | Radix::Empty) {
            match self.peek() {
                '.' => {
                    _ = self.step();
                    match self.peek() {
                        '0'..='9' => {
                            match self.step_decimal_number_fractional() {
                                Some(err) => return Some(err),
                                None => return None
                            }
                        },
                        ch => {
                            return Some(format!("SyntaxError: Unexpected '{}' delimiter character. Expected radix-10 digits to follow '.'", self.get_delimiter_as_str(&ch)))
                        }
                    }
                },
                '0'..='9' => {
                    self.step_number_digits(&radix);
                    match self.peek() {
                        '#' => {
                            _ = self.step();
                            self.step_number_inexact_hash();
                            match self.peek() {
                                '.' => {
                                    _ = self.step();
                                    self.step_number_inexact_hash();
                                    let c = self.peek();
                                    if matches!(c, 'e' | 's' | 'f' | 'd' | 'l') {
                                        match self.step_decimal_number_suffix() {
                                            Some(err) => return Some(err),
                                            None => return None
                                        }
                                    }
                                    return None
                                },
                                'e' | 's' | 'f' | 'd' | 'l' => {
                                    match self.step_decimal_number_suffix() {
                                        Some(err) => return Some(err),
                                        None => return None
                                    }
                                }
                                '/' => {
                                    _ = self.step();
                                    let c = self.peek();
                                    if matches!(c, '0'..='9') {
                                        self.step_number_digits(&radix);
                                    } else {
                                        return Some(format!("SyntaxError: Unexpected '{}'. Expected radix-10 digits to follow '/'", c))
                                    }
                                    self.step_number_inexact_hash();
                                    return None
                                }
                                _ => {
                                    return None
                                }
                            }
                        },
                        '.' => {
                            _ = self.step();
                            self.step_number_digits(&radix);
                            self.step_number_inexact_hash();
                            let c = self.peek();
                            if matches!(c, 'e' | 's' | 'f' | 'd' | 'l') {
                                match self.step_decimal_number_suffix() {
                                    Some(err) => return Some(err),
                                    None => return None
                                }
                            }
                            return None
                        },
                        'e' | 's' | 'f' | 'd' | 'l' => {
                            match self.step_decimal_number_suffix() {
                                Some(err) => return Some(err),
                                None => return None
                            }
                        },
                        '/' => {
                            _ = self.step();
                            let c = self.peek();
                            if matches!(c, '0'..='9') {
                                self.step_number_digits(&radix);
                            } else {
                                return Some(format!("SyntaxError: Unexpected '{}'. Expected radix-10 digits to follow '/'", c))
                            }
                            self.step_number_inexact_hash();
                            return None
                        }
                        _ => {
                            return None
                        }
                    }
                },
                c => {
                    return Some(format!("Unexpected '{}'. Expected only valid radix-10 digits", c))
                }
            }
        } else {
            let base = match radix {
                Radix::Binary => 2,
                Radix::Octal => 8,
                Radix::Hexadecimal => 16,
                Radix::Decimal | Radix::Empty => unreachable!()
            };

            let mut c = self.peek();
            if (matches!(radix, Radix::Binary) & !matches!(c, '0' | '1')) |
                (matches!(radix, Radix::Octal) & !matches!(c, '0'..='7')) | 
                (matches!(radix, Radix::Hexadecimal) & !matches!(c, '0'..='9' | 'a'..='f' | 'A'..='F'))
            {
                return Some(format!("Unexpected '{}'. Expected only valid radix-{} digits", c, base))
            }

            self.step_number_digits(&radix);
            self.step_number_inexact_hash();

            match self.peek() {
                '/' => {
                    _ = self.step();
                    c = self.peek();
                    if (matches!(radix, Radix::Binary) & !matches!(c, '0' | '1')) |
                        (matches!(radix, Radix::Octal) & !matches!(c, '0'..='7')) | 
                        (matches!(radix, Radix::Hexadecimal) & !matches!(c, '0'..='9' | 'a'..='f' | 'A'..='F'))
                    {
                        return Some(format!("Unexpected '{}'. Expected radix-{} digits to follow '/'", c, base))
                    }

                    self.step_number_digits(&radix);
                    self.step_number_inexact_hash();
                    return None
                }
                _ => {
                    return None
                }
            }
        }
    }

    fn step_number(&mut self, already_stepped_sign: bool, radix: &Radix) -> Option<String> {
        let mut c = self.peek();
        if !already_stepped_sign {
            if matches!(c, '+' | '-') {
                _ = self.step();
            } else if self.is_delimiter(&c) {
                return Some(format!("SyntaxError: Unexpected '{}' following prefix within supposed number token", self.get_delimiter_as_str(&c)))

            } else if matches!(c, '\0') {
                return Some("SyntaxError: Unexpected <end-of-file>".to_string())
            }
        }

        c = self.peek();
        if matches!(c, 'i') {
            _ = self.step();
            c = self.peek();
            
            if !self.is_delimiter(&c) & !matches!(c, '\0') {
                return Some(format!("SyntaxError: Unexpected '{}'. Expected delimiter to terminate number token", c))
            }
            return None
        } else if self.is_delimiter(&c) {
            return Some(format!("SyntaxError: Unexpected '{}'. within supposed number token", self.get_delimiter_as_str(&c)))
        } else if matches!(c, '\0') {
           return Some("SyntaxError: Unexpected <end-of-file>".to_string()) 
        }

        match self.step_complex_number_operand(&radix) {
            Some(err) => return Some(err),
            None => {
                c = self.peek();
            }
        }

        if matches!(c, '+' | '-') {
            _ = self.step();
            c = self.peek();
            
            if matches!(c, 'i') {
                _ = self.step();
                if !self.is_delimiter(&c) & !matches!(c, '\0') {
                    return Some(format!("SyntaxError: Unexpected '{}'. Expected delimiter to terminate number token", c))
                }
                return None
            }
            
            
            match self.step_complex_number_operand(&radix) {
                Some(err) => return Some(err),
                None => {
                    c = self.peek();
                    if matches!(c, 'i') {
                        _ = self.step();
                    
                        if !self.is_delimiter(&c) & !matches!(c, '\0') {
                            return Some(format!("SyntaxError: Unexpected '{}'. Expected delimiter to terminate number token", c))
                        }
                        return None
                    } else {
                        return Some(format!("SyntaxError: Unexpected '{}'. Expected 'i' to follow complex number lhs" , c))
                    }
                }
            }
        } else if matches!(c, '@') {
            _ = self.step();

            if matches!(c, '+' | '-') {
                _ = self.step();
            } else if self.is_delimiter(&c) {
                return Some(format!("SyntaxError: Unexpected '{}' following prefix within supposed number token", self.get_delimiter_as_str(&c)))

            } else if matches!(c, '\0') {
                return Some("SyntaxError: Unexpected <end-of-file>".to_string())
            }

            match self.step_complex_number_operand(&radix) {
                Some(err) => return Some(err),
                None => {
                    c = self.peek();
                }
            }

            if !self.is_delimiter(&c) & !matches!(c, '\0') {
                return Some(format!("SyntaxError: Unexpected '{}'. Expected delimiter to terminate number token", c))
            }
            return None
        } else if matches!(c, 'i') {
            _ = self.step();
                    
            if !self.is_delimiter(&c) & !matches!(c, '\0') {
                return Some(format!("SyntaxError: Unexpected '{}'. Expected delimiter to terminate number token", c))
            }
            return None
        } else if !self.is_delimiter(&c) & matches!(c, '\0') {
            return Some(format!("SyntaxError: Unexpected '{}'. Expected delimiter to terminate number token", c))
        }

        None        
    }
    
    fn is_subsequent(&self, c: &char) -> bool {
        matches!(c, 'A'..='Z' | 'a'..='z' | '!' | '$' | '%' | '*' | '/' | ':' | '<' | '=' | '>' | '?' | '^' | '_' | '~' | '0'..='9' | '+' | '-' | '.' | '@')
    }

    fn is_delimiter(&self, c: &char) -> bool {
        matches!(c, '(' | ')' | '"' | ';' | ' ' | '\n' | '\0' | '\r')
    }

    fn get_delimiter_as_str(&self, c: &char) -> String {
        match c {
            ' ' => "<space>".to_string(),
            '\n' => "<newline>".to_string(),
            '\0' => "<end-of-file>".to_string(),
            _ => c.to_string()
        }
    }
    
    pub fn next_token(&mut self) -> Result<Token, String> {
        // return error message if an error, token if valid token

        /*
        https://web.stanford.edu/class/me469b/download/scheme.pdf
        See Section 7.1.1
        Note: <whitespace>, '(', ')', '"', ';' are token delimiters
        */
        
        let mut c = self.peek();

        if c.is_whitespace() {
            _ = self.step();
            c = self.peek();
            while c.is_whitespace() {
                _ = self.step();
                c = self.peek();
            }
        }

        let start = self.index;

        match c {
            c if matches!(c, '+' | '-') => {
                _ = self.step();
                match self.peek() {
                    '0'..'9' => {
                        self.step_number(true, &Radix::Empty);
                        _ = self.step();
                        Ok(Token{kind: TokenKind::Number, start, end: self.index})
                    },
                    'i' => {
                        self.step_number(true, &Radix::Empty);
                        _ = self.step();
                        Ok(Token{kind: TokenKind::Number, start, end: self.index})
                    },
                    _ => {
                        Ok(Token{kind: TokenKind::Variable, start, end: self.index})
                    }
                }
            }
            c if matches!(c, '/' | '*') => {
                _ = self.step();
                Ok(Token{kind: TokenKind::Variable, start, end: self.index})
            },
            '.' => {
                _ = self.step();
                match self.peek() {
                    '0'..='9' => {
                        self.step_decimal_number_fractional();
                        _ = self.step();
                        Ok(Token{kind: TokenKind::Number, start, end: self.index})
                    }
                    _ => {
                        //.synchronize_to_delimiter_after_error();
                        //Err("SyntaxError: Expected decimal number token to follow '.' character".to_string())
                        _ = self.step();
                        Ok(Token{kind: TokenKind::Dot, start, end: self.index})
                    }
                }
            },
            '\0' => {
                _ = self.step();
                Ok(Token{kind: TokenKind::Eof, start, end: self.index})
            },
            '(' => {
                _ = self.step();
                Ok(Token{kind: TokenKind::Lparen, start, end: self.index})
            },
            ')' => {
                _ = self.step();
                Ok(Token{kind: TokenKind::Rparen, start, end: self.index})
            },
            '\'' => {
                _ = self.step();
                Ok(Token{kind: TokenKind::Squote, start, end: self.index})
            },
            '`' => {
                _ = self.step();
                Ok(Token{kind: TokenKind::Bquote, start, end: self.index})
            },
            ';' => {
                _ = self.step();
                Ok(Token{kind: TokenKind::Semicolon, start, end: self.index})
            },
            ',' => {
                _ = self.step();
                let c = self.peek();

                if matches!(c, '@') {
                    _ = self.step();
                    Ok(Token{kind: TokenKind::Seqcomma, start, end: self.index})
                } else {
                    Ok(Token{kind: TokenKind::Comma, start, end: self.index - 1})
                }
            },
            '#' => {
                _ = self.step();
                match self.peek() {
                    't' | 'T' => {
                        _ = self.step();
                        Ok(Token{kind: TokenKind::True, start, end: self.index})
                    },
                    'f' | 'F' => {
                        _ = self.step();
                        Ok(Token{kind: TokenKind::False, start, end: self.index})
                    },
                    '\\' => {
                        _ = self.step();
                        match self.peek() {
                            '\0' => {
                                self.synchronize_to_delimiter_after_error();
                                Err("SyntaxError: Unexpected end-of-file character. Expected a character to follow '#\\'".to_string())
                            },
                            _ => {
                                _ = self.step();
                                Ok(Token{kind: TokenKind::Character, start: start + 2, end: self.index})
                            }                                
                        }
                    },
                    '(' => {
                        _ = self.step();
                        Ok(Token{kind: TokenKind::Sharplparen, start, end: self.index})
                    },
                    'e' | 'E' => {
                        _ = self.step();
                        let radix = match self.step_number_radix() {
                            Ok(r) => r,
                            Err(err) => {
                                self.synchronize_to_delimiter_after_error();
                                return Err(err)
                            }
                        };

                        match self.step_number(false, &radix) {
                            Some(err) => return Err(err),
                            None => {
                                return Ok(Token{kind: TokenKind::Number, start, end: self.index});
                            }
                        }
                    },
                    'i' | 'I' => {
                        _ = self.step();
                        let radix = match self.step_number_radix() {
                            Ok(r) => r,
                            Err(err) => {
                                self.synchronize_to_delimiter_after_error();
                                return Err(err)
                            }
                        };

                        self.step_number(false, &radix);
                        return Ok(Token{kind: TokenKind::Number, start, end: self.index});
                    },
                    'b'| 'B' => {
                        _ = self.step();
                        let exactness = match self.step_number_exactness(Radix::Binary) {
                            Ok(exactness) => exactness,
                            Err(err) => {
                                self.synchronize_to_delimiter_after_error();
                                return Err(err)
                            }
                        };
                        self.step_number(false, &Radix::Binary);
                        return Ok(Token{kind: TokenKind::Number, start, end: self.index});
                    },
                    'o' | 'O' => {
                        _ = self.step();
                        let exactness = match self.step_number_exactness(Radix::Octal) {
                            Ok(exactness) => exactness,
                            Err(err) => {
                                self.synchronize_to_delimiter_after_error();
                                return Err(err)
                            }
                        };
                        self.step_number(false, &Radix::Octal);
                        return Ok(Token{kind: TokenKind::Number, start, end: self.index});
                    },
                    'x' | 'X' => {
                        _ = self.step();
                        let exactness = match self.step_number_exactness(Radix::Hexadecimal) {
                            Ok(exactness) => exactness,
                            Err(err) => {
                                self.synchronize_to_delimiter_after_error();
                                return Err(err)
                            }
                        };
                        self.step_number(false, &Radix::Hexadecimal);
                        return Ok(Token{kind: TokenKind::Number, start, end: self.index});
                    },
                    'd' | 'D' => {
                        _ = self.step();
                        let exactness = match self.step_number_exactness(Radix::Decimal) {
                            Ok(exactness) => exactness,
                            Err(err) => {
                                self.synchronize_to_delimiter_after_error();
                                return Err(err)
                            }
                        };
                        self.step_number(false, &Radix::Decimal);
                        return Ok(Token{kind: TokenKind::Number, start, end: self.index});
                    },
                    c => {
                        _ = self.step();
                        if self.is_delimiter(&c) {
                            self.synchronize_to_delimiter_after_error();
                            return Err("SyntaxError: '#' is not a valid token".to_string())
                        } else if matches!(c, '\0') {
                            self.synchronize_to_delimiter_after_error();
                            return Err("SyntaxError: Unexpected <end-of-file> following '#'".to_string())
                        }
                        
                        self.synchronize_to_delimiter_after_error();
                        return Err(format!("SyntaxError: Unexpected '{}' following a '#'", c))
                    }
                }
            },
            '0'..='9' => {
                self.step_number(false, &Radix::Empty);
                return Ok(Token{kind: TokenKind::Number, start, end: self.index});
            },
            c if matches!(c, 'A'..='Z' | 'a'..='z' | '!' | '$' | '%' | '*' | '/' | ':' | '<' | '=' | '>' | '^' | '_' | '~' ) => {
                // Take a peek of the character that follows a letter 
                // or special initial as described in Section 7.1.1
                let mut d = self.peek();

                // While the character is a subsequent, 
                // proceed the iterator by calling step(). See Section 7.1.1 
                // for the formal description of 'subsequent'
                while self.is_subsequent(&d) {
                    _ = self.step();

                    d = self.peek();
                }

                // When no more subsequents are found, the lexer
                // expects a delimiter. If *not* a delimiter, syntax error
                if !self.is_delimiter(&d) & !matches!(d, '\0'){
                    self.synchronize_to_delimiter_after_error();
                    return Err(format!("SyntaxError: Expected delimiter to terminate identifier token. Found '{}' instead", self.get_delimiter_as_str(&d)));
                }

                let identifier = self.make_identifier(start);
                return Ok(identifier);
            },
            c if matches!(c, '"') => {
                _ = self.step();

                loop {
                    match self.step() {
                        '\0' => {
                            self.synchronize_to_delimiter_after_error();
                            return Err(format!("SyntaxError: Unexpected end of file found before string literal termination"))
                        },
                        '"' => return Ok(Token{kind: TokenKind::String, start, end: self.index}),
                        '\\' => {
                            match self.peek() {
                                '\0' => {
                                    self.synchronize_to_delimiter_after_error();
                                    return Err(format!("SyntaxError: Unexpected end of file found before string literal termination"))
                                },
                                _ => {
                                    self.step();
                                    continue
                                }
                            }
                        }
                        _ => continue
                    }
                }
            },
            _ => {
                self.synchronize_to_delimiter_after_error();
                Err(format!("SyntaxError: '{}' is not a valid token", c))
            }
        }
    }
}