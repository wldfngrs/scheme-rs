// TODO: Improve error messages with source information for the tree node where the parse error occured

use crate::lexer::{Lexer, Token, Exactness, Radix, TokenKind};

enum NumberExactness {
    Exact,
    Inexact {
        upper: NumberKind
    }
}

enum NumberKind {
    Number,
    Complex {
        lhs: f64, 
        rhs: f64
    },
    Real(f64),
    Rational {
        num: f64,
        den: f64
    },
    Integer(i64)
}

/* Number {
        kind: NumberKind,
        radix: Radix,
        exactness: NumberExactness,
    },*/

#[derive(PartialEq, Debug)]
enum TreeKind {
    File,
    Token,
    Expression,
    Boolean,
    Number,
    Character,
    String,
    Datum,
    List,
    Vector,
    Abbreviation,
    Variable,
    Keyword,
    ProcedureCall,
    Operator,
    Operand,
    LambdaExpr,
    Formals,
    Body,
    Definition,
    Sequence,
    Command,
    Conditional,
    Test,
    Consequent,
    Alternate,
    Assignment,
    DerivedExpr,
    CondClause,
    CaseClause,
    BindingSpec,
    IterationSpec,
    DoResult,
    QuasiQuotation,
    Recipient,
    Init,
    Step,
    MacroUse,
    MacroBlock,
    Error
}

pub struct Tree {
    kind: TreeKind,
    start: usize,
    end: usize,
    children: Option<Vec<Tree>>
}

impl Tree {
    fn open(kind: TreeKind, start: usize, children: Option<Vec<Tree>>) -> Tree {
        Tree {
            kind,
            start,
            end: start,
            children
        }
    }

    fn leaf(kind: TreeKind, start: usize, end: usize) -> Tree {
        Tree {
            kind,
            start,
            end,
            children: None
        }
    }

    fn close(&mut self, end: usize) {
        self.end = end;
    }

    fn add_child(&mut self, child: Tree) {
        // asserts for invalid parent kinds, 
        // i.e these nodes don't expect children, 
        // therefore a call on this function with either of them should result in failure
        assert_ne!(self.kind, TreeKind::Boolean);
        assert_ne!(self.kind, TreeKind::Token);
        assert_ne!(self.kind, TreeKind::Character);
        assert_ne!(self.kind, TreeKind::Number);
        assert_ne!(self.kind, TreeKind::String);
        assert_ne!(self.kind, TreeKind::Boolean);
        assert_ne!(self.kind, TreeKind::Variable);

        match self.children {
            Some(ref mut vec) => {
                vec.push(child);
            },
            None => return
        }
    }

    fn children_count(&self) -> usize {
        match self.children {
            Some(ref vec) => vec.len(),
            None => 0
        }
    }

    fn print(&self, level: usize, lexer: &Lexer<'_>) {
        let indent = "  ".repeat(level);
        println!("{indent}{:?}", self.kind);
        let children = &self.children;
        
        match children {
            Some(vec) => {
                for child in vec{
                    child.print(level + 1, lexer);
                }
            },
            None => {
                let literal = &lexer.code[self.start..self.end];
                println!("  {indent}{:?}", literal);
            }
        }
    }
}

pub struct Parser<'a> {
    lexer: Lexer<'a>,
    curr_token: Token,
    pub error_log: Vec<String>,
}

impl Parser<'_>{
    pub fn new(lexer: Lexer<'_>) -> Parser {
        Parser {
            lexer: lexer,
            curr_token: Token{kind: TokenKind::Eof, start: 0, end: 1},
            error_log: Vec::new()
        }
    }

    // skip tokens until parser reaches non-whitespace delimiter token
    fn synchronize_from_parse_error(&mut self) {
        while matches!(self.curr_token.kind, TokenKind::Lparen | TokenKind::Rparen | TokenKind::String | TokenKind::Semicolon) {
            self.advance();
        }
    }

    fn add_error_msg_to_log(&mut self, expected: &str) {
        let as_str = &self.lexer.code[self.curr_token.start..self.curr_token.end];
        //let source_tree: = &self.lexer.code[..self.lexer.index];
        self.error_log.push(format!("ParseError: {}. Found '{}'({:?}) instead.", expected, as_str, self.curr_token.kind));
    }

    /*fn add_error_msg_to_log_with_source_tree_info(&mut self, tree: Tree, expected: &str) {
        let source_tree = &self.lexer.code[tree.start..tree.end];
        expected.to_string().push_str(format!(" at '{}'", source_tree).as_str());
        self.add_error_msg_to_log(expected);
    }*/
    
    fn advance(&mut self) {
        self.curr_token = match self.lexer.next_token() {
            Ok(token) => token,
            Err(error) => {
                self.error_log.push(error);
                let mut res = self.lexer.next_token();

                while res.is_err() {
                    self.error_log.push(res.err().unwrap());
                    res = self.lexer.next_token();
                }
                res.ok().unwrap()
            }
        }
    }

    fn at(&self, kind: TokenKind) -> bool {
        self.curr_token.kind == kind
    }

    fn expect(&mut self, kind: TokenKind) {
        if self.at(kind) {
            self.advance();
        }

        self.add_error_msg_to_log(format!("Expected a {:?}", kind).as_str());
        self.synchronize_from_parse_error();
    }

    fn is_at_keyword(&self) -> bool {
        matches!(self.curr_token.kind, TokenKind::Else | TokenKind::Arrow | TokenKind::Define
        | TokenKind::Unquote | TokenKind::UnquoteSplicing | TokenKind::Quote
        | TokenKind::Lambda | TokenKind::SetExPt | TokenKind::Begin
        | TokenKind::Cond | TokenKind::And | TokenKind::Or
        | TokenKind::Case | TokenKind::Let | TokenKind::LetStar
        | TokenKind::LetRec | TokenKind::Do | TokenKind::Delay
        | TokenKind::Quasiquote)
    }

    fn list(&mut self) -> Tree {
        let mut list = Tree::open(TreeKind::List, self.curr_token.start, Some(Vec::new()));
        // '('
        let child = Tree::leaf(TreeKind::Token, self.curr_token.start, self.curr_token.end);
        list.add_child(child);

        self.advance();
        while !self.at(TokenKind::Rparen) {
            list.add_child(self.datum());
            self.advance();
        }

        if list.children_count() > 2 && self.at(TokenKind::Dot) {
            self.advance();
            list.add_child(self.datum());
        }
        
        list.close(self.lexer.index);
        list
    }

    fn abbreviation(&mut self) -> Tree {
        let mut abbreviation = Tree::open(TreeKind::Abbreviation, self.curr_token.start, Some(Vec::new()));
        // abbreviation prefix
        let child = Tree::leaf(TreeKind::Token, self.curr_token.start, self.curr_token.end);
        abbreviation.add_child(child);
        
        self.advance();
        abbreviation.add_child(self.datum());

        abbreviation.close(self.lexer.index);
        abbreviation
    }

    fn datum(&mut self) -> Tree {
        self.advance();
        let mut datum = Tree::open(TreeKind::Datum, self.curr_token.start, Some(Vec::new()));
        if self.at(TokenKind::True) || self.at(TokenKind::False) {
            let child = Tree::leaf(TreeKind::Boolean, self.curr_token.start, self.curr_token.end);
            datum.add_child(child);
        } else if self.at(TokenKind::String) {
            let child = Tree::leaf(TreeKind::String, self.curr_token.start, self.curr_token.end);
            datum.add_child(child);
        } else if self.at(TokenKind::Number) {
            let child = Tree::leaf(TreeKind::Number, self.curr_token.start, self.curr_token.end);
            datum.add_child(child);
        } else if self.at(TokenKind::Character) {
            let child = Tree::leaf(TreeKind::Character, self.curr_token.start, self.curr_token.end);
            datum.add_child(child);
        } else if self.at(TokenKind::Variable) {
            let child = Tree::leaf(TreeKind::Variable, self.curr_token.start, self.curr_token.end);
            datum.add_child(child);
        } else if self.at(TokenKind::Lparen) {
            self.advance();
            datum.add_child(self.list());
            self.expect(TokenKind::Rparen);
        }
        else if self.at(TokenKind::Squote) || self.at(TokenKind::Bquote) || self.at(TokenKind::Comma) || self.at(TokenKind::Seqcomma) {
            self.advance();
            datum.add_child(self.abbreviation());
        } else if self.at(TokenKind::Sharplparen) {
            // TODO: VECTOR
        } else if self.is_at_keyword() {
            let child = Tree::leaf(TreeKind::Keyword, self.curr_token.start, self.curr_token.end);
            datum.add_child(child);
        } else {
            self.add_error_msg_to_log( "Expected a datum");
            self.synchronize_from_parse_error();
        }
        datum.close(self.lexer.index);
        datum
    }

    fn expression(&mut self) -> Tree {
        let mut expression = Tree::open(TreeKind::Expression, self.curr_token.start, Some(Vec::new()));
        if self.at(TokenKind::Variable) {
            let child = Tree::leaf(TreeKind::Variable, self.curr_token.start, self.curr_token.end);
            expression.add_child(child);
        } else if self.at(TokenKind::Squote) {
            let child = Tree::leaf(TreeKind::Token, self.curr_token.start, self.curr_token.end);
            expression.add_child(child);
            self.advance();
            expression.add_child(self.datum());
        } else if self.at(TokenKind::False) || self.at(TokenKind::True) {
            let child = Tree::leaf(TreeKind::Boolean, self.curr_token.start, self.curr_token.end);
            expression.add_child(child);
        } else if self.at(TokenKind::String) {
            let child = Tree::leaf(TreeKind::String, self.curr_token.start, self.curr_token.end);
            expression.add_child(child);
        } else if self.at(TokenKind::Number) {
            let child = Tree::leaf(TreeKind::Number, self.curr_token.start, self.curr_token.end);
            expression.add_child(child);
        } else if self.at(TokenKind::Character) {
            let child = Tree::leaf(TreeKind::Character, self.curr_token.start, self.curr_token.end);
            expression.add_child(child);
        } else if self.at(TokenKind::Lparen) {
            // TODO
        } else {
            self.add_error_msg_to_log("Expected an expression");
            self.synchronize_from_parse_error();
        }
        expression.close(self.lexer.index);
        expression
    }

    pub fn generate_parse_tree(&mut self) -> Tree {
        let start = self.lexer.index;
        let mut root = Tree::open(TreeKind::File, start, Some(Vec::new()));
        self.advance();
        loop {
            root.add_child(self.expression());
            self.advance();
            if self.curr_token.kind == TokenKind::Eof {
                break;
            }
        }
        root.close(self.lexer.index);
        root.print(0, &self.lexer);
        root
    }
}