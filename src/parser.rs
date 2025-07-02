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
    Literal,
    Quotation,
    SelfEvaluating,
    Datum,
    SimpleDatum,
    CompoundDatum,
    List,
    Vector,
    Abbreviation,
    AbbrevPrefix,
    Symbol,
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
    fn open(kind: TreeKind, start: usize) -> Tree {
        Tree {
            kind,
            start,
            end: start,
            children: Some(Vec::new())
        }
    }

    fn leaf(kind: TreeKind, token_at_leaf: &Token) -> Tree {
        Tree {
            kind,
            start: token_at_leaf.start,
            end: token_at_leaf.end,
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
        assert_ne!(self.kind, TreeKind::Keyword);
        assert_ne!(self.kind, TreeKind::Error);

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
        let children = &self.children;

        match children {
            Some(vec) => {
                println!("{indent}{:?}", self.kind);

                for child in vec{
                    child.print(level + 1, lexer);
                }
            },
            None => {
                let literal = &lexer.code[self.start..self.end];
                
                if matches!(self.kind, TreeKind::Keyword | TreeKind::Variable | TreeKind::Boolean | TreeKind::Number | TreeKind::Character | TreeKind::String) {
                    println!("{indent}{:?}", self.kind);
                    println!("  {indent}{:?}", literal);
                    return
                } else if matches!(self.kind, TreeKind::Error) {
                    println!("{indent}{:?}", self.kind);
                    return
                }
                
                let literal = &lexer.code[self.start..self.end];
                println!("{indent}{:?}", literal);
            }
        }
    }
}

pub struct Parser<'a> {
    lexer: Lexer<'a>,
    curr_token: Token,
    after_curr: Token,
    pub error_log: Vec<String>,
}

impl Parser<'_>{
    pub fn new(lexer: Lexer<'_>) -> Parser {
        Parser {
            lexer: lexer,
            curr_token: Token{kind: TokenKind::Eof, start: 0, end: 1},
            after_curr: Token{kind: TokenKind::Eof, start: 0, end: 1},
            error_log: Vec::new()
        }
    }

    // skip tokens until parser reaches non-whitespace delimiter token
    fn synchronize_from_parse_error(&mut self) {
        while !matches!(self.curr_token.kind, TokenKind::Lparen | TokenKind::Rparen | TokenKind::String | TokenKind::Semicolon | TokenKind::Eof) {
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

    // checks that the token that follows curr_token is of kind, but doesn't update curr_token in the parser
    fn after(&mut self, kind: TokenKind) -> bool {
        self.after_curr = match self.lexer.next_token() {
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
        };

        self.after_curr.kind == kind 
    }

    fn at_any(&self, kinds: &[TokenKind]) -> bool {
        for kind in kinds {
            if self.at(*kind) {
                return true
            }
        }
        false
    }

    fn expect(&mut self, curr_tree: &mut Tree, kind: TokenKind) {
        if self.at(kind) {
            curr_tree.add_child(Tree::leaf(TreeKind::Token, &self.curr_token));
            return
        }

        curr_tree.add_child(Tree::leaf(TreeKind::Error, &self.curr_token));
        self.add_error_msg_to_log(format!("Expected a {:?}", kind).as_str());
        self.synchronize_from_parse_error();
    }

    fn at_keyword(&self) -> bool {
        matches!(self.curr_token.kind, TokenKind::Else | TokenKind::Arrow | TokenKind::Define
        | TokenKind::Unquote | TokenKind::UnquoteSplicing | TokenKind::Quote
        | TokenKind::Lambda | TokenKind::If | TokenKind::SetExPt | TokenKind::Begin
        | TokenKind::Cond | TokenKind::And | TokenKind::Or
        | TokenKind::Case | TokenKind::Let | TokenKind::LetStar
        | TokenKind::LetRec | TokenKind::Do | TokenKind::Delay
        | TokenKind::Quasiquote)
    }

    fn list(&mut self) -> Tree {
        let mut list = Tree::open(TreeKind::List, self.curr_token.start);

        if self.at(TokenKind::Lparen) {
            list.add_child(Tree::leaf(TreeKind::Token, &self.curr_token));
            self.advance();
            while !self.at_any(&[TokenKind::Rparen, TokenKind::Eof, TokenKind::Dot]) {
                list.add_child(self.datum());
                self.advance();
            }

            if list.children_count() > 2 && self.at(TokenKind::Dot) {
                list.add_child(Tree::leaf(TreeKind::Token, &self.curr_token));
                self.advance();
                list.add_child(self.datum());
                self.advance();
            }
            self.expect(&mut list, TokenKind::Rparen);
        } else if self.at_any(&[TokenKind::Squote, TokenKind::Bquote, TokenKind::Comma, TokenKind::Seqcomma]) {
            let mut abbreviation = Tree::open(TreeKind::Abbreviation, self.curr_token.start);
            let mut abbrev_prefix = Tree::open(TreeKind::AbbrevPrefix, self.curr_token.start);
            abbrev_prefix.add_child(Tree::leaf(TreeKind::Token, &self.curr_token));
            abbrev_prefix.close(self.lexer.index);
            abbreviation.add_child(abbrev_prefix);
            self.advance();
            abbreviation.add_child(self.datum());
            abbreviation.close(self.lexer.index);
            list.add_child(abbreviation);
        }
        
        list.close(self.lexer.index);
        list
    }

    fn vector(&mut self) -> Tree {
        let mut vector = Tree::open(TreeKind::Vector, self.curr_token.start);
        vector.add_child(Tree::leaf(TreeKind::Token, &self.curr_token));
        self.advance();
        while !self.at_any(&[TokenKind::Rparen, TokenKind::Eof]) {
            vector.add_child(self.datum());
            self.advance();
        }
        self.expect(&mut vector, TokenKind::Rparen);
        vector.close(self.lexer.index);
        vector
    }

    fn datum(&mut self) -> Tree {
        let mut datum = Tree::open(TreeKind::Datum, self.curr_token.start);
        if self.at(TokenKind::Lparen) | self.at_any(&[TokenKind::Squote, TokenKind::Bquote, TokenKind::Comma, TokenKind::Seqcomma]){
            // List
            let mut compound_datum = Tree::open(TreeKind::CompoundDatum, self.curr_token.start);
            compound_datum.add_child(self.list());
            compound_datum.close(self.lexer.index);
            datum.add_child(compound_datum);
        } else if self.at(TokenKind::Sharplparen) {
            // Vectors
            let mut compound_datum = Tree::open(TreeKind::CompoundDatum, self.curr_token.start);
            compound_datum.add_child(self.vector());
            compound_datum.close(self.lexer.index);
            datum.add_child(compound_datum);
        } else {
            let mut simple_datum = Tree::open(TreeKind::SimpleDatum, self.curr_token.start);

            if self.at(TokenKind::True) || self.at(TokenKind::False) {
                simple_datum.add_child(Tree::leaf(TreeKind::Boolean, &self.curr_token));
            } else if self.at(TokenKind::String) {
                simple_datum.add_child(Tree::leaf(TreeKind::String, &self.curr_token));
            } else if self.at(TokenKind::Number) {
                simple_datum.add_child(Tree::leaf(TreeKind::Number, &self.curr_token));
            } else if self.at(TokenKind::Character) {
                simple_datum.add_child(Tree::leaf(TreeKind::Character, &self.curr_token))
            } else if self.at(TokenKind::Variable) {
                let mut symbol = Tree::open(TreeKind::Symbol, self.curr_token.start);
                symbol.add_child(Tree::leaf(TreeKind::Variable, &self.curr_token));
                symbol.close(self.lexer.index);
                simple_datum.add_child(symbol);
            } else if self.at_keyword() {
                let mut symbol = Tree::open(TreeKind::Symbol, self.curr_token.start);
                symbol.add_child(Tree::leaf(TreeKind::Keyword, &self.curr_token));
                symbol.close(self.lexer.index);
                simple_datum.add_child(symbol);
            }

            simple_datum.close(self.lexer.index);
            datum.add_child(simple_datum);
        }

        datum.close(self.lexer.index);
        datum
    }

    fn literal(&mut self) -> Tree {
        let mut literal = Tree::open(TreeKind::Literal, self.curr_token.start);
        if self.at(TokenKind::Squote) {
            let mut quotation = Tree::open(TreeKind::Quotation, self.curr_token.start);
            quotation.add_child(Tree::leaf(TreeKind::Token, &self.curr_token));
            self.advance();
            quotation.add_child(self.datum());
            quotation.close(self.lexer.index);
            literal.add_child(quotation);
        } else if self.at(TokenKind::Lparen) && self.after_curr.kind == TokenKind::Quote {
            let mut quotation = Tree::open(TreeKind::Quotation, self.curr_token.start);
            quotation.add_child(Tree::leaf(TreeKind::Token, &self.curr_token));
            
            let mut symbol = Tree::open(TreeKind::Symbol, self.after_curr.start);
            symbol.add_child(Tree::leaf(TreeKind::Keyword, &self.after_curr));
            symbol.close(self.lexer.index);
            quotation.add_child(symbol);

            self.advance();
            quotation.add_child(self.datum());
            self.advance();
            self.expect(&mut quotation, TokenKind::Rparen);
            quotation.close(self.lexer.index);
            literal.add_child(quotation);
        } else {
            let mut selfeval = Tree::open(TreeKind::SelfEvaluating, self.curr_token.start);

            if self.at_any(&[TokenKind::False, TokenKind::True]) {
                selfeval.add_child(Tree::leaf(TreeKind::Boolean, &self.curr_token));
            } else if self.at(TokenKind::Number) {
                selfeval.add_child(Tree::leaf(TreeKind::Number, &self.curr_token));
            } else if self.at(TokenKind::Character) {
                selfeval.add_child(Tree::leaf(TreeKind::Character, &self.curr_token));
            } else if self.at(TokenKind::String) {
                selfeval.add_child(Tree::leaf(TreeKind::String, &self.curr_token));
            }

            selfeval.close(self.lexer.index);
            literal.add_child(selfeval);
        }

        literal.close(self.lexer.index);
        literal
    }
    
    fn expression(&mut self) -> Tree {
        let mut expression = Tree::open(TreeKind::Expression, self.curr_token.start);
        if self.at(TokenKind::Variable) {
            expression.add_child(Tree::leaf(TreeKind::Variable, &self.curr_token));
        } else if self.at_any(&[TokenKind::Squote, TokenKind::True, TokenKind::False, TokenKind::Number, TokenKind::Character, TokenKind::String]) {
            expression.add_child(self.literal());
        } else if self.at(TokenKind::Lparen) && self.after(TokenKind::Quote) {
            expression.add_child(self.literal());
        } else if self.at(TokenKind::Lparen) {
            // TODO
        } else {
            expression.add_child(Tree::leaf(TreeKind::Error, &self.curr_token));
            self.add_error_msg_to_log("Expected an expression");
            self.synchronize_from_parse_error();
        }

        expression.close(self.lexer.index);
        expression
    }

    pub fn generate_parse_tree(&mut self) -> Tree {
        let start = self.lexer.index;
        let mut root = Tree::open(TreeKind::File, start);
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