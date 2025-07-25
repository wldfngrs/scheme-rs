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
    Program,
    CommandOrDefinition,
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
    DefFormals,
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

#[derive(PartialEq, Eq)]
enum BeginTerminalSite {
    AtCommandOrDefinition,
    AtDefinition
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
        // therefore a call on this function with any of them should result in failure
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
                if child.children_count() > 0 || matches!(child.kind, TreeKind::Token | TreeKind::Character | TreeKind::Number | TreeKind::String | TreeKind::Boolean | TreeKind::Variable | TreeKind::Keyword | TreeKind::Error) {
                    vec.push(child);
                }
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
    prev_token: Token,
    curr_token: Token,
    ahead_token: Token,
    //after_curr: Token,
    pub error_log: Vec<String>,
}

impl Parser<'_>{
    pub fn new(lexer: Lexer<'_>) -> Parser {
        Parser {
            lexer: lexer,
            prev_token: Token{kind: TokenKind::Eof, start: 0, end: 1},
            curr_token: Token{kind: TokenKind::Eof, start: 0, end: 1},
            ahead_token: Token{kind: TokenKind::Eof, start: 0, end: 1},
            error_log: Vec::new()
        }
    }

    // skip tokens until parser reaches non-whitespace delimiter token
    // TODO: Make whitespaces be possible synchronization points
    fn synchronize_from_parse_error(&mut self) {
        while !matches!(self.curr_token.kind, TokenKind::Lparen | TokenKind::Rparen | TokenKind::String | TokenKind::Semicolon | TokenKind::Eof) {
            self.advance();
        }
    }

    fn add_error_msg_to_log(&mut self, expected: &str) {
        let as_str = &self.lexer.code[self.curr_token.start..self.curr_token.end];
        //let source_tree: = &self.lexer.code[..self.curr_token.end];
        self.error_log.push(format!("ParseError: {}. Found '{}'({:?}) instead.", expected, as_str, self.curr_token.kind));
    }

    /*fn add_error_msg_to_log_with_source_tree_info(&mut self, tree: Tree, expected: &str) {
        let source_tree = &self.lexer.code[tree.start..tree.end];
        expected.to_string().push_str(format!(" at '{}'", source_tree).as_str());
        self.add_error_msg_to_log(expected);
    }*/

    fn init_token_cursor(&mut self) {
        assert_eq!(self.lexer.index, 0);
        self.advance();
        self.advance();
    }
    
    fn advance(&mut self) {
        self.prev_token = self.curr_token;
        self.curr_token = self.ahead_token;
        self.ahead_token = match self.lexer.next_token() {
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

    fn at_any(&self, kinds: &[TokenKind]) -> bool {
        for kind in kinds {
            if self.at(*kind) {
                return true
            }
        }
        false
    }

    fn ahead(&self, kind: TokenKind) -> bool {
        self.ahead_token.kind == kind
    }

    fn ahead_any(&self, kinds: &[TokenKind]) -> bool {
        for kind in kinds {
            if self.ahead(*kind) {
                return true
            }
        }
        false
    }

    fn expect(&mut self, curr_tree: &mut Tree, kind: TokenKind) {
        if self.at(kind) {
            let tree_kind = if kind == TokenKind::Variable {
                TreeKind::Variable
            } else {
                TreeKind::Token
            };
            curr_tree.add_child(Tree::leaf(tree_kind, &self.curr_token));
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
            abbrev_prefix.close(self.curr_token.end);
            abbreviation.add_child(abbrev_prefix);
            self.advance();
            abbreviation.add_child(self.datum());
            abbreviation.close(self.curr_token.end);
            list.add_child(abbreviation);
        } else {
            list.add_child(Tree::leaf(TreeKind::Error, &self.curr_token));
            self.add_error_msg_to_log("Expected a list");
            self.synchronize_from_parse_error();
        }
        
        list.close(self.curr_token.end);
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
        vector.close(self.curr_token.end);
        vector
    }

    fn datum(&mut self) -> Tree {
        let mut datum = Tree::open(TreeKind::Datum, self.curr_token.start);
        if self.at_any(&[TokenKind::Lparen, TokenKind::Squote, TokenKind::Bquote, TokenKind::Comma, TokenKind::Seqcomma]){
            // List
            let mut compound_datum = Tree::open(TreeKind::CompoundDatum, self.curr_token.start);
            compound_datum.add_child(self.list());
            compound_datum.close(self.curr_token.end);
            datum.add_child(compound_datum);
        } else if self.at(TokenKind::Sharplparen) {
            // Vectors
            let mut compound_datum = Tree::open(TreeKind::CompoundDatum, self.curr_token.start);
            compound_datum.add_child(self.vector());
            compound_datum.close(self.curr_token.end);
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
                symbol.close(self.curr_token.end);
                simple_datum.add_child(symbol);
            } else if self.at_keyword() {
                let mut symbol = Tree::open(TreeKind::Symbol, self.curr_token.start);
                symbol.add_child(Tree::leaf(TreeKind::Keyword, &self.curr_token));
                symbol.close(self.curr_token.end);
                simple_datum.add_child(symbol);
            } else {
                simple_datum.add_child(Tree::leaf(TreeKind::Error, &self.curr_token));
                self.add_error_msg_to_log("Expected a simple datum");
                self.synchronize_from_parse_error();
            }

            simple_datum.close(self.curr_token.end);
            datum.add_child(simple_datum);
        }

        datum.close(self.curr_token.end);
        datum
    }

    fn literal(&mut self) -> Tree {
        let mut literal = Tree::open(TreeKind::Literal, self.curr_token.start);
        if self.at(TokenKind::Squote) {
            let mut quotation = Tree::open(TreeKind::Quotation, self.curr_token.start);
            quotation.add_child(Tree::leaf(TreeKind::Token, &self.curr_token));
            self.advance();
            quotation.add_child(self.datum());
            quotation.close(self.curr_token.end);
            literal.add_child(quotation);
        } else if self.at(TokenKind::Lparen) && self.ahead(TokenKind::Quote) {
            let mut quotation = Tree::open(TreeKind::Quotation, self.curr_token.start);
            quotation.add_child(Tree::leaf(TreeKind::Token, &self.curr_token));
            
            self.advance();
            let mut symbol = Tree::open(TreeKind::Symbol, self.curr_token.start);
            symbol.add_child(Tree::leaf(TreeKind::Keyword, &self.curr_token));
            symbol.close(self.curr_token.end);
            quotation.add_child(symbol);

            self.advance();
            quotation.add_child(self.datum());
            self.advance();
            self.expect(&mut quotation, TokenKind::Rparen);
            quotation.close(self.curr_token.end);
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
            } else {
                selfeval.add_child(Tree::leaf(TreeKind::Error, &self.curr_token));
                self.add_error_msg_to_log("Expected a self-evaluating literal");
                self.synchronize_from_parse_error();
            }

            selfeval.close(self.curr_token.end);
            literal.add_child(selfeval);
        }

        literal.close(self.curr_token.end);
        literal
    }
    
    fn procedure_call(&mut self) -> Tree {
        let mut proc_call = Tree::open(TreeKind::ProcedureCall, self.curr_token.start);
        // '('
        proc_call.add_child(Tree::leaf(TreeKind::Token, &self.curr_token));
        
        self.advance();
        // operator
        let mut operator = Tree::open(TreeKind::Operator, self.curr_token.start);
        operator.add_child(self.expression());
        operator.close(self.curr_token.end);
        proc_call.add_child(operator);

        // operand
        let mut operand = Tree::open(TreeKind::Operand, self.curr_token.start);
        self.advance();
        while !self.at_any(&[TokenKind::Rparen, TokenKind::Eof]) {
            operand.add_child(self.expression());
            self.advance();
        }
        // curr is on eof or rparen
        operand.close(self.prev_token.end);
        proc_call.add_child(operand);
        self.expect(&mut proc_call, TokenKind::Rparen);

        proc_call.close(self.curr_token.end);
        proc_call
    }

    fn sequence(&mut self) -> Tree {
        let mut sequence = Tree::open(TreeKind::Sequence, self.curr_token.start);
        let mut command = Tree::open(TreeKind::Command, self.curr_token.start);
        command.add_child(self.expression());
        self.advance();
        // while at possible expression start token
        while self.at_any(&[TokenKind::Squote, TokenKind::True, 
                                TokenKind::False, TokenKind::Number, 
                                TokenKind::Character, TokenKind::String, 
                                TokenKind::Variable, TokenKind::Lparen, 
                                TokenKind::Bquote]) 
        {
            command.add_child(self.expression());
            self.advance();
        }
        command.close(self.prev_token.end);
        sequence.add_child(command);
        sequence.close(self.prev_token.end);
        sequence
    }

    fn body(&mut self) -> Tree {
        let mut body = Tree::open(TreeKind::Body, self.curr_token.start);
        
        while self.at(TokenKind::Lparen) && self.ahead_any(&[TokenKind::Define, TokenKind::Begin]) {
            body.add_child(self.definition(BeginTerminalSite::AtDefinition));
            self.advance();
        }

        body.add_child(self.sequence());

        body.close(self.curr_token.end);
        body
    }
    
    fn definition(&mut self, site: BeginTerminalSite) -> Tree {
        let mut definition = match site {
            BeginTerminalSite::AtDefinition => Tree::open(TreeKind::Definition, self.curr_token.start),
            BeginTerminalSite::AtCommandOrDefinition => Tree::open(TreeKind::CommandOrDefinition, self.curr_token.start)
        };

        self.expect(&mut definition, TokenKind::Lparen);
        self.advance();
        if self.at(TokenKind::Define) {
            definition.add_child(Tree::leaf(TreeKind::Token, &self.curr_token));
            self.advance();
            if self.at(TokenKind::Lparen) {
                definition.add_child(Tree::leaf(TreeKind::Token, &self.curr_token));
                self.advance();
                self.expect(&mut definition, TokenKind::Variable);
                self.advance();

                // def formals
                let mut def_formals = Tree::open(TreeKind::DefFormals, self.curr_token.start);
                while self.at(TokenKind::Variable) {
                    self.expect(&mut def_formals, TokenKind::Variable);
                    self.advance();
                }
                if self.at(TokenKind::Dot) {
                    self.advance();
                    self.expect(&mut def_formals, TokenKind::Variable);
                    def_formals.close(self.curr_token.end);
                    self.advance();
                } else {
                    def_formals.close(self.prev_token.end);
                    definition.add_child(def_formals);
                }
                self.expect(&mut definition, TokenKind::Rparen);
                self.advance();
                // body
                definition.add_child(self.body());
            } else {
                self.expect(&mut definition, TokenKind::Variable);
                self.advance();
                definition.add_child(self.expression());
                self.advance();
            }
        } else if self.at(TokenKind::Begin) {
            definition.add_child(Tree::leaf(TreeKind::Token, &self.curr_token));
            self.advance();
            
            if site == BeginTerminalSite::AtCommandOrDefinition {
                while !self.at_any(&[TokenKind::Rparen, TokenKind::Eof]) {
                    self.command_or_definition();
                    self.advance();
                }
            } else {
                while self.at(TokenKind::Lparen) {
                    definition.add_child(self.definition(BeginTerminalSite::AtDefinition));
                    self.advance();
                }
            }
        } else {
            definition.add_child(Tree::leaf(TreeKind::Error, &self.curr_token));
            self.add_error_msg_to_log("Expected a definition");
            self.synchronize_from_parse_error();
        }

        self.expect(&mut definition, TokenKind::Rparen);

        definition.close(self.curr_token.end);
        definition
    }

    fn lambda(&mut self) -> Tree {
        let mut lam = Tree::open(TreeKind::LambdaExpr, self.curr_token.start);
        // (
        lam.add_child(Tree::leaf(TreeKind::Token, &self.curr_token));
        
        self.advance();
        // lambda
        lam.add_child(Tree::leaf(TreeKind::Token, &self.curr_token));

        self.advance();
        // formals
        let mut formals = Tree::open(TreeKind::Formals, self.curr_token.start);
        if self.at(TokenKind::Lparen) {
            // (
            formals.add_child(Tree::leaf(TreeKind::Token, &self.curr_token));
            self.advance();
            while !self.at_any(&[TokenKind::Rparen, TokenKind::Eof, TokenKind::Dot]) {
                self.expect(&mut formals, TokenKind::Variable);
                self.advance();
            }

            if formals.children_count() > 2 && self.at(TokenKind::Dot) {
                self.advance();
                self.expect(&mut formals, TokenKind::Variable);
            }
            
            self.expect(&mut formals, TokenKind::Rparen);
        } else {
            self.expect(&mut formals, TokenKind::Variable);
        }
        formals.close(self.curr_token.end);
        lam.add_child(formals);
        
        // body
        self.advance();
        lam.add_child(self.body());

        self.expect(&mut lam, TokenKind::Rparen);

        lam.close(self.curr_token.end);
        lam
    }

    fn conditional(&mut self) -> Tree {
        let mut conditional = Tree::open(TreeKind::Conditional, self.curr_token.start);
        // (
        conditional.add_child(Tree::leaf(TreeKind::Token, &self.curr_token));

        self.advance();
        // if
        conditional.add_child(Tree::leaf(TreeKind::Token, &self.curr_token));

        self.advance();
        let mut test = Tree::open(TreeKind::Test, self.curr_token.start);
        test.add_child(self.expression());
        test.close(self.curr_token.end);
        conditional.add_child(test);

        self.advance();
        let mut consequent = Tree::open(TreeKind::Consequent, self.curr_token.start);
        consequent.add_child(self.expression());
        consequent.close(self.curr_token.end);
        conditional.add_child(consequent);

        self.advance();

        if self.at(TokenKind::Rparen) {
            self.expect(&mut conditional, TokenKind::Rparen);
            conditional.close(self.curr_token.end);
            return conditional
        }
        
        let mut alternate = Tree::open(TreeKind::Alternate, self.curr_token.start);
        alternate.add_child(self.expression());
        alternate.close(self.curr_token.end);
        conditional.add_child(alternate);

        self.advance();
        self.expect(&mut conditional, TokenKind::Rparen);

        conditional.close(self.curr_token.end);
        conditional
    }

    fn assignment(&mut self) -> Tree {
        let mut assignment = Tree::open(TreeKind::Assignment, self.curr_token.start);
        // (
        assignment.add_child(Tree::leaf(TreeKind::Token, &self.curr_token));
        self.advance();

        // set!
        assignment.add_child(Tree::leaf(TreeKind::Token, &self.curr_token));
        self.advance();

        self.expect(&mut assignment, TokenKind::Variable);

        self.advance();
        assignment.add_child(self.expression());

        self.advance();
        self.expect(&mut assignment, TokenKind::Rparen);

        assignment.close(self.curr_token.end);
        assignment
    }

    fn cond_clause(&mut self) -> Tree {
        let mut cond_clause = Tree::open(TreeKind::CondClause, self.curr_token.start);

        // (
        cond_clause.add_child(Tree::leaf(TreeKind::Token, &self.curr_token));

        self.advance();
        let mut test = Tree::open(TreeKind::Test, self.curr_token.start);
        test.add_child(self.expression());
        test.close(self.curr_token.end);
        cond_clause.add_child(test);

        self.advance();
        if self.at(TokenKind::Rparen) {
            cond_clause.add_child(Tree::leaf(TreeKind::Token, &self.curr_token));
        } else if self.at(TokenKind::Arrow) {
            // =>
            cond_clause.add_child(Tree::leaf(TreeKind::Token, &self.curr_token));

            self.advance();
            let mut recipient = Tree::open(TreeKind::Recipient, self.curr_token.start);
            recipient.add_child(self.expression());
            recipient.close(self.curr_token.end);
            cond_clause.add_child(recipient);

            self.advance();
            self.expect(&mut cond_clause, TokenKind::Rparen);
        } else {
            cond_clause.add_child(self.sequence());
            self.expect(&mut cond_clause, TokenKind::Rparen);
        }

        cond_clause.close(self.curr_token.end);
        cond_clause
    }
    
    fn case_clause(&mut self) -> Tree {
        let mut case_clause = Tree::open(TreeKind::CaseClause, self.curr_token.start);
        self.expect(&mut case_clause, TokenKind::Lparen);
        self.advance();
        
        self.expect(&mut case_clause, TokenKind::Lparen);
        self.advance();

        while !self.at_any(&[TokenKind::Rparen, TokenKind::Eof]) {
            case_clause.add_child(self.datum());
            self.advance();
        }

        self.expect(&mut case_clause, TokenKind::Rparen);
        self.advance();
        case_clause.add_child(self.sequence());
        self.expect(&mut case_clause, TokenKind::Rparen);

        case_clause.close(self.curr_token.end);
        case_clause
    }

    fn binding_spec(&mut self) -> Tree {
        let mut binding_spec = Tree::open(TreeKind::BindingSpec, self.curr_token.start);

        self.expect(&mut binding_spec, TokenKind::Lparen);

        self.advance();
        self.expect(&mut binding_spec, TokenKind::Variable);

        self.advance();
        binding_spec.add_child(self.expression());

        self.advance();
        self.expect(&mut binding_spec, TokenKind::Rparen);

        binding_spec.close(self.curr_token.end);
        binding_spec
    }

    fn iteration_spec(&mut self) -> Tree {
        let mut iteration_spec = Tree::open(TreeKind::IterationSpec, self.curr_token.start);

        self.expect(&mut iteration_spec, TokenKind::Lparen);
        
        self.advance();
        self.expect(&mut iteration_spec, TokenKind::Variable);

        self.advance();
        let mut init = Tree::open(TreeKind::Init, self.curr_token.start);
        init.add_child(self.expression());
        init.close(self.curr_token.end);
        iteration_spec.add_child(init);

        self.advance();
        if self.at(TokenKind::Rparen) {
            iteration_spec.add_child(Tree::leaf(TreeKind::Token, &self.curr_token));
            iteration_spec.close(self.curr_token.end);
            return iteration_spec
        }

        let mut step = Tree::open(TreeKind::Step, self.curr_token.start);
        step.add_child(self.expression());
        step.close(self.curr_token.end);
        iteration_spec.add_child(step);

        self.advance();
        self.expect(&mut iteration_spec, TokenKind::Rparen);

        iteration_spec.close(self.curr_token.end);
        iteration_spec
    }

    fn derived_expression(&mut self) -> Tree {
        // every derived expression but quasi-quotations get parsed here
        let mut derived_expr = Tree::open(TreeKind::DerivedExpr, self.curr_token.start);
        derived_expr.add_child(Tree::leaf(TreeKind::Token, &self.curr_token));
        self.advance();

        if self.at(TokenKind::Cond) {
            derived_expr.add_child(Tree::leaf(TreeKind::Token, &self.curr_token));
            self.advance();
            
            while self.at(TokenKind::Lparen) && self.ahead_any(&[TokenKind::Squote, TokenKind::True, 
                                                                            TokenKind::False, TokenKind::Number, 
                                                                            TokenKind::Character, TokenKind::String, 
                                                                            TokenKind::Variable, TokenKind::Lparen, 
                                                                            TokenKind::Bquote])
            {
                derived_expr.add_child(self.cond_clause());
                self.advance();
            }

            if derived_expr.children_count() > 2 && self.at(TokenKind::Rparen) {
                derived_expr.add_child(Tree::leaf(TreeKind::Token, &self.curr_token));
                derived_expr.close(self.curr_token.end);
                return derived_expr
            }

            self.expect(&mut derived_expr, TokenKind::Lparen);
            
            self.advance();
            self.expect(&mut derived_expr, TokenKind::Else);
            
            self.advance();
            derived_expr.add_child(self.sequence());
            self.expect(&mut derived_expr, TokenKind::Rparen);
            self.advance();
        } else if self.at(TokenKind::Case) {
            derived_expr.add_child(Tree::leaf(TreeKind::Token, &self.curr_token));

            self.advance();
            derived_expr.add_child(self.expression());

            self.advance();
            while self.at(TokenKind::Lparen) && self.ahead(TokenKind::Lparen) {
                derived_expr.add_child(self.case_clause());
                self.advance();
            }

            if derived_expr.children_count() > 2 && self.at(TokenKind::Rparen) {
                derived_expr.add_child(Tree::leaf(TreeKind::Token, &self.curr_token));
                derived_expr.close(self.curr_token.end);
                return derived_expr
            }

            self.expect(&mut derived_expr, TokenKind::Lparen);
            
            self.advance();
            self.expect(&mut derived_expr, TokenKind::Else);

            self.advance();
            derived_expr.add_child(self.sequence());
            self.expect(&mut derived_expr, TokenKind::Rparen);
            self.advance();
        } else if self.at(TokenKind::And) || self.at(TokenKind::Or) {
            // and | or
            derived_expr.add_child(Tree::leaf(TreeKind::Token, &self.curr_token));
            self.advance();

            let mut test = Tree::open(TreeKind::Test, self.curr_token.start);

            while self.at_any(&[TokenKind::Squote, TokenKind::True, 
                                                                            TokenKind::False, TokenKind::Number, 
                                                                            TokenKind::Character, TokenKind::String, 
                                                                            TokenKind::Variable, TokenKind::Lparen, 
                                                                            TokenKind::Bquote])
            {
                test.add_child(self.expression());
                self.advance();
            }

            test.close(self.curr_token.end);
            derived_expr.add_child(test);
        } else if self.at(TokenKind::Let) {
            derived_expr.add_child(Tree::leaf(TreeKind::Token, &self.curr_token));

            self.advance();
            if self.at(TokenKind::Variable) {
                derived_expr.add_child(Tree::leaf(TreeKind::Variable, &self.curr_token));
                self.advance();
            }

            self.expect(&mut derived_expr, TokenKind::Lparen);
            
            self.advance();
            while self.at(TokenKind::Lparen) && self.ahead(TokenKind::Variable) {
                derived_expr.add_child(self.binding_spec());
                self.advance();
            }

            self.expect(&mut derived_expr, TokenKind::Rparen);
            self.advance();
            derived_expr.add_child(self.body());
        } else if self.at_any(&[TokenKind::LetStar, TokenKind::LetRec]) {
            derived_expr.add_child(Tree::leaf(TreeKind::Token, &self.curr_token));

            self.advance();
            self.expect(&mut derived_expr, TokenKind::Lparen);

            self.advance();
            while self.at(TokenKind::Lparen) && self.ahead(TokenKind::Variable) {
                derived_expr.add_child(self.binding_spec());
                self.advance();
            }

            self.expect(&mut derived_expr, TokenKind::Rparen);
            self.advance();
            derived_expr.add_child(self.body());
        } else if self.at(TokenKind::Begin) {
            derived_expr.add_child(Tree::leaf(TreeKind::Token, &self.curr_token));

            self.advance();
            derived_expr.add_child(self.sequence());
        } else if self.at(TokenKind::Do) {
            derived_expr.add_child(Tree::leaf(TreeKind::Token, &self.curr_token));

            self.advance();
            self.expect(&mut derived_expr, TokenKind::Lparen);

            self.advance();
            while self.at(TokenKind::Lparen) && self.ahead(TokenKind::Variable) {
                derived_expr.add_child(self.iteration_spec());
                self.advance();
            }

            self.expect(&mut derived_expr, TokenKind::Rparen);

            self.advance();
            self.expect(&mut derived_expr, TokenKind::Lparen);

            self.advance();
            let mut test = Tree::open(TreeKind::Test, self.curr_token.start);
            test.add_child(self.expression());
            test.close(self.curr_token.end);
            derived_expr.add_child(test);

            self.advance();
            if !self.at(TokenKind::Rparen) {
                let mut do_result = Tree::open(TreeKind::DoResult, self.curr_token.start);

                do_result.add_child(self.sequence());

                do_result.close(self.curr_token.end);
                derived_expr.add_child(do_result);
            }
            self.expect(&mut derived_expr, TokenKind::Rparen);

            self.advance();
            let mut command = Tree::open(TreeKind::Command, self.curr_token.start);
             while self.at_any(&[TokenKind::Squote, TokenKind::True, 
                                                                            TokenKind::False, TokenKind::Number, 
                                                                            TokenKind::Character, TokenKind::String, 
                                                                            TokenKind::Variable, TokenKind::Lparen, 
                                                                            TokenKind::Bquote])
            {
                command.add_child(self.expression());
                self.advance();
            } 
            command.close(self.curr_token.end);
            derived_expr.add_child(command);
        } else if self.at(TokenKind::Delay) {
            derived_expr.add_child(Tree::leaf(TreeKind::Token, &self.curr_token));
            self.advance();
            derived_expr.add_child(self.expression());
            self.advance();
        }

        self.expect(&mut derived_expr, TokenKind::Rparen);
        derived_expr.close(self.curr_token.end);
        derived_expr
    }
    
    fn expression(&mut self) -> Tree {
        let mut expression = Tree::open(TreeKind::Expression, self.curr_token.start);
        if self.at(TokenKind::Variable) {
            expression.add_child(Tree::leaf(TreeKind::Variable, &self.curr_token));
        } else if self.at_any(&[TokenKind::Squote, TokenKind::True, TokenKind::False, TokenKind::Number, TokenKind::Character, TokenKind::String]) {
            expression.add_child(self.literal());
        } else if self.at(TokenKind::Lparen) {
            if self.ahead(TokenKind::Quote) {
                expression.add_child(self.literal());
            } else if self.ahead(TokenKind::Lambda) {
                // lambda expression
                expression.add_child(self.lambda());
            } else if self.ahead(TokenKind::If) {
                // conditional expression
                expression.add_child(self.conditional());
            } else if self.ahead(TokenKind::SetExPt) {
                // assignment expression
                expression.add_child(self.assignment());
            } else if self.ahead_any(&[TokenKind::Cond, TokenKind::Case, TokenKind::And, TokenKind::Or, TokenKind::Let, TokenKind::LetStar, TokenKind::LetRec, TokenKind::Begin, TokenKind::Do, TokenKind::Delay, TokenKind::Quasiquote]) {
                // derived expression
                expression.add_child(self.derived_expression());
            } else {
                // procedure call
                expression.add_child(self.procedure_call());
            }
        } else if self.at(TokenKind::Bquote) {
            // quasiquotation
        } else {
            expression.add_child(Tree::leaf(TreeKind::Error, &self.curr_token));
            self.add_error_msg_to_log("Expected an expression");
            self.synchronize_from_parse_error();
        }

        expression.close(self.curr_token.end);
        expression
    }

    fn command_or_definition(&mut self) -> Tree {
        let mut cod = Tree::open(TreeKind::CommandOrDefinition, self.curr_token.start);

        if self.at(TokenKind::Lparen) {
            if self.ahead(TokenKind::Define) {
                cod.add_child(self.definition(BeginTerminalSite::AtDefinition));
            } else if self.ahead(TokenKind::Define) {
                cod.add_child(self.definition(BeginTerminalSite::AtCommandOrDefinition));
            } else if self.ahead(TokenKind::DefineSyntax){
                // define-syntax
            } else {
                let mut command = Tree::open(TreeKind::Command, self.curr_token.start);
                command.add_child(self.expression());
                command.close(self.curr_token.end);
                cod.add_child(command);
            }
        } else {
            let mut command = Tree::open(TreeKind::Command, self.curr_token.start);
            command.add_child(self.expression());
            command.close(self.curr_token.end);
            cod.add_child(command);
        }

        cod.close(self.curr_token.end);
        cod
    }

    pub fn generate_parse_tree(&mut self) -> Tree {
        let start = self.curr_token.end;
        let mut program = Tree::open(TreeKind::Program, start);
        self.init_token_cursor();
        loop {
            if self.at(TokenKind::Eof) {
                break;
            }
            program.add_child(self.command_or_definition());
            self.advance();
        }
        program.close(self.curr_token.end);
        program.print(0, &self.lexer);
        program
    }
}