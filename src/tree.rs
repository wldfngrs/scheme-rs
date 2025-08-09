use std::collections::{vec_deque, VecDeque};
use std::ops::Deref;

use crate::ir::{IrArena, PrimIr, PrimIrKind, IrID};
use crate::lexer::Token;

#[derive(PartialEq, Eq, Debug)]
pub enum FormalsTy {
    Type1,
    Type2,
    Type3
}

#[derive(PartialEq, Eq, Debug)]
pub enum LetTy {
    Type1,
    Type2,
}

#[derive(PartialEq, Eq, Debug)]
pub enum ListTy {
    Proper,
    Improper
}

#[derive(PartialEq, Eq, Debug)]
pub enum DefineTy {
    Type1,
    Type2,
    Type3
}

#[derive(PartialEq, Eq, Debug)]
pub enum CondTy {
    Type1,
    Type2,
    Type3
}

#[derive(PartialEq, Debug)]
pub enum TreeKind {
    Program,
    CommandOrDefinition,
    Token,
    Expression,
    BooleanT,
    BooleanF,
    Number,
    Variable,
    Character,
    String,
    Literal,
    Quotation,
    SelfEvaluating,
    Datum,
    SimpleDatum,
    CompoundDatum,
    List(ListTy),
    Vector,
    Abbreviation,
    AbbrevPrefix,
    Symbol,
    Keyword,
    ProcedureCall,
    Operator,
    Operand,
    LambdaExpr(FormalsTy),
    Formals,
    Body,
    Definition(DefineTy),
    DefFormals,
    Sequence,
    Command,
    Conditional,
    Test,
    Consequent,
    Alternate,
    Assignment,
    DerivedExpr,
    CondClause(CondTy),
    CaseClause,
    BindingSpec,
    IterationSpec,
    DoResult,
    //QuasiQuotation,
    Recipient,
    Init,
    Step,
    //MacroUse,
    //MacroBlock,
    Error
}

pub struct Tree {
    pub kind: TreeKind,
    pub start: usize,
    pub end: usize,
    pub children: Option<Vec<Tree>>
}

impl Tree {
    pub fn open(kind: TreeKind, start: usize) -> Tree {
        Tree {
            kind,
            start,
            end: start,
            children: Some(Vec::new())
        }
    }

    pub fn leaf(kind: TreeKind, token_at_leaf: &Token) -> Tree {
        Tree {
            kind,
            start: token_at_leaf.start,
            end: token_at_leaf.end,
            children: None
        }
    }

    pub fn close(&mut self, end: usize) {
        self.end = end;
    }

    pub fn add_child(&mut self, child: Tree) {
        // asserts for invalid parent kinds, 
        // i.e these nodes don't expect children, 
        // therefore a call on this function with any of them should result in failure
        assert!(!matches!(self.kind, TreeKind::BooleanT | TreeKind::BooleanF | TreeKind::Character | TreeKind::Number | TreeKind::String | TreeKind::Variable | TreeKind::Keyword | TreeKind::Error), "Invalid attempt to add child to barren node");

        match self.children {
            Some(ref mut vec) => {
                if child.children_count() > 0 || matches!(child.kind, TreeKind::Token | TreeKind::Character | TreeKind::Number | TreeKind::String | TreeKind::BooleanT | TreeKind::BooleanF | TreeKind::Variable | TreeKind::Keyword | TreeKind::Error) {
                    vec.push(child);
                }
            },
            None => return
        }
    }

    pub fn children_count(&self) -> usize {
        match self.children {
            Some(ref vec) => vec.len(),
            None => 0
        }
    }

    pub fn print(&self, level: usize, code: &str) {
        let indent = "  ".repeat(level);
        let children = &self.children;

        match children {
            Some(vec) => {
                println!("{indent}{:?}", self.kind);

                for child in vec{
                    child.print(level + 1, code);
                }
            },
            None => {
                let literal = &code[self.start..self.end];
                
                if matches!(self.kind, TreeKind::Keyword | TreeKind::Variable | TreeKind::BooleanT | TreeKind::BooleanF | TreeKind::Number | TreeKind::Character | TreeKind::String) {
                    println!("{indent}{:?}", self.kind);
                    println!("  {indent}{:?}", literal);
                    return
                } else if matches!(self.kind, TreeKind::Error) {
                    println!("{indent}{:?}", self.kind);
                    return
                }
                
                let literal = &code[self.start..self.end];
                println!("{indent}{:?}", literal);
            }
        }
    }

    fn is_keyword(
        &self,
        code_ctx: &str,
        keyword: &str
    ) -> bool {
        if self.kind == TreeKind::Keyword && code_ctx[self.start..self.end] == *keyword {
            return true
        }

        false
    }

    fn list_to_prim_ir(
        mut children: VecDeque<&Tree>,
        list_type: &ListTy,
        arena: &mut IrArena<PrimIr>,
        code_ctx: &str
    ) -> Option<IrID> {
        match children.pop_front() {
            Some(element) => {
                if children.len() == 0 && list_type == &ListTy::Improper {
                    return Some(element.to_prim_ir(arena, code_ctx));
                }
                
                let car = element.to_prim_ir(arena, code_ctx);
                let cdr = Tree::list_to_prim_ir(children, list_type, arena, code_ctx);
                let kind = PrimIrKind::Pair{car, cdr};
                let id = arena.add(PrimIr { kind }, (element.start, element.end));
                return Some(id)
            },
            None => {
                return None
            }
        };
    }

    fn lambda_to_prim_ir(
        &self,
        formals: Vec<IrID>,
        body: Vec<IrID>,
        ty: &FormalsTy,
        arena: &mut IrArena<PrimIr>,
    ) -> IrID {
        let formals_ty = match ty {
            &FormalsTy::Type1 => FormalsTy::Type1,
            &FormalsTy::Type2 => FormalsTy::Type2,
            &FormalsTy::Type3 => FormalsTy::Type3
        };
        
        let lambda = arena.add(PrimIr { kind: PrimIrKind::Lambda{ty: formals_ty, formals, body} }, (self.start, self.end));

        let id = arena.add(PrimIr { kind: PrimIrKind::Environment(lambda) }, (self.start, self.end));
        id
    }

    fn case_to_prim_ir(
        key: IrID,
        consequent: IrID,
        else_clause: Option<IrID>,
        mut children: VecDeque<&Tree>,
        arena: &mut IrArena<PrimIr>,
        code_ctx: &str
    ) -> Option<IrID> {
        let mut alternate = else_clause;
        loop {
            match children.pop_back() {
                Some(element) => {
                    let datum = element.to_prim_ir(arena,   code_ctx);
                    let operator = arena.add(PrimIr { kind: PrimIrKind::Symbol("eqv?".to_string()) }, (element.start, element.end));
                    let test = arena.add(PrimIr { kind: PrimIrKind::Call { operator, operands: vec![key, datum] }}, (element.start, element.end));
                    let conditional = arena.add(PrimIr { kind: PrimIrKind::Conditional { test, consequent, alternate } }, (element.start, element.end));
                    alternate = Some(conditional);
                },
                None => {
                    break
                }
            }
        }
        alternate
    }

    fn and_to_prim_ir(
        &self,
        mut children: VecDeque<&Tree>,
        arena: &mut IrArena<PrimIr>,
        code_ctx: &str
    ) -> IrID {
        if children.len() == 0 {
            return arena.add(PrimIr { kind: PrimIrKind::Boolean(true) }, (self.start, self.end));
        } else if children.len() == 1 {
            return children[0].to_prim_ir(arena, code_ctx);
        }

        let test = children.pop_front().unwrap();
        let test = test.to_prim_ir(arena, code_ctx);
        let consequent = self.and_to_prim_ir(children, arena, code_ctx);
        let alternate = Some(arena.add(PrimIr { kind: PrimIrKind::Boolean(false) }, (self.start, self.end)));
        let id = arena.add(PrimIr { kind: PrimIrKind::Conditional { test, consequent, alternate } }, (self.start, self.end));
        return id
    }

    fn or_to_prim_ir(
        &self,
        mut children: VecDeque<&Tree>,
        arena: &mut IrArena<PrimIr>,
        code_ctx: &str
    ) -> IrID {
        if children.len() == 0 {
            return arena.add(PrimIr { kind: PrimIrKind::Boolean(false) }, (self.start, self.end));
        } else if children.len() == 1 {
            return children[0].to_prim_ir(arena, code_ctx);
        }
        
        let test = children.pop_front().unwrap();
        let test = test.to_prim_ir(arena, code_ctx);
        let consequent = test;
        let alternate = Some(self.or_to_prim_ir(children, arena, code_ctx));
        let id = arena.add(PrimIr { kind: PrimIrKind::Conditional { test, consequent, alternate } }, (self.start, self.end));
        return id
    }

    fn letrec_to_prim_ir(
        &self,
        children: VecDeque<&Tree>,
        body: &mut VecDeque<IrID>,
        arena: &mut IrArena<PrimIr>,
        code_ctx: &str
    ) -> IrID {
        let mut expressions: VecDeque<IrID> = VecDeque::new();
        for binds in children {
            let bind = binds.children_without_tokens();
            let name = bind[0].to_prim_ir(arena, code_ctx);
            let value = bind[1].to_prim_ir(arena, code_ctx);
            let define_id = arena.add(PrimIr { kind: PrimIrKind::Define { name, value } }, (self.start, self.end));
            expressions.push_back(define_id);
        }

        expressions.append(body);
        let exprs_id = arena.add(PrimIr { kind: PrimIrKind::Exprs(expressions) }, (self.start, self.end));
        
        let env_id = arena.add(PrimIr { kind: PrimIrKind::Environment(exprs_id) }, (self.start, self.end));
        env_id
    }

    fn let_to_prim_ir(
        &self,
        formals: Vec<IrID>,
        operands: Vec<IrID>,
        body: Vec<IrID>,
        arena: &mut IrArena<PrimIr>,
    ) -> IrID {
        let operator = self.lambda_to_prim_ir(formals, body, &FormalsTy::Type1, arena);

        let id = arena.add(PrimIr { kind: PrimIrKind::Call { operator, operands } }, (self.start, self.end));
        id
    }

    fn let_star_to_prim_ir(
        &self,
        mut children: VecDeque<&Tree>,
        body: Vec<IrID>,
        arena: &mut IrArena<PrimIr>,
        code_ctx: &str
    ) -> IrID {
        if children.len() == 0 {
            return self.let_to_prim_ir(Vec::new(), Vec::new(), body, arena);
        }
        
        let top_bind = children.pop_front().unwrap();
        let top_bind = top_bind.children_without_tokens();

        let variable: Vec<IrID> = vec![top_bind[0].to_prim_ir(arena, code_ctx)];
        let init: Vec<IrID> = vec![top_bind[1].to_prim_ir(arena, code_ctx)];

        let let_body: Vec<IrID> = vec![self.let_star_to_prim_ir(children, body, arena, code_ctx)];
        return self.let_to_prim_ir(variable, init, let_body, arena)
    }

    fn children_without_tokens(&self) -> VecDeque<&Tree> {
        let without_tokens: VecDeque<&Tree> = self.children.as_ref().unwrap().iter().filter(|n| n.kind != TreeKind::Token).collect();
        without_tokens
    }

    pub fn to_prim_ir(&self, arena: &mut IrArena<PrimIr>, code_ctx: &str) -> IrID {
        let children: Option<VecDeque<&Tree>> = if self.children.is_some() {
            Some(self.children_without_tokens())
        } else {
            None
        };

        match &self.kind {
            TreeKind::Program => {
                let children = children.unwrap();
                let mut expressions: VecDeque<IrID> = VecDeque::new();
                for child in children {
                    expressions.push_back(child.to_prim_ir(arena, code_ctx));
                }
                let kind = PrimIrKind::Exprs(expressions);
                let id = arena.add(PrimIr { kind }, (self.start, self.end));
                id
            },
            TreeKind::CommandOrDefinition => {
                let mut children = children.unwrap();
                if children.len() == 1 {
                    return children[0].to_prim_ir(arena, code_ctx);
                }
                // pop begin keyword node
                children.pop_front();
                let mut expressions: VecDeque<IrID> = VecDeque::new();
                for child in children {
                    expressions.push_back(child.to_prim_ir(arena, code_ctx));
                }
                let exprs_id = arena.add(PrimIr { kind: PrimIrKind::Exprs(expressions) }, (self.start, self.end));

                let id = arena.add(PrimIr { kind: PrimIrKind::Environment(exprs_id) }, (self.start, self.end));
                id
            },
            TreeKind::Expression | TreeKind::Literal | TreeKind::SelfEvaluating | TreeKind::Datum | TreeKind::SimpleDatum | TreeKind::CompoundDatum | TreeKind::Operator | TreeKind::Operand | TreeKind::Test |
            TreeKind::Consequent | TreeKind::Alternate | TreeKind::DoResult | TreeKind::Step | TreeKind::Recipient | TreeKind::Init => {
                // number of children is always 1
                let children = children.unwrap();
                return children[0].to_prim_ir(arena, code_ctx);
            },
            TreeKind::BooleanT => {
                let kind = PrimIrKind::Boolean(true);
                let id = arena.add(PrimIr { kind }, (self.start, self.end));
                id
            },
            TreeKind::BooleanF => {
                let kind = PrimIrKind::Boolean(false);
                let id = arena.add(PrimIr { kind }, (self.start, self.end));
                id
            },
            TreeKind::Number => {
                // TODO this
                0
            },
            TreeKind::Variable | TreeKind::Keyword => {
                let kind = PrimIrKind::Symbol(code_ctx[self.start..self.end].to_string());
                let id = arena.add(PrimIr { kind }, (self.start, self.end));
                id
            },
            TreeKind::Character => {
                let kind = PrimIrKind::Char(code_ctx[self.start..self.end].to_string());
                let id = arena.add(PrimIr { kind }, (self.start, self.end));
                id
            },
            TreeKind::String => {
                let kind = PrimIrKind::String(code_ctx[self.start..self.end].to_string());
                let id = arena.add(PrimIr { kind }, (self.start, self.end));
                id
            },
            TreeKind::Quotation => {
                let children = children.unwrap();
                if children.len() == 1 {
                    return children[0].to_prim_ir(arena, code_ctx);
                }

                return children[1].to_prim_ir(arena, code_ctx);
            },
            TreeKind::List(list_type) => {
                let children = children.unwrap();
                 
                match Tree::list_to_prim_ir(children, list_type, arena, code_ctx) {
                    Some(ir_id) => return ir_id,
                    None => {
                        let id = arena.add(PrimIr { kind: PrimIrKind::Error}, (self.start, self.end));
                        id
                    }
                }
            },
            TreeKind::Vector => {
                let children = children.unwrap();
                let mut datums: Vec<IrID> = Vec::new();
                for child in children {
                    datums.push(child.to_prim_ir(arena, code_ctx));
                }
                let kind = PrimIrKind::Vector(datums);
                let id = arena.add(PrimIr { kind }, (self.start, self.end));
                id
            },
            TreeKind::Abbreviation => {
                let children = children.unwrap();
                // index 0 is abbrev prefix
                return children[1].to_prim_ir(arena, code_ctx)
            },
            TreeKind::Symbol => {
                let children = children.unwrap();
                return children[0].to_prim_ir(arena, code_ctx);
            },
            TreeKind::ProcedureCall => {
                let children = children.unwrap();
                let operator = children[0].to_prim_ir(arena, code_ctx);
                //let operator = arena.node_at(operator);
                //let operator = code_ctx[operator.span.0..operator.span.1].to_string();
                let mut operands: Vec<IrID> = Vec::new();
                for child in children {
                    operands.push(child.to_prim_ir(arena, code_ctx));
                }
                let kind = PrimIrKind::Call{operator, operands};
                let id = arena.add(PrimIr {kind}, (self.start, self.end));
                id
            },
            TreeKind::LambdaExpr(formals_type) => {
                let children = children.unwrap();
                let ty = formals_type;
                let formals_children = children[1].children_without_tokens();
                let body_children = children[2].children_without_tokens();

                let mut formals: Vec<IrID> = Vec::new();
                let mut body: Vec<IrID> = Vec::new();

                for child in formals_children {
                    formals.push(child.to_prim_ir(arena, code_ctx));
                }

                for child in body_children {
                    body.push(child.to_prim_ir(arena, code_ctx));
                } 

                return self.lambda_to_prim_ir(formals, body, ty, arena);
            },
            TreeKind::Definition(define_type) => {
                let mut children = children.unwrap();
                if children[0].is_keyword(code_ctx, "begin") {
                    children.pop_front();
                    let mut expressions: VecDeque<IrID> = VecDeque::new();
                    for child in children {
                        expressions.push_back(child.to_prim_ir(arena, code_ctx));
                    }
                    let kind = PrimIrKind::Exprs(expressions);
                    let id = arena.add(PrimIr { kind }, (self.start, self.end));
                    return id
                }
                
                let name = children[1].to_prim_ir(arena, code_ctx);
                let value: IrID;
                match define_type {
                    DefineTy::Type1 => {
                        value = children[2].to_prim_ir(arena, code_ctx);
                    },
                    DefineTy::Type2 => {
                        let formals_children = children[2].children_without_tokens();
                        let body_children = children[3].children_without_tokens();

                        let mut formals: Vec<IrID> = Vec::new();
                        let mut body: Vec<IrID> = Vec::new();

                        for child in formals_children {
                            formals.push(child.to_prim_ir(arena, code_ctx));
                        }

                        for child in body_children {
                            body.push(child.to_prim_ir(arena, code_ctx));
                        }
                        
                        value = self.lambda_to_prim_ir(formals, body, &FormalsTy::Type1, arena);
                    },
                    DefineTy::Type3 => {
                        let formals_children = children[2].children_without_tokens();
                        let body_children = children[3].children_without_tokens();

                        let mut formals: Vec<IrID> = Vec::new();
                        let mut body: Vec<IrID> = Vec::new();

                        for child in formals_children {
                            formals.push(child.to_prim_ir(arena, code_ctx));
                        }

                        for child in body_children {
                            body.push(child.to_prim_ir(arena, code_ctx));
                        }

                        value = self.lambda_to_prim_ir(formals, body, &FormalsTy::Type3, arena);
                    }
                }

                let kind = PrimIrKind::Define {name, value};
                let id = arena.add(PrimIr { kind }, (self.start, self.end));
                id
            },
            TreeKind::Sequence => {
                let children = children.unwrap();
                let mut expressions: VecDeque<IrID> = VecDeque::new();
                for child in children {
                    expressions.push_back(child.to_prim_ir(arena, code_ctx));
                }
                let kind = PrimIrKind::Exprs(expressions);
                let id = arena.add(PrimIr { kind }, (self.start, self.end));
                id
            },
            TreeKind::Conditional => {
                let children = children.unwrap();
                let test = children[1].to_prim_ir(arena, code_ctx);
                let consequent = children[2].to_prim_ir(arena, code_ctx);
                let mut alternate: Option<IrID> = None;
                if children.len() > 3 {
                    alternate = Some(children[3].to_prim_ir(arena, code_ctx));
                }

                let kind = PrimIrKind::Conditional {test, consequent, alternate};
                let id = arena.add(PrimIr { kind }, (self.start, self.end));
                id
            },
            TreeKind::Assignment => {
                let children = children.unwrap();
                let variable = children[1].to_prim_ir(arena, code_ctx);
                let expression = children[2].to_prim_ir(arena, code_ctx);
                
                let kind = PrimIrKind::Set{variable, expression};
                let id = arena.add(PrimIr {kind}, (self.start, self.end));
                id
            },
            TreeKind::DerivedExpr => {
                let mut children = children.unwrap();
                let mut desugared: VecDeque<IrID> = VecDeque::new();
                let node = children.pop_front().unwrap();
                if node.is_keyword(code_ctx, "cond") {
                    for child in children {
                        if child.is_keyword(code_ctx, "else") {
                            continue;
                        }
                        desugared.push_back(child.to_prim_ir(arena, code_ctx));
                    }
                } else if node.is_keyword(code_ctx, "case") {
                    let key = children.pop_front().unwrap();
                    let key = key.to_prim_ir(arena, code_ctx);
                    let count = self.children_count();
                    let mut else_clause: Option<IrID> = if children[count - 1].kind == TreeKind::Sequence {
                        let sequence = children.pop_back().unwrap();
                        children.pop_back();
                        Some(sequence.to_prim_ir(arena, code_ctx))
                    } else {
                        None
                    };

                    for child in children.iter().rev() {
                        let mut case_children = child.children_without_tokens();
                        let sequence = case_children.pop_back().unwrap();
                        let sequence = sequence.to_prim_ir(arena, code_ctx);
                        else_clause = Tree::case_to_prim_ir(key, sequence, else_clause, children.clone(), arena, code_ctx);
                    }
                    desugared.push_back(else_clause.unwrap());
                } else if node.is_keyword(code_ctx, "and") {
                    desugared.push_back(self.and_to_prim_ir(children, arena, code_ctx));
                } else if node.is_keyword(code_ctx, "or") {
                    desugared.push_back(self.or_to_prim_ir(children, arena, code_ctx));
                } else if node.is_keyword(code_ctx, "let") {
                    let body_node = children.pop_back().unwrap();
                    let mut body: Vec<IrID> = Vec::new();
                    
                    for child in body_node.children_without_tokens() {
                        body.push(child.to_prim_ir(arena, code_ctx));
                    }

                    if children[0].kind == TreeKind::Variable {
                        // return let-rec
                        let tag = children.pop_front().unwrap();
                        let tag = tag.to_prim_ir(arena, code_ctx);
                        let mut formals: Vec<IrID> = Vec::new();
                        let mut operands: Vec<IrID> = Vec::new();

                        for child in children {
                            let binds = child.children_without_tokens();
                            formals.push(binds[0].to_prim_ir(arena, code_ctx));
                            operands.push(binds[1].to_prim_ir(arena, code_ctx));
                        }

                        let mut expressions: VecDeque<IrID> = VecDeque::new();
                        
                        let lambda = self.lambda_to_prim_ir(formals, body, &FormalsTy::Type1, arena);
                        let define_id = arena.add(PrimIr { kind: PrimIrKind::Define { name: tag, value: lambda } }, (self.start, self.end));
                        
                        expressions.push_back(define_id);
                        let call_id = arena.add(PrimIr { kind: PrimIrKind::Call { operator: tag, operands } }, (self.start, self.end));
                        expressions.push_back(call_id);

                        let exprs_id = arena.add(PrimIr { kind: PrimIrKind::Exprs(expressions) }, (self.start, self.end));
                        
                        let env_id = arena.add(PrimIr { kind: PrimIrKind::Environment(exprs_id) }, (self.start, self.end));

                        desugared.push_back(env_id);
                    } else {
                        let mut variables: Vec<IrID> = Vec::new();
                        let mut inits: Vec<IrID> = Vec::new();
                    
                        for binds in children {
                            let bind = binds.children_without_tokens();
                            variables.push(bind[0].to_prim_ir(arena, code_ctx));
                            inits.push(bind[1].to_prim_ir(arena, code_ctx));
                        }
                    
                        desugared.push_back(self.let_to_prim_ir(variables, inits, body, arena));
                    }
                } else if node.is_keyword(code_ctx, "let*") {
                    let body_node = children.pop_back().unwrap();
                    let mut body: Vec<IrID> = Vec::new();

                    for child in body_node.children_without_tokens() {
                        body.push(child.to_prim_ir(arena, code_ctx));
                    }

                    desugared.push_back(self.let_star_to_prim_ir(children, body, arena, code_ctx));
                } else if node.is_keyword(code_ctx, "letrec") {
                    let body_node = children.pop_back().unwrap();
                    let mut body: VecDeque<IrID> = VecDeque::new();

                    for child in body_node.children_without_tokens() {
                        body.push_back(child.to_prim_ir(arena, code_ctx));
                    }

                    desugared.push_back(self.letrec_to_prim_ir(children, &mut body, arena, code_ctx));
                } else if node.is_keyword(code_ctx, "begin") {
                    desugared.push_back(children[0].to_prim_ir(arena, code_ctx));
                } else if node.is_keyword(code_ctx, "do") {
                    let mut body: Vec<IrID> = Vec::new();
                    let mut inits: Vec<IrID> = Vec::new();
                    let mut alternate: VecDeque<IrID> = VecDeque::new();
                    
                    while children.back().unwrap().kind == TreeKind::Command {
                        let expr = children.pop_back().unwrap();
                        alternate.push_back(expr.to_prim_ir(arena, code_ctx));
                    }

                    let mut consequent: VecDeque<IrID> = VecDeque::new();
                    if children.back().unwrap().kind == TreeKind::DoResult {
                        let sequence = children.pop_back().unwrap().children_without_tokens();
                        consequent.push_back(sequence[0].to_prim_ir(arena, code_ctx));
                    }

                    let consequent = arena.add(PrimIr { kind: PrimIrKind::Exprs(consequent) }, (self.start, self.end));

                    let test = children.pop_back().unwrap();
                    let test = test.to_prim_ir(arena, code_ctx);

                    while children.back().unwrap().kind == TreeKind::IterationSpec {
                        let mut iters = children.pop_back().unwrap().children_without_tokens();
                        
                        if iters.back().unwrap().kind == TreeKind::Step {
                            let step = iters.pop_back().unwrap();
                            alternate.push_back(step.to_prim_ir(arena, code_ctx));
                        }

                        let value = iters.pop_back().unwrap();
                        let value = value.to_prim_ir(arena, code_ctx);

                        let name = iters.pop_back().unwrap();
                        let name = name.to_prim_ir(arena, code_ctx);

                        let define = arena.add(PrimIr { kind: PrimIrKind::Define { name, value } }, (self.start, self.end));
                        inits.push(define);
                    }

                    let alternate = arena.add(PrimIr { kind: PrimIrKind::Exprs(alternate) }, (self.start, self.end));

                    let condition = arena.add(PrimIr { kind: PrimIrKind::Conditional { test, consequent, alternate: Some(alternate) } }, (self.start, self.end));
                    body.push(condition);
                    
                    let loop_id = arena.add(PrimIr { kind: PrimIrKind::Loop { inits, body } }, (self.start, self.end));
                    let env_id = arena.add(PrimIr { kind: PrimIrKind::Environment(loop_id) }, (self.start, self.end));
                    desugared.push_back(env_id);
                }

                let kind = PrimIrKind::Exprs(desugared);
                let id = arena.add(PrimIr { kind }, (self.start, self.end));
                id
            },
            TreeKind::CondClause(cond_ty) => {
                let mut children = children.unwrap();
                children.pop_front();
                let mut expressions: VecDeque<IrID> = VecDeque::new();
                let test: IrID;
                let consequent: IrID;
                let alternate: Option<IrID>;

                match cond_ty {
                    CondTy::Type1 => {
                        test = children[0].to_prim_ir(arena, code_ctx);
                        consequent = children[1].to_prim_ir(arena, code_ctx);
                        alternate = None;
                    },
                    CondTy::Type2 => {
                        test = children[0].to_prim_ir(arena, code_ctx);
                        consequent = test;
                        alternate = None;
                    },
                    CondTy::Type3 => {
                        test = children[0].to_prim_ir(arena, code_ctx);
                        let operator = children[1].to_prim_ir(arena, code_ctx);
                        //let operator = arena.node_at(operator);
                        //let operator = code_ctx[operator.span.0..operator.span.1].to_string();
                        let operands: Vec<IrID> = vec![test];
                        let kind = PrimIrKind::Call { operator, operands };
                        let id = arena.add(PrimIr { kind }, (self.start, self.end));
                        consequent = id;
                        alternate = None;
                    }
                }

                let kind = PrimIrKind::Conditional {test, consequent, alternate};
                let id = arena.add(PrimIr { kind }, (self.start, self.end));
                expressions.push_back(id);
                let kind = PrimIrKind::Exprs(expressions);
                let id = arena.add(PrimIr { kind }, (self.start, self.end));
                id
            },
            _ => unreachable!("Invalid TreeKind passed to to_prim_ir() function")
        }
    }
}