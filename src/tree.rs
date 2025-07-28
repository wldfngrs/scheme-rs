use std::collections::VecDeque;
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
    QuasiQuotation,
    Recipient,
    Init,
    Step,
    MacroUse,
    MacroBlock,
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

    fn node_is_keyword(
        node: &Tree,
        code_ctx: &str,
        keyword: &str
    ) -> bool {
        if node.kind == TreeKind::Keyword && code_ctx[node.start..node.end] == *keyword {
            return true
        }

        false
    }

    fn list_to_prim_ir(
        mut children: VecDeque<&Tree>,
        list_type: ListTy,
        arena: &mut IrArena<PrimIr>,
        code_ctx: &str
    ) -> Option<IrID> {
        match children.pop_front() {
            Some(element) => {
                if children.len() == 0 && list_type == ListTy::Improper {
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
        formals_node: &Tree,
        body_node: &Tree,
        ty: FormalsTy,
        arena: &mut IrArena<PrimIr>,
        code_ctx: &str
    ) -> IrID {
        let children: VecDeque<&Tree> = formals_node.children.as_ref()
                                                .unwrap().iter()
                                                .filter(|n| n.kind != TreeKind::Token)
                                                .collect();
        let mut formals: Vec<IrID> = Vec::new();
        for child in children {
            formals.push(child.to_prim_ir(arena, code_ctx));
        }

        let children: VecDeque<&Tree> = body_node.children.as_ref()
                                                .unwrap().iter()
                                                .filter(|n| n.kind != TreeKind::Token)
                                                .collect();
        let mut body: Vec<IrID> = Vec::new();
        for child in children {
            body.push(child.to_prim_ir(arena, code_ctx));
        }
        let kind = PrimIrKind::Lambda{ty, formals, body};
        let id = arena.add(PrimIr { kind }, (self.start, self.end));
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
            match children.pop_front() {
                Some(element) => {
                    let datum = element.to_prim_ir(arena,   code_ctx);
                    let test = arena.add(PrimIr { kind: PrimIrKind::Call { operator: "eqv?".to_string(), operands: vec![key, datum] }}, (element.start, element.end));
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

    fn children_without_tokens(node: &Tree) -> VecDeque<&Tree> {
        let without_tokens: VecDeque<&Tree> = node.children.as_ref().unwrap().iter().filter(|n| n.kind != TreeKind::Token).collect();
        without_tokens
    }

    pub fn to_prim_ir(&self, arena: &mut IrArena<PrimIr>, code_ctx: &str) -> IrID {
        let children: Option<VecDeque<&Tree>> = if self.children.is_some() {
            Some(self.children.unwrap().iter().filter(|n| n.kind != TreeKind::Token).collect())
        } else {
            None
        };

        match self.kind {
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
                let kind = PrimIrKind::Exprs(expressions);
                let id = arena.add(PrimIr { kind }, (self.start, self.end));
                id
            },
            TreeKind::Expression | TreeKind::Literal | TreeKind::SelfEvaluating | TreeKind::Datum | TreeKind::SimpleDatum | TreeKind::CompoundDatum | TreeKind::Operator | TreeKind::Operand | TreeKind::Test |
            TreeKind::Consequent | TreeKind::Alternate => {
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
                let mut children = children.unwrap();
                let operator = children[0].to_prim_ir(arena, code_ctx);
                let operator = arena.node_at(operator);
                let operator = code_ctx[operator.span.0..operator.span.1].to_string();
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

                return self.lambda_to_prim_ir(children[1], children[2], ty, arena, code_ctx);
            },
            TreeKind::Definition(define_type) => {
                let mut children = children.unwrap();
                if Tree::node_is_keyword(children[0], code_ctx, "begin") {
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
                let mut value: IrID = 0;
                match define_type {
                    DefineTy::Type1 => {
                        value = children[2].to_prim_ir(arena, code_ctx);
                    },
                    DefineTy::Type2 => {
                        value = self.lambda_to_prim_ir(children[2], children[3], FormalsTy::Type1, arena, code_ctx);
                    },
                    DefineTy::Type3 => {
                        value = self.lambda_to_prim_ir(children[2], children[3], FormalsTy::Type3, arena, code_ctx);
                    }
                }
                let name = arena.node_at(name);
                let name = code_ctx[name.span.0..name.span.1].to_string();
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
                let variable = arena.node_at(variable);
                let variable = code_ctx[variable.span.0..variable.span.1].to_string();
                let expression = children[2].to_prim_ir(arena, code_ctx);
                
                let kind = PrimIrKind::Set{variable, expression};
                let id = arena.add(PrimIr {kind}, (self.start, self.end));
                id
            },
            TreeKind::DerivedExpr => {
                let mut children = children.unwrap();
                let mut desugared: VecDeque<IrID> = VecDeque::new();
                let node = children.pop_front().unwrap();
                if Tree::node_is_keyword(node, code_ctx, "cond") {
                    for child in children {
                        if Tree::node_is_keyword(child, code_ctx, "else") {
                            continue;
                        }
                        desugared.push_back(child.to_prim_ir(arena, code_ctx));
                    }
                } else if Tree::node_is_keyword(node, code_ctx, "case") {
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
                        let case_children = Tree::children_without_tokens(child);
                        let sequence = case_children.pop_back().unwrap();
                        let sequence = sequence.to_prim_ir(arena, code_ctx);
                        else_clause = Tree::case_to_prim_ir(key, sequence, else_clause, children, arena, code_ctx);
                    }
                    desugared.push_back(else_clause.unwrap());
                } else if Tree::node_is_keyword(node, code_ctx, "and") {
                    
                } else if Tree::node_is_keyword(node, code_ctx, "or") {

                } else if Tree::node_is_keyword(node, code_ctx, keyword) {

                }

                let kind = PrimIrKind::Exprs(desugared);
                let id = arena.add(PrimIr { kind }, (self.start, self.end));
                id
            },
            TreeKind::CondClause(cond_ty) => {
                let children = children.unwrap();
                children.pop_front();
                let mut expressions: VecDeque<IrID> = VecDeque::new();
                let mut test: IrID;
                let mut consequent: IrID;
                let mut alternate: Option<IrID>;

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
                        let operator = arena.node_at(operator);
                        let operator = code_ctx[operator.span.0..operator.span.1].to_string();
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
        }
    }
}