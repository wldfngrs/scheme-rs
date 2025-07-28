use std::{collections::VecDeque, str::Chars};

use crate::tree::FormalsTy;

enum NumKind {
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

pub type IrID = usize;

pub enum PrimIrKind {
    Exprs(VecDeque<IrID>),
    Boolean(bool),
    Symbol(String),
    Char(String),
    Vector(Vec<IrID>),
    Call {
        operator: String,
        operands: Vec<IrID>
    },
    Lambda {
        ty: FormalsTy,
        formals: Vec<IrID>,
        body: Vec<IrID>
    },
    Error,
    Pair {
        car: IrID, 
        cdr: Option<IrID>
    },
    Number(NumKind),
    String(String),
    Define {
        name: String,
        value: IrID,
    },
    Set {
        variable: String,
        expression: IrID
    },
    Conditional {
        test: IrID, 
        consequent: IrID, 
        alternate: Option<IrID>
    }
    //Port
}

pub struct PrimIr {
    pub kind: PrimIrKind
}

/*struct LambdaIr {
    kind: LambdaIrKind
}*/

pub struct IrNode<T> {
    pub kind: T,
    pub span: (usize, usize)
}

pub struct IrArena<T> {
    nodes: Vec<IrNode<T>>
}

impl<T> IrArena<T> {
    pub fn new() -> IrArena<T> {
        IrArena { nodes: Vec::new() }
    }

    pub fn add(&mut self, kind: T, span: (usize, usize)) -> IrID {
        let i = self.nodes.len();
        self.nodes.push(IrNode { kind, span });
        i
    }

    pub fn node_at(&self, id: IrID) -> &IrNode<T> {
        return &self.nodes[id];
    }
}

pub struct IrCtx<'code> {
    code: &'code str
}

impl<'code> IrCtx<'code> {
    pub fn init(code: &'code str) -> IrCtx<'code> {
        IrCtx { 
            code: code,
        }
    }

    /*pub fn lambda_ir_from<T>(&self, prim_ir: IrArena<PrimIr>, start_id: IrID, lambda_ir: &mut IrArena) -> () {

    }*/
}