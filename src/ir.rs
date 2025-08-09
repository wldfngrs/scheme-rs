use std::{collections::{HashMap, VecDeque}};

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
    //Undefined,
    Environment(IrID),
    Exprs(VecDeque<IrID>),
    Boolean(bool),
    Symbol(String),
    Char(String),
    Vector(Vec<IrID>),
    Call {
        operator: IrID,
        operands: Vec<IrID>
    },
    Procedure {
        //closure: Env,
        params: Vec<String>,
        body: Vec<IrID>,
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
        name: IrID,
        value: IrID,
    },
    Set {
        variable: IrID,
        expression: IrID
    },
    Conditional {
        test: IrID, 
        consequent: IrID, 
        alternate: Option<IrID>
    },
    Loop {
        inits: Vec<IrID>,
        body: Vec<IrID>
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
        // use a hash table for the arena, if an IrNode already exists in the hash table,
        // it is in the arena, and the stored id should be returned, else add to the hash table,
        // and arena and return the ids
        let i = self.nodes.len();
        self.nodes.push(IrNode { kind, span });
        i
    }
}