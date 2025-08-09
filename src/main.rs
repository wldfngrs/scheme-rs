mod tests;
mod compiler;
mod parser;
mod tree;
mod lexer;
mod ir;

use std::env;

use crate::compiler::Compiler;

fn main() {
    let args: Vec<String> = env::args().collect();
    let scheme_rs = Compiler::new(args);
    scheme_rs.start();
}
