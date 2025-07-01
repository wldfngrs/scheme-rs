mod tests;
mod lexer;
mod parser;
mod compiler;

use std::env;

use crate::compiler::Compiler;

fn main() {
    let args: Vec<String> = env::args().collect();
    let scheme_rs = Compiler::new(args);
    scheme_rs.start();
}
