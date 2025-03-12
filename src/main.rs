#![feature(macro_metavar_expr)]
#![feature(macro_metavar_expr_concat)]

use std::fs::OpenOptions;

use instructions::{Instruction, Operation};

pub mod addressing;
pub mod instructions;

fn main() {
    let mut file = OpenOptions::new().read(true).open("test.bin").unwrap();

    loop {
        let instr = Operation::from(Instruction::read(&mut file));
        println!("{:02x?}", instr);
    }
}
