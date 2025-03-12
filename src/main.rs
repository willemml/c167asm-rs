#![feature(macro_metavar_expr)]
#![feature(macro_metavar_expr_concat)]

use std::fs::OpenOptions;

use instructions::Instruction;

pub mod addressing;
pub mod instructions;

fn main() {
    println!("Hello, world!");

    let mut file = OpenOptions::new().read(true).open("test.bin").unwrap();

    loop {
        println!("{:02x?}", Instruction::read(&mut file));
    }
}
