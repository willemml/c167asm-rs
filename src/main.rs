#![feature(macro_metavar_expr)]
#![feature(macro_metavar_expr_concat)]

use std::fs::OpenOptions;

use instructions::{Instruction, Operation};

pub mod addressing;
pub mod instructions;

fn main() {
    let mut file = OpenOptions::new().read(true).open("test.bin").unwrap();

    let mut write = OpenOptions::new()
        .write(true)
        .append(false)
        .create(true)
        .open("write.bin")
        .unwrap();

    loop {
        let op = Operation::from(Instruction::read(&mut file));

        println!("{:02x?}", op);

        let instr = Instruction::from(op);
        println!("{:02x?}", instr);
        instr.write(&mut write);
    }
}
