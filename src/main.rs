#![feature(macro_metavar_expr)]
#![feature(macro_metavar_expr_concat)]

use std::fs::OpenOptions;

use instructions::Instruction;

pub mod addressing;
pub mod instructions;

fn main() {
    println!("Hello, world!");

    let mut file = OpenOptions::new().read(true).open("test.bin").unwrap();
    let mut output = OpenOptions::new()
        .create(true)
        .write(true)
        .open("write.bin")
        .unwrap();

    loop {
        let instr = Instruction::read(&mut file);
        let mut bytes = Vec::with_capacity(4);
        instr.write(&mut bytes);
        println!("{:02x?}: {:02x?}", bytes, instr);

        instr.write(&mut output);
    }
}
