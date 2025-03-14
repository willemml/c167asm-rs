#![feature(macro_metavar_expr)]
#![feature(macro_metavar_expr_concat)]

use std::io::{BufReader, Write};
use std::{fs::OpenOptions, io::Read};

use instructions::{Instruction, Operation};
use registers::SpecialFunctionRegister;
use thiserror::Error;

pub const DPP0: usize = 0x0204;
pub const DPP1: usize = 0x0205;
pub const DPP2: usize = 0x00E0;
pub const DPP3: usize = 0x0003;

pub mod addressing;
pub mod instructions;
pub mod registers;

#[derive(Debug, Error)]
pub enum Error {
    #[error("byte is not a valid opcode")]
    InvalidOpCode,
    #[error("not enough data to read instruction parameters")]
    MissingArgs,
    #[error("instruction does support those arguments")]
    BadArgs,
    #[error("io error")]
    Io(#[from] std::io::Error),
}

pub(crate) type Result<T = (), E = Error> = std::result::Result<T, E>;

fn main() {
    let mut file = OpenOptions::new().read(true).open("irom.bin").unwrap();

    let mut write = Vec::with_capacity(0x7FFF);

    let mut interpreter = Interpreter::new();
    let mut bin = Vec::with_capacity(1_000_000);

    file.read(&mut bin).unwrap();

    (&mut interpreter.memory[0x80000..]).write(&bin).unwrap();

    loop {
        // use this to get offsets/addresses file.stream_position();
        let op = Operation::from(Instruction::read(&mut file).unwrap());

        println!("{:04x}: {:02x?}", write.len(), op);

        let instr = Instruction::try_from(op).unwrap();
        instr.write(&mut write).unwrap();
    }
}

pub struct Interpreter {
    memory: Vec<u8>,
    instruction_pointer: usize,
}

impl Interpreter {
    fn new() -> Self {
        Self {
            memory: vec![0u8; 16_000_000],
            instruction_pointer: 0,
        }
    }

    pub fn execute(&mut self) -> Result {
        loop {
            let mut slice = &self.memory[self.instruction_pointer..self.instruction_pointer + 4];
            let instr = Instruction::read(&mut slice)?;
            self.instruction_pointer += instr.len();
        }
    }

    pub fn read_word(&self, address: usize) -> u16 {
        u16::from_le_bytes(self.memory[address..address + 1].try_into().unwrap())
    }

    pub fn read_gpr_word(&self, gpr: u8) -> u16 {
        self.read_word(registers::CP::ADDRESS + ((gpr as usize & 0xF) * 2))
    }

    pub fn read_gpr_byte(&self, gpr: u8) -> u8 {
        self.memory[registers::CP::ADDRESS + (gpr as usize & 0xF)]
    }

    pub fn read_sfr_word<S: SpecialFunctionRegister>(&self) -> u16 {
        self.read_word(S::ADDRESS)
    }

    pub fn read_sfr_byte<S: SpecialFunctionRegister>(&self) -> u8 {
        self.memory[S::ADDRESS]
    }
}
