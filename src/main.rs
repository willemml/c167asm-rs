#![feature(macro_metavar_expr)]
#![feature(macro_metavar_expr_concat)]
#![feature(bigint_helper_methods)]

use std::{fs::OpenOptions, io::Read};

use interpreter::Interpreter;
use thiserror::Error;

pub const DPP0: usize = 0x0204;
pub const DPP1: usize = 0x0205;
pub const DPP2: usize = 0x00E0;
pub const DPP3: usize = 0x0003;

pub mod addressing;
pub mod instructions;
pub mod interpreter;
pub mod registers;

#[derive(Debug, Error)]
pub enum Error {
    #[error("byte is not a valid opcode")]
    InvalidOpCode,
    #[error("not enough data to read instruction parameters")]
    MissingArgs,
    #[error("instruction does support those arguments")]
    BadArgs,
    #[error("not a readable data address type")]
    NotReadable,
    #[error("not a writeable data address type")]
    NotWriteable,
    #[error("not a known SFR")]
    InvalidSFR,
    #[error("not a known ESFR")]
    InvalidESFR,
    #[error("io error")]
    Io(#[from] std::io::Error),
}

pub(crate) type Result<T = (), E = Error> = std::result::Result<T, E>;

fn main() {
    let mut irom = OpenOptions::new().read(true).open("irom.bin").unwrap();
    let mut flash = OpenOptions::new().read(true).open("flash.bin").unwrap();

    let mut interpreter = Interpreter::new();

    irom.read(&mut interpreter.memory).unwrap();
    flash.read(&mut interpreter.memory[0x80000..]).unwrap();

    registers::init_sfr(&mut interpreter.memory);
    registers::init_esfr(&mut interpreter.memory);

    interpreter.execute().unwrap();
}
