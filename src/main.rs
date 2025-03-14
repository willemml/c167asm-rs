#![feature(macro_metavar_expr)]
#![feature(macro_metavar_expr_concat)]

use std::{fs::OpenOptions, io::Read, num::Wrapping};

use addressing::Address;
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
    #[error("not a readable data address type")]
    NotReadable,
    #[error("not a writeable data address type")]
    NotWriteable,
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

    interpreter.execute().unwrap();
}

pub struct Interpreter {
    memory: Vec<u8>,
    instruction_pointer: u16,
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
            let ip = self.get_ip();
            let instr = {
                let mut slice = &self.memory[ip..ip + 4];
                Instruction::read(&mut slice)?
            };
            let op = Operation::from(instr);

            println!("{:04x}: {:02x?}", self.instruction_pointer, op);
            match op {
                Operation::Add(a, b) => println!(
                    "  a: 0x{:04x}, b: 0x{:04x}",
                    self.read_address::<1>(a)?,
                    self.read_address::<1>(b)?
                ),
                Operation::SubCB(a, b) => println!(
                    "  a: 0x{:04x}, b: 0x{:04x}",
                    self.read_address::<1>(a)?,
                    self.read_address::<1>(b)?
                ),
                Operation::OrB(a, b) => println!(
                    "  a: 0x{:04x}, b: 0x{:04x}",
                    self.read_address::<1>(a)?,
                    self.read_address::<1>(b)?
                ),
                Operation::Mov(a, b) => {
                    let w = self.read_address::<1>(b)?;
                    self.write_address::<1>(a, w)?
                }
                _ => {}
            }

            self.instruction_pointer += instr.len() as u16;
        }
    }

    pub fn read_byte(&self, address: usize) -> u8 {
        self.memory[address]
    }

    pub fn write_word(&mut self, address: usize, word: u16) {
        let [b1, b2] = word.to_le_bytes();
        self.memory[address] = b1;
        self.memory[address + 1] = b2;
    }

    pub fn write_byte(&mut self, address: usize, byte: u8) {
        self.memory[address] = byte;
    }

    pub fn read_word(&self, address: usize) -> u16 {
        u16::from_le_bytes([self.memory[address], self.memory[address + 1]])
    }

    pub fn read_gpr_word(&self, gpr: u8) -> u16 {
        self.read_word(self.gpr_address(gpr))
    }

    pub fn gpr_address(&self, gpr: u8) -> usize {
        registers::CP::ADDRESS + ((gpr as usize & 0xF) * 2)
    }

    pub fn read_gpr_byte(&self, gpr: u8) -> u8 {
        self.memory[self.read_sfr_word::<registers::CP>() as usize + (gpr as usize & 0xF)]
    }

    pub fn get_ip(&self) -> usize {
        self.instruction_pointer as usize
            | ((self.read_sfr_word::<registers::CSP>() as usize) << 16)
    }

    pub fn read_sfr_word<S: SpecialFunctionRegister>(&self) -> u16 {
        self.read_word(S::ADDRESS)
    }

    pub fn read_sfr_byte<S: SpecialFunctionRegister>(&self) -> u8 {
        self.memory[S::ADDRESS]
    }

    pub fn read_dpp_addressed_word(&self, address: u16) -> u16 {
        self.read_word(self.get_dpp_address(address))
    }

    pub fn get_dpp_address(&self, address: u16) -> usize {
        let dpp = match address >> 14 {
            0 => self.read_sfr_word::<registers::DPP0>(),
            1 => self.read_sfr_word::<registers::DPP1>(),
            2 => self.read_sfr_word::<registers::DPP2>(),
            3 => self.read_sfr_word::<registers::DPP3>(),
            _ => panic!(),
        } as usize;
        let addr = (address & 0x3FFF) as usize;

        addr | (dpp << 14)
    }

    pub fn write_dpp_addressed_word(&mut self, address: u16, word: u16) {
        self.write_word(self.get_dpp_address(address), word);
    }

    pub fn write_gpr_word(&mut self, gpr: u8, word: u16) {
        let addr = self.gpr_address(gpr);
        self.write_word(addr, word);
    }
    pub fn write_gpr_byte(&mut self, gpr: u8, byte: u8) {
        let addr = self.gpr_address(gpr);
        self.write_byte(addr, byte);
    }

    // TODO: allow for EXT addressing overrides
    // TODO: split into separate functions for bytewise and wordwise addresses
    pub fn read_address<const N: u16>(&mut self, address: Address) -> Result<u16, Error> {
        Ok(match address {
            Address::GPR(a) => self.read_gpr_word(a),
            Address::Indirect(a) => self.read_dpp_addressed_word(self.read_gpr_word(a)),
            Address::IndirectIncr(a) => {
                let gpr = self.read_gpr_word(a);

                // TODO: not sure if this is correct...
                self.write_gpr_word(a, (Wrapping(gpr) + Wrapping(N)).0);

                self.read_dpp_addressed_word(gpr)
            }
            Address::IndirectDecr(a) => {
                let gpr = self.read_gpr_word(a);

                // TODO: not sure if this is correct...
                self.write_gpr_word(a, (Wrapping(gpr) - Wrapping(N)).0);

                self.read_dpp_addressed_word(gpr)
            }
            Address::Data16(a) => a,
            Address::Data8(a) => a as u16,
            Address::Data4(a) => a as u16,
            Address::Data3(a) => a as u16,
            Address::Mem(a) => self.read_dpp_addressed_word(a),
            Address::Reg(a) => {
                if 0xF0 & a == 0xF0 {
                    self.read_gpr_word(a)
                } else {
                    // TODO: fix translation when invalid SFR
                    // self.read_word(registers::addr_from_byte(a))
                    0
                }
            }
            Address::Special(s) => {
                let mode = s >> 3;

                if mode == 0 {
                    s as u16
                } else {
                    let gpr = s & 0b11;
                    let v = self.read_gpr_word(gpr);
                    if (s >> 2) & 0b01 == 1 {
                        self.write_gpr_word(gpr, v + N);
                    }
                    v
                }
            }
            Address::Bitaddr(addr, bit) => (self.read_word(addr as usize) >> bit) & 1,
            Address::Indirect16(a, c) => self.read_dpp_addressed_word(self.read_gpr_word(a) + c),
            _ => return Err(Error::NotReadable),
        })
    }

    // TODO: allow for EXT addressing overrides
    // TODO: split into separate functions for bytewise and wordwise addresses
    pub fn write_address<const N: u16>(&mut self, address: Address, val: u16) -> Result {
        match address {
            Address::GPR(a) => self.write_gpr_word(a, val),
            Address::Indirect(a) => self.write_dpp_addressed_word(self.read_gpr_word(a), val),
            Address::IndirectIncr(a) => {
                let gpr = self.read_gpr_word(a);

                // TODO: not sure if this is correct...
                self.write_gpr_word(a, (Wrapping(gpr) + Wrapping(N)).0);

                self.write_dpp_addressed_word(gpr, val)
            }
            Address::IndirectDecr(a) => {
                let gpr = self.read_gpr_word(a);

                // TODO: not sure if this is correct...
                self.write_gpr_word(a, (Wrapping(gpr) - Wrapping(N)).0);

                self.write_dpp_addressed_word(gpr, val)
            }
            Address::Mem(a) => self.write_dpp_addressed_word(a, val),
            Address::Reg(a) => {
                if 0xF0 & a == 0xF0 {
                    self.write_gpr_word(a, val)
                } else {
                    self.write_word(registers::addr_from_byte(a), val)
                }
            }
            Address::Bitaddr(addr, bit) => {
                let word = self.read_word(addr as usize) & (0xFFFF ^ (1 << bit));

                self.write_word(addr as usize, word | ((val & 1) << bit));
            }
            Address::Indirect16(a, c) => {
                self.write_dpp_addressed_word(self.read_gpr_word(a) + c, val);
            }
            _ => return Err(Error::NotWriteable),
        }
        Ok(())
    }
}
