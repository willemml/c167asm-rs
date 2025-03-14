use std::num::Wrapping;

use crate::Error;
use crate::Result;
use crate::addressing::Address;
use crate::instructions::*;
use crate::registers;
use crate::registers::SpecialFunctionRegister;

macro_rules! mk_write_fn {
    ($type:ident, $t:ty) => {
        // TODO: allow for EXT addressing overrides
        // TODO: split into separate functions for bytewise and wordwise addresses
        pub fn ${concat(write_address_,$type)}<const N: u16>(&mut self, address: Address, val: $t) -> Result {
            match address {
                Address::GPR(a) => self.${concat(write_gpr_, $type)}(a, val),
                Address::Indirect(a) => self.${concat(write_dpp_addressed_, $type)}(self.read_gpr_word(a), val),
                Address::IndirectIncr(a) => {
                    let gpr = self.read_gpr_word(a);

                    // TODO: not sure if this is correct...
                    self.write_gpr_word(a, (Wrapping(gpr) + Wrapping(N)).0);

                    self.${concat(write_dpp_addressed_, $type)}(gpr, val)
                }
                Address::IndirectDecr(a) => {
                    let gpr = self.read_gpr_word(a);

                    // TODO: not sure if this is correct...
                    self.write_gpr_word(a, (Wrapping(gpr) - Wrapping(N)).0);

                    self.${concat(write_dpp_addressed_, $type)}(gpr, val)
                }
                Address::Mem(a) => self.${concat(write_dpp_addressed_, $type)}(a, val),
                Address::Reg(a) => {
                    if 0xF0 & a == 0xF0 {
                        self.${concat(write_gpr_, $type)}(a, val)
                    } else {
                        // TODO: handle ESFR
                        self.${concat(write_, $type)}(registers::sfr_addr_from_byte(a).unwrap_or(0), val)
                    }
                }
                Address::Bitaddr(addr, bit) => {
                    let word = self.${concat(read_, $type)}(addr as usize) & (0xFF ^ (1 << bit));

                    self.${concat(write_, $type)}(addr as usize, word | ((val & 1) << bit));
                }
                Address::Indirect16(a, c) => {
                    self.${concat(write_dpp_addressed_, $type)}(self.read_gpr_word(a) + c, val);
                }
                _ => return Err(Error::NotWriteable),
            }
            Ok(())
        }
    };
}
macro_rules! mk_read_fn {
    ($type:ident, $t:ty) => {
        pub fn ${concat(read_address_, $type)}<const N: u16>(&mut self, address: Address) -> Result<$t, Error> {
            Ok(match address {
                Address::GPR(a) => self.${concat(read_gpr_, $type)}(a),
                Address::Indirect(a) => self.${concat(read_dpp_addressed_, $type)}(self.read_gpr_word(a)),
                Address::IndirectIncr(a) => {
                    let gpr = self.read_gpr_word(a);

                    // TODO: not sure if this is correct...
                    self.write_gpr_word(a, (Wrapping(gpr) + Wrapping(N)).0);
                    self.${concat(read_dpp_addressed_, $type)}(gpr)
                }
                Address::IndirectDecr(a) => {
                    let gpr = self.read_gpr_word(a);

                    // TODO: not sure if this is correct...
                    self.write_gpr_word(a, (Wrapping(gpr) - Wrapping(N)).0);

                    self.${concat(read_dpp_addressed_, $type)}(gpr)
                }
                Address::Data16(a) => a as $t,
                Address::Data8(a) => a as $t,
                Address::Data4(a) => a as $t,
                Address::Data3(a) => a as $t,
                Address::Mem(a) => self.${concat(read_dpp_addressed_, $type)}(a),
                Address::Reg(a) => {
                    if 0xF0 & a == 0xF0 {
                        self.${concat(read_gpr_, $type)}(a)
                    } else {
                        // TODO: handle ESFR
                        self.${concat(read_, $type)}(registers::sfr_addr_from_byte(a).unwrap_or(0))
                    }
                }
                Address::Special(s) => {
                    let mode = s >> 3;

                    if mode == 0 {
                        s as $t
                    } else {
                        let gpr = s & 0b11;
                        let v = self.read_gpr_word(gpr);
                        if (s >> 2) & 0b01 == 1 {
                            self.write_gpr_word(gpr, v + N);
                        }
                        self.${concat(read_dpp_addressed_, $type)}(v)
                    }
                }
                Address::Bitaddr(addr, bit) => (self.${concat(read_, $type)}(addr as usize) >> bit) & 1,
                Address::Indirect16(a, c) => {
                    self.${concat(read_dpp_addressed_, $type)}(self.read_gpr_word(a) + c)
                }
                _ => return Err(Error::NotReadable),
            })
        }
    };
}

pub struct Interpreter {
    pub memory: Vec<u8>,
    instruction_pointer: u16,
}

impl Interpreter {
    pub fn new() -> Self {
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
                Operation::Add(a, b) => {
                    let a_initial = self.read_address_word::<2>(a)?;
                    let b_val = self.read_address_word::<2>(b)?;
                    self.write_address_word::<2>(a, a_initial + b_val)?;
                }
                Operation::SubCB(a, b) => println!(
                    "  a: 0x{:04x}, b: 0x{:04x}",
                    self.read_address_word::<1>(a)?,
                    self.read_address_word::<1>(b)?
                ),
                Operation::OrB(a, b) => println!(
                    "  a: 0x{:04x}, b: 0x{:04x}",
                    self.read_address_word::<1>(a)?,
                    self.read_address_word::<1>(b)?
                ),
                Operation::Mov(a, b) => {
                    let w = self.read_address_word::<1>(b)?;
                    self.write_address_word::<1>(a, w)?
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
    pub fn read_dpp_addressed_byte(&self, address: u16) -> u8 {
        self.read_byte(self.get_dpp_address(address))
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
    pub fn write_dpp_addressed_byte(&mut self, address: u16, byte: u8) {
        self.write_byte(self.get_dpp_address(address), byte);
    }

    pub fn write_gpr_word(&mut self, gpr: u8, word: u16) {
        let addr = self.gpr_address(gpr);
        self.write_word(addr, word);
    }
    pub fn write_gpr_byte(&mut self, gpr: u8, byte: u8) {
        let addr = self.gpr_address(gpr);
        self.write_byte(addr, byte);
    }

    mk_write_fn!(byte, u8);
    mk_write_fn!(word, u16);
    mk_read_fn!(byte, u8);
    mk_read_fn!(word, u16);
}
