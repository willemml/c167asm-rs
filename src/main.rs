use std::{io::Bytes, marker::PhantomData};

use strum::FromRepr;

fn main() {
    println!("Hello, world!");
}

macro_rules! mk_address {
    {
        $($name:ident($type:ty)),*
    } => {
        #[derive(Debug, Copy, Clone)]
        pub enum Address {$(
           $name($type)
        ),*}

        $(
        #[derive(Debug, Copy, Clone)]
        pub struct $name($type);

        impl From<$name> for Address {
            fn from(value: $name) -> Self {
                Self::$name(value.0)
            }
        }
        )*
    };
}

mk_address! {
    GPR(u8),
    IndirectWord(u8),
    IndirectWordIncr(u8),
    Data16(u16),
    Data8(u8),
    Data3(u8),
    Mem(u16),
    Reg(u8),
    Special(u8),
    Mask8(u8),
    Bitoff(u8)
}

#[repr(u8)]
#[derive(Debug, Copy, Clone, FromRepr)]
enum Instr {
    AddRR = 0,
    AddRS = 8,
    AddRD = 6,
    AddRM = 2,
    AddMR = 4,
    BFLDH = 0x1A,
}

#[repr(u8)]
pub enum InstrFull {
    AddRR(GPR, GPR) = 0,
    AddRS(GPR, Special) = 8,
    AddRD(Reg, Data16) = 6,
    AddRM(Reg, Mem) = 2,
    AddMR(Mem, Reg) = 4,
    BFLDH(Bitoff, Mask8, Data8) = 0x1A,
}

macro_rules! instructions {
    {$($name:ident($($an:ident: $at:ty),*) = $num:expr)*} => {
        #[repr(u8)]
        #[derive(Debug, Copy, Clone, FromRepr)]
        pub enum OpCode {
            $($name = $num,)*
        }

        #[repr(u8)]
        #[derive(Debug, Copy, Clone)]
        pub enum Instruction {
            $($name($($at),*) = $num,)*
        }

        impl Instruction {
            pub fn read<R: std::io::Read>(reader: &mut Bytes<R>) -> Self {
                match OpCode::from_repr(reader.next().unwrap().unwrap()).unwrap() {
                    $(OpCode::$name => {
                        let ($($an),*) = <($($at),*)>::read(reader);

                        Self::$name($($an),*)
                    })*
                }
            }
        }
    };
}

instructions! {
    AddRR(a: GPR, b: GPR) = 0
}

impl InstrFull {
    pub fn read<R: std::io::Read>(reader: &mut Bytes<R>) -> Self {
        match Instr::from_repr(reader.next().unwrap().unwrap()).unwrap() {
            Instr::AddRR => {
                let (r1, r2) = <(GPR, GPR)>::read(reader);

                Self::AddRR(r1, r2)
            }
            Instr::AddRS => todo!(),
            Instr::AddRD => todo!(),
            Instr::AddRM => todo!(),
            Instr::AddMR => todo!(),
            Instr::BFLDH => todo!(),
        }
    }
}

pub trait ArgRead {
    fn read<R: std::io::Read>(reader: &mut Bytes<R>) -> Self;
}

impl ArgRead for (Reg, Data16) {
    fn read<R: std::io::Read>(reader: &mut Bytes<R>) -> (Reg, Data16) {
        let b1 = reader.next().unwrap().unwrap();
        let b2 = reader.next().unwrap().unwrap();
        let b3 = reader.next().unwrap().unwrap();

        (Reg(b1), Data16(u16::from_le_bytes([b2, b3])))
    }
}
impl ArgRead for (Reg, Data8) {
    fn read<R: std::io::Read>(reader: &mut Bytes<R>) -> (Reg, Data8) {
        let b1 = reader.next().unwrap().unwrap();
        let b2 = reader.next().unwrap().unwrap();

        (Reg(b1), Data8(b2))
    }
}
impl ArgRead for (Reg, Mem) {
    fn read<R: std::io::Read>(reader: &mut Bytes<R>) -> (Reg, Mem) {
        let b1 = reader.next().unwrap().unwrap();
        let b2 = reader.next().unwrap().unwrap();
        let b3 = reader.next().unwrap().unwrap();

        (Reg(b1), Mem(u16::from_le_bytes([b2, b3])))
    }
}
impl ArgRead for (Mem, Reg) {
    fn read<R: std::io::Read>(reader: &mut Bytes<R>) -> (Mem, Reg) {
        let b1 = reader.next().unwrap().unwrap();
        let b2 = reader.next().unwrap().unwrap();
        let b3 = reader.next().unwrap().unwrap();

        (Mem(u16::from_le_bytes([b2, b3])), Reg(b1))
    }
}

impl ArgRead for (GPR, GPR) {
    fn read<R: std::io::Read>(reader: &mut Bytes<R>) -> (GPR, GPR) {
        let byte = reader.next().unwrap().unwrap();

        (GPR((byte & 0xF0) >> 4), GPR(byte & 0x0F))
    }
}

impl ArgRead for (GPR, Special) {
    fn read<R: std::io::Read>(reader: &mut Bytes<R>) -> (GPR, Special) {
        let byte = reader.next().unwrap().unwrap();
        let reg = GPR((byte >> 4) & 0x0F);
        (reg, Special(byte & 0x0F))
    }
}

impl ArgRead for (Bitoff, Mask8, Data8) {
    fn read<R: std::io::Read>(reader: &mut Bytes<R>) -> (Bitoff, Mask8, Data8) {
        let b1 = reader.next().unwrap().unwrap();
        let b2 = reader.next().unwrap().unwrap();
        let b3 = reader.next().unwrap().unwrap();
        (Bitoff(b1), Mask8(b2), Data8(b3))
    }
}
