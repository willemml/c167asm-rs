#![feature(macro_metavar_expr)]
#![feature(macro_metavar_expr_concat)]

use std::{io::Bytes, marker::PhantomData};

use strum::FromRepr;

fn main() {
    println!("Hello, world!");
}
macro_rules! mk_instr {
    ($name:ident($($arg:ident),*) {$(
        $variant:ident($($argname:ident: $addr:ident),*): $byte:expr
    ),*}) => {
        #[repr(u8)]
        pub enum ${concat($name, Variants)} {$(
            $variant($($addr),*) = $byte
        ),*}

        pub struct $name($(${ignore($arg)} pub Address),*);

        impl From<${concat($name, Variants)}> for $name {
            fn from(value: ${concat($name, Variants)}) -> Self {
                match value {$(
                    ${concat($name, Variants)}::$variant($($argname),*) =>
                        $name($($argname.into()),*)
                ),*}
            }
        }
    };
}

macro_rules! mk_twoarg_w {
    ($name:ident $base:literal) => {
        mk_instr!($name(a, b) {
            RwRw(a: GPR, b: GPR): $base,
            RD16(a: Reg, b: Data16): $base + 2,
            RM(a: Reg, b: Mem): $base + 4,
            MR(a: Mem, b: Reg): $base + 6,
            S(a: GPR, b: Special): $base + 8
        });
    };
}
macro_rules! mk_twoarg_b {
    ($name:ident $base:literal) => {
        mk_instr!($name(a, b) {
            RwRw(a: GPR, b: GPR): $base,
            RD8(a: Reg, b: Data8): $base + 2,
            RM(a: Reg, b: Mem): $base + 4,
            MR(a: Mem, b: Reg): $base + 6,
            S(a: GPR, b: Special): $base + 8
        });
    };
}

mk_twoarg_w!(Add 0x00);
mk_twoarg_b!(AddB 0x01);

mk_twoarg_w!(AddC 0x10);
mk_twoarg_b!(AddCB 0x11);

mk_twoarg_w!(Sub 0x20);
mk_twoarg_b!(SubB 0x21);

mk_twoarg_w!(SubC 0x30);
mk_twoarg_b!(SubCB 0x31);

mk_twoarg_w!(Cmp 0x40);
mk_twoarg_b!(CmpB 0x41);

mk_twoarg_w!(Xor 0x50);
mk_twoarg_b!(XorB 0x51);

mk_twoarg_w!(And 0x60);
mk_twoarg_b!(AndB 0x61);

mk_twoarg_w!(Or 0x70);
mk_twoarg_b!(OrB 0x71);

macro_rules! mk_address {
    {
        $($name:ident($type:ty)),*
    } => {
        pub enum Address {$(
           $name($type)
        ),*}

        $(
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
    Mem(u32),
    Reg(u8),
    Special(u8),
    Mask8(u8),
    Bitoff(u8)
}

enum Arg {
    None,
    Single(Address),
    Pair(Address, Address),
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
enum InstrFull {
    AddRR(GPR, GPR) = 0,
    AddRS(GPR, Special) = 8,
    AddRD(Reg, Data16) = 6,
    AddRM(Reg, Mem) = 2,
    AddMR(Mem, Reg) = 4,
    BFLDH(Bitoff, Mask8, Data8) = 0x1A,
}

struct OpReader<A, B = Empty, C = Empty> {
    a: PhantomData<A>,
    b: PhantomData<B>,
    c: PhantomData<C>,
}

struct Empty;

impl InstrFull {
    fn read<R: std::io::Read>(reader: &mut Bytes<R>) -> Self {
        match Instr::from_repr(reader.next().unwrap().unwrap()).unwrap() {
            Instr::AddRR => {
                let (r1, r2) = OpReader::<GPR, GPR>::read(reader);

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

impl OpReader<GPR, GPR> {
    fn read<R: std::io::Read>(reader: &mut Bytes<R>) -> (GPR, GPR) {
        let byte = reader.next().unwrap().unwrap();

        (GPR((byte & 0xF0) >> 4), GPR(byte & 0x0F))
    }
}

impl OpReader<GPR, Special> {
    fn read<R: std::io::Read>(reader: &mut Bytes<R>) -> (GPR, Special) {
        let byte = reader.next().unwrap().unwrap();
        let reg = GPR((byte >> 4) & 0x0F);
        (reg, Special(byte & 0x0F))
    }
}

impl OpReader<Bitoff, Mask8, Data8> {
    fn read<R: std::io::Read>(reader: &mut Bytes<R>) -> (Bitoff, Mask8, Data8) {
        let b1 = reader.next().unwrap().unwrap();
        let b2 = reader.next().unwrap().unwrap();
        let b3 = reader.next().unwrap().unwrap();
        (Bitoff(b1), Mask8(b2), Data8(b3))
    }
}

impl Instr {
    fn read_args<R: std::io::Read>(&self, reader: &mut Bytes<R>) -> Arg {
        let byte = reader.next().unwrap().unwrap();
        match self {
            Instr::AddRR => Arg::Pair(Address::GPR((byte & 0xF0) >> 4), Address::GPR(byte & 0x0F)),
            Instr::AddRS => {
                let reg = Address::GPR((byte >> 4) & 0x0F);
                let mode = (byte >> 2) & 0b11;
                let second = if mode == 0b10 {
                    Address::IndirectWord(byte & 0b11)
                } else if mode == 0b11 {
                    Address::IndirectWordIncr(byte & 0b11)
                } else {
                    Address::Data3(byte & 0b111)
                };
                Arg::Pair(reg, second)
            }
            Instr::AddRD => todo!(),
            Instr::AddRM => todo!(),
            Instr::AddMR => todo!(),
            Instr::BFLDH => todo!(),
        }
    }
}

struct Instruction {
    code: Instr,
    arg: Arg,
}

impl Instruction {
    fn read<R: std::io::Read>(reader: &mut Bytes<R>) -> Self {
        let code = Instr::from_repr(reader.next().unwrap().unwrap()).unwrap();

        todo!()
    }
}
