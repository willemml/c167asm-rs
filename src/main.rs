#![feature(macro_metavar_expr)]

use std::io::Bytes;

use strum::FromRepr;

fn main() {
    println!("Hello, world!");
}

macro_rules! mk_address {
    {
        $($name:ident($($type:ty),*)),*
    } => {
        #[derive(Debug, Copy, Clone)]
        pub enum Address {$(
           $name($($type),*)
        ),*}

        $(
        #[derive(Debug, Copy, Clone)]
        pub struct $name($($type),*);

        impl From<$name> for Address {
            fn from(value: $name) -> Self {
                Self::$name($(value.${index()} ${ignore($type)}),*)
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
    Data4(u8),
    Data3(u8),
    Mem(u16),
    Reg(u8),
    Special(u8),
    Mask8(u8),
    Bitoff(u8),
    Bitaddr(u8,u8),
    Pag10(u16),
    Seg8(u8),
    Irang2(u8),
    Rel(u8)
}

macro_rules! instructions {
    {$($name:ident($($at:ty),*) = $num:expr)*} => {
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
                        #[allow(unused_parens)]
                        let read = <($($at),*)>::read(reader);

                        Self::$name($(${ignore($at)} read.${index()}),*)
                    })*
                }
            }
        }
    };
    // {
    //     $($nameo:ident($o:ty) = $numo:expr)*;
    //     $($nameab:ident($a:ty, $b:ty) = $numab:expr)*;
    //     $($name:ident($($id:ident: $type:ty),*) = $num:expr)*
    // } => {
    //     instructions! {
    //         $($nameo(a: $o) = $numo)*
    //         $($nameab(a: $a, b: $b) = $numab)*
    //         $($name($($id: $type),*) = $num)*
    //     }
    // }
}

pub struct C<T>(T);

instructions! {
    Jmpr0(Rel) = 0x0D
    Jmpr1(Rel) = 0x1D
    Jmpr2(Rel) = 0x2D
    Jmpr3(Rel) = 0x3D
    Jmpr4(Rel) = 0x4D
    Jmpr5(Rel) = 0x5D
    Jmpr6(Rel) = 0x6D
    Jmpr7(Rel) = 0x7D
    Jmpr8(Rel) = 0x8D
    Jmpr9(Rel) = 0x9D
    JmprA(Rel) = 0xAD
    JmprB(Rel) = 0xBD
    JmprC(Rel) = 0xCD
    JmprD(Rel) = 0xDD
    JmprE(Rel) = 0xED
    JmprF(Rel) = 0xFD

    Bclr0(Bitoff) = 0x0E
    Bclr1(Bitoff) = 0x1E
    Bclr2(Bitoff) = 0x2E
    Bclr3(Bitoff) = 0x3E
    Bclr4(Bitoff) = 0x4E
    Bclr5(Bitoff) = 0x5E
    Bclr6(Bitoff) = 0x6E
    Bclr7(Bitoff) = 0x7E
    Bclr8(Bitoff) = 0x8E
    Bclr9(Bitoff) = 0x9E
    BclrA(Bitoff) = 0xAE
    BclrB(Bitoff) = 0xBE
    BclrC(Bitoff) = 0xCE
    BclrD(Bitoff) = 0xDE
    BclrE(Bitoff) = 0xEE
    BclrF(Bitoff) = 0xFE

    Bset0(Bitoff) = 0x0F
    Bset1(Bitoff) = 0x1F
    Bset2(Bitoff) = 0x2F
    Bset3(Bitoff) = 0x3F
    Bset4(Bitoff) = 0x4F
    Bset5(Bitoff) = 0x5F
    Bset6(Bitoff) = 0x6F
    Bset7(Bitoff) = 0x7F
    Bset8(Bitoff) = 0x8F
    Bset9(Bitoff) = 0x9F
    BsetA(Bitoff) = 0xAF
    BsetB(Bitoff) = 0xBF
    BsetC(Bitoff) = 0xCF
    BsetD(Bitoff) = 0xDF
    BsetE(Bitoff) = 0xEF
    BsetF(Bitoff) = 0xFF

    AddRR(GPR, GPR) = 0x00
    AddRS(GPR, Special) = 0x08
    AddRD(Reg, Data16) = 0x06
    AddRM(Reg, Mem) = 0x02
    AddMR(Mem, Reg) = 0x04

    AddBRR(GPR, GPR) = 0x01
    AddBRS(GPR, Special) = 0x09
    AddBRD(Reg, Data8) = 0x07
    AddBRM(Reg, Mem) = 0x03
    AddBMR(Mem, Reg) = 0x05

    AddCRR(GPR, GPR) = 0x10
    AddCRS(GPR, Special) = 0x18
    AddCRD(Reg, Data16) = 0x16
    AddCRM(Reg, Mem) = 0x12
    AddCMR(Mem, Reg) = 0x14

    AddCBRR(GPR, GPR) = 0x11
    AddCBRS(GPR, Special) = 0x19
    AddCBRD(Reg, Data16) = 0x17
    AddCBRM(Reg, Mem) = 0x13
    AddCBMR(Mem, Reg) = 0x15

    SubRR(GPR, GPR) = 0x20
    SubRS(GPR, Special) = 0x28
    SubRD(Reg, Data16) = 0x26
    SubRM(Reg, Mem) = 0x22
    SubMR(Mem, Reg) = 0x24

    SubBRR(GPR, GPR) = 0x21
    SubBRS(GPR, Special) = 0x29
    SubBRD(Reg, Data8) = 0x27
    SubBRM(Reg, Mem) = 0x23
    SubBMR(Mem, Reg) = 0x25

    SubCRR(GPR, GPR) = 0x30
    SubCRS(GPR, Special) = 0x38
    SubCRD(Reg, Data16) = 0x36
    SubCRM(Reg, Mem) = 0x32
    SubCMR(Mem, Reg) = 0x34

    SubCBRR(GPR, GPR) = 0x31
    SubCBRS(GPR, Special) = 0x39
    SubCBRD(Reg, Data16) = 0x37
    SubCBRM(Reg, Mem) = 0x33
    SubCBMR(Mem, Reg) = 0x35

    AndRR(GPR, GPR) = 0x60
    AndRS(GPR, Special) = 0x68
    AndRD(Reg, Data16) = 0x66
    AndRM(Reg, Mem) = 0x62
    AndMR(Mem, Reg) = 0x64

    AndBRR(GPR, GPR) = 0x61
    AndBRS(GPR, Special) = 0x69
    AndBRD(Reg, Data8) = 0x67
    AndBRM(Reg, Mem) = 0x63
    AndBMR(Mem, Reg) = 0x65

    OrRR(GPR, GPR) = 0x70
    OrRS(GPR, Special) = 0x78
    OrRD(Reg, Data16) = 0x76
    OrRM(Reg, Mem) = 0x72
    OrMR(Mem, Reg) = 0x74

    OrBRR(GPR, GPR) = 0x71
    OrBRS(GPR, Special) = 0x79
    OrBRD(Reg, Data8) = 0x77
    OrBRM(Reg, Mem) = 0x73
    OrBMR(Mem, Reg) = 0x75

    XorRR(GPR, GPR) = 0x50
    XorRS(GPR, Special) = 0x58
    XorRD(Reg, Data16) = 0x56
    XorRM(Reg, Mem) = 0x52
    XorMR(Mem, Reg) = 0x54

    XorBRR(GPR, GPR) = 0x51
    XorBRS(GPR, Special) = 0x59
    XorBRD(Reg, Data8) = 0x57
    XorBRM(Reg, Mem) = 0x53
    XorBMR(Mem, Reg) = 0x55

    CmpRR(GPR, GPR) = 0x40
    CmpRS(GPR, Special) = 0x48
    CmpRD(Reg, Data16) = 0x46
    CmpRM(Reg, Mem) = 0x42
    CmpMR(Mem, Reg) = 0x44

    CmpBRR(GPR, GPR) = 0x41
    CmpBRS(GPR, Special) = 0x49
    CmpBRD(Reg, Data8) = 0x47
    CmpBRM(Reg, Mem) = 0x43
    CmpBMR(Mem, Reg) = 0x45

    AshrRR(GPR, GPR) = 0xAB
    AshrRD(GPR, Data4) = 0xBC

    Mul(GPR, GPR) = 0x0B
    Mulu(GPR, GPR) = 0x1B

    RolRR(GPR, GPR) = 0x0C
    RolRD(GPR, Data4) = 0x1C

    BCMP(Bitaddr, Bitaddr) = 0x2A
    BMOVN(Bitaddr, Bitaddr) = 0x3A

    PRIOR(GPR, GPR) = 0x2B

    RorRR(GPR, GPR) = 0x2C
    RorRD(GPR, Data4) = 0x3C

    Bmov(Bitaddr, Bitaddr) = 0x4A
    Bor(Bitaddr, Bitaddr) = 0x5A
    Band(Bitaddr, Bitaddr) = 0x6A
    Bxor(Bitaddr, Bitaddr) = 0x7A

    Div(GPR) = 0x4B
    Divu(GPR) = 0x5B

    Divl(GPR) = 0x6B
    Divlu(GPR) = 0x7B

    ShlRR(GPR, GPR) = 0x4C
    ShlRD(GPR, Data4) = 0x5C

    ShrRR(GPR, GPR) = 0x6C
    ShrRD(GPR, Data4) = 0x7C

    BFLDL(Bitoff, Mask8, Data8) = 0x0A
    BFLDH(Bitoff, Mask8, Data8) = 0x1A
}

pub trait ArgRead {
    fn read<R: std::io::Read>(reader: &mut Bytes<R>) -> Self;
}

impl ArgRead for (Reg, Data16) {
    fn read<R: std::io::Read>(reader: &mut Bytes<R>) -> Self {
        let b1 = reader.next().unwrap().unwrap();
        let b2 = reader.next().unwrap().unwrap();
        let b3 = reader.next().unwrap().unwrap();

        (Reg(b1), Data16(u16::from_le_bytes([b2, b3])))
    }
}
impl ArgRead for (Bitaddr, Bitaddr) {
    fn read<R: std::io::Read>(reader: &mut Bytes<R>) -> Self {
        let b1 = reader.next().unwrap().unwrap();
        let b2 = reader.next().unwrap().unwrap();
        let b3 = reader.next().unwrap().unwrap();

        (Bitaddr(b1, b3 >> 4), Bitaddr(b2, b3 & 0x0F))
    }
}
impl ArgRead for (Reg, Data8) {
    fn read<R: std::io::Read>(reader: &mut Bytes<R>) -> Self {
        let b1 = reader.next().unwrap().unwrap();
        let b2 = reader.next().unwrap().unwrap();

        (Reg(b1), Data8(b2))
    }
}
impl ArgRead for (Reg, Mem) {
    fn read<R: std::io::Read>(reader: &mut Bytes<R>) -> Self {
        let b1 = reader.next().unwrap().unwrap();
        let b2 = reader.next().unwrap().unwrap();
        let b3 = reader.next().unwrap().unwrap();

        (Reg(b1), Mem(u16::from_le_bytes([b2, b3])))
    }
}
impl ArgRead for (Mem, Reg) {
    fn read<R: std::io::Read>(reader: &mut Bytes<R>) -> Self {
        let b1 = reader.next().unwrap().unwrap();
        let b2 = reader.next().unwrap().unwrap();
        let b3 = reader.next().unwrap().unwrap();

        (Mem(u16::from_le_bytes([b2, b3])), Reg(b1))
    }
}

#[allow(unused_parens)]
impl GPR {
    fn read<R: std::io::Read>(reader: &mut Bytes<R>) -> C<GPR> {
        let byte = reader.next().unwrap().unwrap();

        C(GPR(byte & 0x0F))
    }
}
#[allow(unused_parens)]
impl Bitoff {
    fn read<R: std::io::Read>(reader: &mut Bytes<R>) -> C<Bitoff> {
        let byte = reader.next().unwrap().unwrap();

        C(Bitoff(byte))
    }
}
#[allow(unused_parens)]
impl Rel {
    fn read<R: std::io::Read>(reader: &mut Bytes<R>) -> C<Rel> {
        let byte = reader.next().unwrap().unwrap();

        C(Rel(byte))
    }
}

impl ArgRead for (GPR, GPR) {
    fn read<R: std::io::Read>(reader: &mut Bytes<R>) -> Self {
        let byte = reader.next().unwrap().unwrap();

        (GPR(byte >> 4), GPR(byte & 0x0F))
    }
}

impl ArgRead for (GPR, Special) {
    fn read<R: std::io::Read>(reader: &mut Bytes<R>) -> Self {
        let byte = reader.next().unwrap().unwrap();
        let reg = GPR(byte >> 4);
        (reg, Special(byte & 0x0F))
    }
}

impl ArgRead for (GPR, Data4) {
    fn read<R: std::io::Read>(reader: &mut Bytes<R>) -> Self {
        let byte = reader.next().unwrap().unwrap();
        let data = Data4(byte >> 4);
        (GPR(byte & 0x0F), data)
    }
}

impl ArgRead for (Bitoff, Mask8, Data8) {
    fn read<R: std::io::Read>(reader: &mut Bytes<R>) -> Self {
        let b1 = reader.next().unwrap().unwrap();
        let b2 = reader.next().unwrap().unwrap();
        let b3 = reader.next().unwrap().unwrap();
        (Bitoff(b1), Mask8(b2), Data8(b3))
    }
}
