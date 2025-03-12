use crate::addressing::*;

macro_rules! instructions {
    {$($name:ident($($at:ty),*) = $num:expr)*} => {
        #[repr(u8)]
        #[derive(Debug, Copy, Clone, strum::FromRepr)]
        pub enum OpCode {
            $($name = $num,)*
        }

        #[repr(u8)]
        #[derive(Debug, Copy, Clone)]
        pub enum Instruction {
            $($name($($at),*) = $num,)*
            JmpR(ConditionCode, u8) = 0x0F,
            Bclr(Bitoff, u8) = 0x0E,
            Bset(Bitoff, u8) = 0x0D,
        }

        impl Instruction {
            pub fn read<R: std::io::Read>(reader: &mut std::io::Bytes<R>) -> Self {
                let byte = reader.next().unwrap().unwrap();
                match byte & 0x0F {
                    0x0D => {
                        Self::JmpR(ConditionCode::from_repr(byte >> 4).unwrap(), Rel::read(reader).0.0)
                    }
                    0x0E => {
                        Self::Bclr(Bitoff::read(reader).0, byte >> 4)
                    }
                    0x0F => {
                        Self::Bset(Bitoff::read(reader).0, byte >> 4)
                    }
                    _ => match OpCode::from_repr(byte).unwrap() {$(
                        OpCode::$name => {
                            #[allow(unused)]
                            #[allow(unused_parens)]
                            let read = <($($at),*)>::read(reader);

                            Self::$name($(${ignore($at)} read.${index()}),*)
                        })*
                    }
                }
            }
        }
    };

    {
        $($specn:ident = $start:expr),*
        ;
        $($name:ident($($at:ty),*) = $num:expr)*
    } => {
        instructions! {
        $(
            ${concat($specn,RR)}(GPR, GPR) = 0x00 + $start
            ${concat($specn,RS)}(Reg, Mem) = 0x02 + $start
            ${concat($specn,RD)}(Mem, Reg) = 0x04 + $start
            ${concat($specn,RM)}(Reg, Data16) = 0x06 + $start
            ${concat($specn,MR)}(GPR, Special) = 0x08 + $start
        )*
        $(
            $name($($at),*) = $num
        )*
        }
    }
}

instructions! {
    Add = 0x00,
    AddB = 0x01,
    AddC = 0x10,
    AddCB = 0x11,
    Sub = 0x20,
    SubB = 0x21,
    SubC = 0x30,
    SubCB = 0x31,
    And = 0x60,
    AndB = 0x61,
    Or = 0x70,
    OrB = 0x71,
    Xor = 0x50,
    XorB = 0x51,
    Cmp = 0x40,
    CmpB = 0x41
    ;
    AshrRR(GPR, GPR) = 0xAC
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

    Cmpi1D4(GPR, Data4) = 0x80
    Cmpi1M(GPR, Mem) = 0x82
    Cmpi1D16(GPR, Data16) = 0x86

    Cmpi2D4(GPR, Data4) = 0x90
    Cmpi2M(GPR, Mem) = 0x92
    Cmpi2D16(GPR, Data16) = 0x96

    Cmpd1RD4(GPR, Data4) = 0xA0
    Cmpd1RM(GPR, Mem) = 0xA2
    Cmpd1(GPR, Data16) = 0xA6

    Cmpd2RD4(GPR, Data4) = 0xB0
    Cmpd2RM(GPR, Mem) = 0xB2
    Cmpd2(GPR, Data16) = 0xB6

    NegB(GPRb) = 0xA1
    Neg(GPR) = 0x81

    CPL(GPR) = 0x91
    CPLB(GPRb) = 0xB1

    DISWDT() = 0xA5
    EINIT() = 0xB5
    SRVWDT() = 0xA7
    SRST() = 0xB7
    NOP() = 0xCC

    RET() = 0xCB
    RETS() = 0xDB
    RETP(Reg) = 0xEB
    RETI() = 0xFB

    Push(Reg) = 0xEC
    Pop(Reg) = 0xFC

    MovIM(Indirect, Mem) = 0x84
    MovIdR(IndirectDecr, GPR) = 0x88
    MovMI(Mem, Indirect) = 0x94
    MovRIi(GPR, IndirectIncr) = 0x98
    MovRI(GPR, Indirect) = 0xA8
    MovIR(Indirect, GPR) = 0xB8
    MovI16R(Indirect16, GPR) = 0xC4
    MovRI16(GPR, Indirect16) = 0xD4
    MovII(Indirect, Indirect) = 0xC8

    MovBIdR(IndirectDecr, GPRb) = 0x89
    MovBRIi(GPRb, IndirectIncr) = 0x99
    MovBIM(Indirect, Mem) = 0xA4
    MovBRI(GPRb, Indirect) = 0xA9
    MovBMI(Mem, Indirect) = 0xB4
    MovBIR(Indirect, GPRb) = 0xB9
    MovBII(Indirect, Indirect) = 0xC9

    MovBZRRb(GPR, GPRb) = 0xC0
    MovBZRM(Reg, Mem) = 0xC2
    MovBZMR(Mem, Reg) = 0xC5

    SCXT(Reg, Data16) = 0xC6

    Trap(Trap7) = 0x9B

    JmpI(ConditionCode, Indirect) = 0x9C
    JmpA(ConditionCode, Caddr) = 0xEA

    JB(Bitaddr, Rel) = 0x8A
    JNB(Bitaddr, Rel) = 0x9A
    JBC(Bitaddr, Rel) = 0xAA
    JNBS(Bitaddr, Rel) = 0xBA

    CallI(ConditionCode, Indirect) = 0xAB
    CallR(Rel) = 0xBB
    CallA(ConditionCode, Caddr) = 0xCA

    BFLDL(Bitoff, Mask8, Data8) = 0x0A
    BFLDH(Bitoff, Mask8, Data8) = 0x1A
}
