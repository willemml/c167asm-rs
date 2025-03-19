use crate::Error;
use crate::Result;
use crate::addressing::*;

macro_rules! instructions {
    {$($group:ident($($arg:ident),*) {$($name:ident($($an:ident: $at:ty),*) = $num:expr $(,$ext:literal)*)*})*} => {
        #[derive(Debug, Copy, Clone)]
        pub enum Operation {
            $($group($(Address ${ignore($arg)}),*),)*
            JmpR(ConditionCode, u8),
            Bclr(Bitaddr),
            Bset(Bitaddr),
        }

        impl TryFrom<Operation> for Instruction {
            type Error = Error;
            fn try_from(value: Operation) -> Result<Self> {
                match value {
                    Operation::JmpR(cc, n) => Ok(Self::JmpR(cc,n)),
                    Operation::Bclr(n) => Ok(Self::Bclr(n)),
                    Operation::Bset(n) => Ok(Self::Bset(n)),
                    $(
                      Operation::$group($($arg),*) => {
                          #[allow(irrefutable_let_patterns)]
                          #[allow(unused_parens)]
                          $(if let ($(Ok($an)),*) = ($(<$at>::try_from($an)),*) {
                              Ok(Instruction::$name($($an),*))
                          } else)* {
                              Err(Error::BadArgs)
                          }
                      }
                    )*
                }
            }
        }

        impl From<Instruction> for Operation {
            fn from(value: Instruction) -> Self {
                match value {
                    $($(
                        Instruction::$name($($an),*) => Self::$group($($an.into()),*),
                    )*)*

                    Instruction::JmpR(cc, n) => Self::JmpR(cc,n),
                    Instruction::Bclr(a) => Self::Bclr(a),
                    Instruction::Bset(a) => Self::Bset(a),
                }
            }
        }

        #[repr(u8)]
        #[derive(Debug, Copy, Clone, strum::FromRepr)]
        enum OpCode {
            $($($name = $num,)*)*
        }

        #[repr(u8)]
        #[derive(Debug, Copy, Clone)]
        pub enum Instruction {
            $($($name($($at),*) = $num,)*)*
            JmpR(ConditionCode, u8) = 0x0F,
            Bclr(Bitaddr) = 0x0E,
            Bset(Bitaddr) = 0x0D,
        }

        impl Instruction {
            pub fn read<R: std::io::Read>(reader: &mut R) -> Result<Self> {
                let mut byte = 0;
                reader.read_exact(std::slice::from_mut(&mut byte))?;
                match byte & 0x0F {
                    0x0D => {
                        Ok(Self::JmpR(ConditionCode::from_repr(byte >> 4).unwrap(), Rel::read(reader)?.0.0))
                    }
                    0x0E => {
                        Ok(Self::Bclr(Bitaddr(Bitoff::read(reader)?.0.0, byte >> 4)))
                    }
                    0x0F => {
                        Ok(Self::Bset(Bitaddr(Bitoff::read(reader)?.0.0, byte >> 4)))
                    }
                    _ => match OpCode::from_repr(byte).ok_or(Error::InvalidOpCode)? {$($(
                        OpCode::$name => {
                            #[allow(unused)]
                            #[allow(unused_parens)]
                            let read = <($($at),*)>::read(reader)?;

                            #[allow(unused_variables)]
                            let mut extra: [u8; ${count($ext)}] = [$(0u8 ${ignore($ext)}),*];

                            reader.read_exact(&mut extra)?;

                            Ok(Self::$name($(${ignore($at)} read.${index()}),*))
                        })*)*
                    }
                }
            }
            pub fn write<W: std::io::Write>(&self, writer: &mut W) -> Result {
                match self {
                    Self::JmpR(cc, rel) => {
                        writer.write(&[0x0D | (*cc as u8) << 4, *rel]).unwrap();
                    }
                    Self::Bclr(bitaddr) => {
                        writer.write(&[0x0E | bitaddr.1 << 4, bitaddr.0]).unwrap();
                    }
                    Self::Bset(bitaddr) => {
                        writer.write(&[0x0F | bitaddr.1 << 4, bitaddr.0]).unwrap();
                    }
                    $($(
                        #[allow(non_snake_case)]
                        Self::$name($($an),*) => {
                            writer.write(&[OpCode::$name as u8 $(,$ext)*]).unwrap();
                            ($(*$an),*).write(writer)?;
                        }
                    )*)*
                }
                Ok(())
            }

            pub fn len(&self) -> usize {
                let mut bytes = Vec::with_capacity(4);
                self.write(&mut bytes).unwrap();
                bytes.len()
            }
        }
    };

    {
        $($specn:ident = $start:expr)*
        ;
        $($single:ident = $ss:expr $(,$ext:literal)*)*
        ;
        $($name1:ident($at1:ty) = $num1:expr)*
        ;
        $($group2:ident {
            $($name2:ident($at2:ty, $bt2:ty) = $num2:expr)*
        })*
        :singles: {
            $($name2s:ident($at2s:ty, $bt2s:ty) = $num2s:expr)*
        }
        ;
        $($name3:ident($at3:ty, $bt3:ty, $ct3:ty) = $num3:expr)*
    } => {
        instructions! {
        $($specn(a, b) {
            ${concat($specn,RR)}(a: GPR, b: GPR) = 0x00 + $start
            ${concat($specn,RS)}(a: Reg, b: Mem) = 0x08 + $start
            ${concat($specn,RD)}(a: Mem, b: Reg) = 0x06 + $start
            ${concat($specn,RM)}(a: Reg, b: Data16) = 0x02 + $start
            ${concat($specn,MR)}(a: GPR, b: Special) = 0x04 + $start
        })*
        $($single() {
            $single() = $ss $(,$ext)*
        })*
        $($name1(a) {
            $name1(a: $at1) = $num1
        })*
        $($group2(a, b) {
        $(
            $name2(a: $at2, b: $bt2) = $num2
        )*
        })*
        $($name2s(a, b) { $name2s(a: $at2s, b: $bt2s) = $num2s })*
        $($name3(a, b, c) { $name3(a: $at3, b: $bt3, c: $ct3) = $num3 })*
        }
    }
}

instructions! {
    Add = 0x00
    AddB = 0x01
    AddC = 0x10
    AddCB = 0x11
    And = 0x60
    AndB = 0x61
    Cmp = 0x40
    CmpB = 0x41
    Or = 0x70
    OrB = 0x71
    Sub = 0x20
    SubB = 0x21
    SubC = 0x30
    SubCB = 0x31
    Xor = 0x50
    XorB = 0x51

    ;

    DISWDT = 0xA5, 0x5A, 0xA5, 0xA5
    EINIT = 0xB5, 0x4A, 0xB5, 0xB5
    IDLE = 0x87, 78, 87, 87
    NOP = 0xCC, 0x00
    PWRDN = 0x97, 0x68, 0x97, 0x97
    RET = 0xCB, 0x00
    RETI = 0xFB, 0x88
    RETS = 0xDB, 0x00
    SRST = 0xB7, 0x48, 0xB7, 0xB7
    SRVWDT = 0xA7, 0x58, 0xA7, 0xA7

    ;

    AtEx(AtEx) = 0xD1
    CallR(Rel) = 0xBB
    CPLB(GPR) = 0xB1
    CPL(GPR) = 0x91
    Div(GPR) = 0x4B
    Divl(GPR) = 0x6B
    Divlu(GPR) = 0x7B
    Divu(GPR) = 0x5B
    EXTo(EXTSeq) = 0xD7
    EXTreg(EXTRSeq) = 0xDC
    NegB(GPR) = 0xA1
    Neg(GPR) = 0x81
    Pop(Reg) = 0xFC
    Push(Reg) = 0xEC
    RETP(Reg) = 0xEB
    Trap(Trap7) = 0x9B

    ;

    Ashr {
        AshrRD(GPR, Data4) = 0xBC
        AshrRR(GPR, GPR) = 0xAC
    }

    Cmpd1 {
        Cmpd1(GPR, Data16) = 0xA6
        Cmpd1RD4(GPR, Data4) = 0xA0
        Cmpd1RM(GPR, Mem) = 0xA2
    }
    Cmpd2 {
        Cmpd2(GPR, Data16) = 0xB6
        Cmpd2RD4(GPR, Data4) = 0xB0
        Cmpd2RM(GPR, Mem) = 0xB2
    }
    Cmpi1 {
        Cmpi1D16(GPR, Data16) = 0x86
        Cmpi1D4(GPR, Data4) = 0x80
        Cmpi1M(GPR, Mem) = 0x82
    }
    Cmpi2 {
        Cmpi2D16(GPR, Data16) = 0x96
        Cmpi2D4(GPR, Data4) = 0x90
        Cmpi2M(GPR, Mem) = 0x92
    }
    MovB {
        MovBI16R(Indirect16, GPR) = 0xE4
        MovBIdR(IndirectDecr, GPR) = 0x89
        MovBIIi(Indirect, IndirectIncr) = 0xE9
        MovBII(Indirect, Indirect) = 0xC9
        MovBIM(Indirect, Mem) = 0xA4
        MovBIR(Indirect, GPR) = 0xB9
        MovBMI(Mem, Indirect) = 0xB4
        MovBMR(Mem, Reg) = 0xF7
        MovBRD4(GPR, Data4) = 0xE1
        MovBRD8(Reg, Data8) = 0xE7
        MovBRI16(GPR, Indirect16) = 0xF4
        MovBRI(GPR, Indirect) = 0xA9
        MovBRIi(GPR, IndirectIncr) = 0x99
        MovBRM(Reg, Mem) = 0xF3
        MovBRR(GPR, GPR) = 0xF1
    }
    MovBZ {
        MovBZMR(Mem, Reg) = 0xC5
        MovBZRM(Reg, Mem) = 0xC2
        MovBZRRb(GPR, GPR) = 0xC0
    }
    MovBS {
        MovBSRR(GPR, GPR) = 0xD0
        MovBSRM(Reg, Mem) = 0xD2
        MovBSMR(Mem, Reg) = 0xD5
    }
    Mov {
        MovI16R(Indirect16, GPR) = 0xC4
        MovIdR(IndirectDecr, GPR) = 0x88
        MovIIi(Indirect, IndirectIncr) = 0xE8
        MovII(Indirect, Indirect) = 0xC8
        MovIM(Indirect, Mem) = 0x84
        MovIR(Indirect, GPR) = 0xB8
        MovMI(Mem, Indirect) = 0x94
        MovMR(Mem, Reg) = 0xF6
        MovRD16(GPR, Data16) = 0xE6
        MovRD4(GPR, Data4) = 0xE0
        MovRI16(GPR, Indirect16) = 0xD4
        MovRI(GPR, Indirect) = 0xA8
        MovRIi(GPR, IndirectIncr) = 0x98
        MovRM(Reg, Mem) = 0xF2
        MovRR(GPR, GPR) = 0xF0
    }
    Rol {
        RolRD(GPR, Data4) = 0x1C
        RolRR(GPR, GPR) = 0x0C
    }
    Ror {
        RorRD(GPR, Data4) = 0x3C
        RorRR(GPR, GPR) = 0x2C
    }
    Shl {
        ShlRD(GPR, Data4) = 0x5C
        ShlRR(GPR, GPR) = 0x4C
    }
    Shr {
        ShrRD(GPR, Data4) = 0x7C
        ShrRR(GPR, GPR) = 0x6C
    }

    :singles: {
        Band(Bitaddr, Bitaddr) = 0x6A
        Bcmp(Bitaddr, Bitaddr) = 0x2A
        Bmov(Bitaddr, Bitaddr) = 0x4A
        BmovN(Bitaddr, Bitaddr) = 0x3A
        Bor(Bitaddr, Bitaddr) = 0x5A
        Bxor(Bitaddr, Bitaddr) = 0x7A
        CallA(ConditionCode, Caddr) = 0xCA
        CallI(ConditionCode, Indirect) = 0xAB
        JB(Bitaddr, Rel) = 0x8A
        JBC(Bitaddr, Rel) = 0xAA
        JmpA(ConditionCode, Caddr) = 0xEA
        JmpI(ConditionCode, Indirect) = 0x9C
        JmpS(Seg, Caddr) = 0xFA
        JNB(Bitaddr, Rel) = 0x9A
        JNBS(Bitaddr, Rel) = 0xBA
        Mul(GPR, GPR) = 0x0B
        Mulu(GPR, GPR) = 0x1B
        PRIOR(GPR, GPR) = 0x2B
        SCXT(Reg, Data16) = 0xC6
    }

    ;

    BFLDL(Bitoff, Mask8, Data8) = 0x0A
    BFLDH(Bitoff, Mask8, Data8) = 0x1A
}
