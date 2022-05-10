mod armv4t;

use gdbstub::{
    arch,
    target::ext::base::BaseOps,
    target::{self, ext::base::singlethread::SingleThreadBase},
};

struct Target {}

struct Arch {}

#[derive(Default, Debug, Clone, PartialEq)]
struct Registers {
    r: [u32; 16],
    cpsr: u32,
}

#[derive(Debug)]
enum BreakpointKind {
    Arm,
    Thumb,
}

#[derive(Debug)]
struct RegId {}

impl target::Target for Target {
    type Arch = Arch;

    type Error = ();

    fn base_ops(&mut self) -> BaseOps<'_, Self::Arch, Self::Error> {
        BaseOps::SingleThread(self)
    }
}

impl SingleThreadBase for Target {
    fn read_registers(
        &mut self,
        regs: &mut <Self::Arch as arch::Arch>::Registers,
    ) -> target::TargetResult<(), Self> {
        todo!()
    }

    fn write_registers(
        &mut self,
        regs: &<Self::Arch as arch::Arch>::Registers,
    ) -> target::TargetResult<(), Self> {
        todo!()
    }

    fn read_addrs(
        &mut self,
        start_addr: <Self::Arch as arch::Arch>::Usize,
        data: &mut [u8],
    ) -> target::TargetResult<(), Self> {
        todo!()
    }

    fn write_addrs(
        &mut self,
        start_addr: <Self::Arch as arch::Arch>::Usize,
        data: &[u8],
    ) -> target::TargetResult<(), Self> {
        todo!()
    }
}

impl arch::Arch for Arch {
    type Usize = u32;

    type Registers = Registers;

    type BreakpointKind = BreakpointKind;

    type RegId = RegId;

    fn single_step_gdb_behavior() -> gdbstub::arch::SingleStepGdbBehavior {
        todo!()
    }

    fn target_description_xml() -> Option<&'static str> {
        Some(r#"<target version="1.0"><architecture>armv4t</architecture></target>"#)
    }
}

impl arch::Registers for Registers {
    type ProgramCounter = u32;

    fn pc(&self) -> Self::ProgramCounter {
        self.r[15]
    }

    fn gdb_serialize(&self, write_byte: impl FnMut(Option<u8>)) {
        todo!()
    }

    fn gdb_deserialize(&mut self, bytes: &[u8]) -> Result<(), ()> {
        todo!()
    }
}

impl arch::BreakpointKind for BreakpointKind {
    fn from_usize(kind: usize) -> Option<Self> {
        match kind {
            2 => Some(BreakpointKind::Thumb),
            4 => Some(BreakpointKind::Arm),
            _ => None,
        }
    }
}

impl arch::RegId for RegId {
    fn from_raw_id(id: usize) -> Option<(Self, Option<std::num::NonZeroUsize>)> {
        todo!()
    }
}
