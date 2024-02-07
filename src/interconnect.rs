use crate::{bios::Bios, map};

pub struct Interconnect {
    bios: Bios,
}

impl Interconnect {
    pub fn new(bios: Bios) -> Interconnect {
        Interconnect { bios }
    }

    pub fn load32(&self, addr: u32) -> u32 {
        if addr % 4 != 0 {
            panic!("Unaligned load32 address: {:08x}", addr);
        }

        if let Some(offset) = map::BIOS.contains(addr) {
            return self.bios.load32(offset);
        }
        panic!("unhandled fetch32 at address {:08X}", addr);
    }

    pub fn store32(&self, addr: u32, val: u32) {
        if addr % 4 != 0 {
            panic!("Unaligned store32 address: {:08x}", addr);
        }

        if let Some(offset) = map::MEMCONTROL.contains(addr) {
            match offset {
                0 => {
                    if val != 0x1f00_0000 {
                        panic!("Bad exception 1 base address : 0x{:08X}", val)
                    }
                }
                4 => {
                    if val != 0x1f80_2000 {
                        panic!("Bad exception 2 base address : 0x{:08X}", val)
                    }
                }
                _ => println!("Unhandled write to MEMCONTROL register"),
            }
            return;
        }
        panic!("unhandled store32 into address {:08X}", addr);
    }
}
