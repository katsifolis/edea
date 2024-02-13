use crate::{
    bios::Bios,
    map::{self, mask_region},
    ram::Ram,
};

pub struct Interconnect {
    bios: Bios,
    ram: Ram,
}

impl Interconnect {
    pub fn new(bios: Bios, ram: Ram) -> Interconnect {
        Interconnect { bios, ram }
    }

    pub fn load8(&self, addr: u32) -> u8 {
        let absolute_addr = mask_region(addr);

        if let Some(offset) = map::BIOS.contains(absolute_addr) {
            return self.bios.load8(offset);
        }

        if let Some(offset) = map::RAM.contains(absolute_addr) {
            return self.ram.load8(offset);
        }

        if map::EXPANSION_1.contains(absolute_addr).is_some() {
            return 0xff;
        }

        panic!("unhandled fetch8 at absolute_address {:08X}", absolute_addr);
    }

    pub fn load32(&self, addr: u32) -> u32 {
        let absolute_addr = mask_region(addr);

        if absolute_addr % 4 != 0 {
            panic!("Unaligned load32 absolute_address: {:08x}", absolute_addr);
        }

        if let Some(offset) = map::BIOS.contains(absolute_addr) {
            return self.bios.load32(offset);
        }

        if let Some(offset) = map::RAM.contains(absolute_addr) {
            return self.ram.load32(offset);
        }

        if let Some(offset) = map::IRQ_CONTROL.contains(absolute_addr) {
            println!("IRQ control read: {:08X}", offset);
            return 0;
        }

        panic!(
            "unhandled fetch32 at absolute_address {:08X}",
            absolute_addr
        );
    }

    pub fn store8(&mut self, addr: u32, val: u8) {
        let absolute_addr = mask_region(addr);

        if let Some(offset) = map::EXPANSION_2.contains(absolute_addr) {
            println!("Unhandled write to expansion 2 Register {:X}", offset);
            return;
        }

        if let Some(offset) = map::RAM.contains(absolute_addr) {
            return self.ram.store8(offset, val);
        }

        panic!("unhandled store8 into address {:08X}", addr)
    }

    pub fn store16(&mut self, addr: u32, val: u16) {
        if addr % 2 != 0 {
            panic!("Unaligned store16 into adress: {:08X}", addr)
        }

        let absolute_addr = mask_region(addr);

        if let Some(offset) = map::SPU.contains(absolute_addr) {
            println!("Unhandled write to SPU Register {:X}", offset);
            return;
        }

        if let Some(offset) = map::TIMERS.contains(absolute_addr) {
            println!("Unhandled write to timer register {:X}", offset);
            return;
        }

        panic!("unhandled store16 into address {:08X}", addr)
    }

    pub fn store32(&mut self, addr: u32, val: u32) {
        let absolute_addr = mask_region(addr);

        if absolute_addr % 4 != 0 {
            panic!("Unaligned store32 absolute_address: {:08x}", absolute_addr);
        }

        if let Some(offset) = map::SYS_CONTROL.contains(absolute_addr) {
            match offset {
                0 => {
                    if val != 0x1f00_0000 {
                        panic!("Bad exception 1 base absolute_address : 0x{:08X}", val)
                    }
                }
                4 => {
                    if val != 0x1f80_2000 {
                        panic!("Bad exception 2 base absolute_address : 0x{:08X}", val)
                    }
                }
                _ => println!("Unhandled write to SYS_CONTROL register"),
            }
            return;
        }

        if let Some(_offset) = map::RAM_SIZE.contains(absolute_addr) {
            println!("Unhandled write to RAM_SIZE register");
            return;
        }

        if let Some(_offset) = map::CACHE_CONTROL.contains(absolute_addr) {
            println!("Unhandled write to CACHE_CONTROL register");
            return;
        }

        if let Some(offset) = map::RAM.contains(absolute_addr) {
            self.ram.store32(offset, val);
            return;
        }

        if let Some(offset) = map::IRQ_CONTROL.contains(absolute_addr) {
            println!("IRQ control: {:X} <- {:08X}", offset, val);
            return;
        }

        panic!(
            "unhandled store32 into absolute_address {:08X}",
            absolute_addr
        );
    }
}
