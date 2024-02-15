pub struct Range(u32, u32);

impl Range {
    pub fn contains(self, addr: u32) -> Option<u32> {
        let Range(start, length) = self;

        if addr >= start && addr < start + length {
            Some(addr - start)
        } else {
            None
        }
    }
}

const REGION_MASK: [u32; 8] = [
    0xffff_ffff,
    0xffff_ffff,
    0xffff_ffff,
    0xffff_ffff, // KUSEG: 2048MB
    0x7fff_ffff, // KSEG0
    0x1fff_ffff, // KSEG1
    0xffff_ffff, // KSEG2
    0xffff_ffff,
];

pub fn mask_region(addr: u32) -> u32 {
    let idx = (addr >> 29) as usize;
    addr & REGION_MASK[idx]
}

pub const RAM: Range = Range(0x00000000, 2 * 1024 * 1024);
pub const BIOS: Range = Range(0x1FC0_0000, 512 * 1024);
pub const SYS_CONTROL: Range = Range(0x1f801000, 36);
pub const RAM_SIZE: Range = Range(0x1f801060, 4);
pub const CACHE_CONTROL: Range = Range(0xfffe0130, 4);
pub const SPU: Range = Range(0x1f801c00, 640);
pub const EXPANSION_1: Range = Range(0x1f000000, 512 * 1024);
pub const EXPANSION_2: Range = Range(0x1f802000, 66);
pub const IRQ_CONTROL: Range = Range(0x1f801070, 8);
pub const TIMERS: Range = Range(0x1F801100, 0x30);
pub const DMA: Range = Range(0x1F801080, 0x80);
