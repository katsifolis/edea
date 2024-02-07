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

pub const BIOS: Range = Range(0xBFC0_0000, 512 * 1024);
pub const MEMCONTROL: Range = Range(0x1f801000, 36);
