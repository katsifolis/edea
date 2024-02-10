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
pub const RAM_SIZE: Range = Range(0x1f801060, 4);
pub const CACHE_CONTROL: Range = Range(0xfffe0130, 4);
pub const RAM: Range = Range(0xa0000000, 2 * 1024 * 1024);
