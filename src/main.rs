pub mod cpu;
pub mod bios;
pub mod map;
pub mod interconnect;

use std::path::Path;
use bios::Bios;
use interconnect::Interconnect;
use cpu::Cpu;

fn main() {
    let bios = Bios::new(&Path::new("roms/scph1001.bin")).unwrap();
    let inter = Interconnect::new(bios);
    let mut cpu = Cpu::new(inter);

    loop {
        cpu.run_next_instruction();
    }
}