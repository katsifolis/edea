pub mod bios;
pub mod cpu;
pub mod interconnect;
pub mod map;

use bios::Bios;
use cpu::Cpu;
use interconnect::Interconnect;
use std::path::Path;

fn main() {
    let mut args = std::env::args();

    if args.len() < 2 {
        panic!("Give me the bios!")
    }

    let path = args.nth(1).unwrap();

    let bios = Bios::new(Path::new(&path)).unwrap();

    let inter = Interconnect::new(bios);

    let mut cpu = Cpu::new(inter);

    loop {
        cpu.run_next_instruction();
    }
}
