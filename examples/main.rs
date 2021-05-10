use std::fs::File;
use std::io::BufWriter;
use std::io::BufReader;
use std::env;

extern crate modfile;
use modfile::ptmf;

fn main() {
	let args : Vec<String> = env::args().collect();

	for i in 1..args.len() {	
		let ref filename = args[i];
		println!("***** {} *****", filename);

		let file = match File::open(filename) {
			Ok(file) => file,
			Err(..) => panic!("Failed to open file"),
		};
		
		let mut reader = BufReader::new(&file);
		let mut module = ptmf::read_mod(&mut reader, true).unwrap();

		println!("Songname: '{}'",module.name);
		for si in &module.sample_info {
			if si.length > 0 {
				println!("Sample name: '{}' length: '{}' repeat start: '{}' repeat length: '{}'", si.name, si.length*2, si.repeat_start*2, si.repeat_length*2);
			}
		}
		
		let mut filename = filename.clone();
		filename.push_str(".copy");

		let file = match File::create(filename) {
			Ok(file) => file,
			Err(..) => panic!("Failed to open file"),
		};

		let mut writer = BufWriter::new(&file);
		ptmf::write_mod(&mut writer, &mut module).unwrap();
		
		println!("***** *****")
	}
}
