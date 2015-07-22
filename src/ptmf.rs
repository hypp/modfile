use std::io::Write;
use std::fmt;
use std::cmp;

const DEFAULT_NUMBER_OF_SAMPLES:usize = 31;


#[derive(Debug)]
pub struct SampleInfo {
	pub name: String, // [char; 22],
	pub length: u16, // In words, so multiply by 2
	pub finetune: u8,
	pub volume: u8, // 0-64
	pub repeat_start: u16, // In words from start of sample data, so multiply by 2
	pub repeat_length: u16, // Number of words to loop, multiply by 2
}

impl Default for SampleInfo {
	fn default() -> SampleInfo {
		SampleInfo{name: String::new(), length:0, finetune:0, volume:0, repeat_start:0, repeat_length:0}
	}
}

#[derive(Debug)]
pub struct Channel {
	pub period: u16, // Really u12
	pub sample_number: u8,
	pub effect: u16 // Really u12
}

#[derive(Debug)]
pub struct Row {
	pub channels: Vec<Channel>
}

impl Default for Row {
	fn default() -> Row {
		let mut row = Row{channels: Vec::new()};
		for _ in 0..4 {
			let c = Channel{period: 0, sample_number: 0, effect: 0};
			row.channels.push(c);
		}
		
		row
	}
}

#[derive(Debug)]
pub struct Pattern {
	pub rows: Vec<Row>
}

impl Default for Pattern {
	fn default() -> Pattern {
		let mut p = Pattern{rows: Vec::new()};

		for _ in 0..64 {
			let row:Row = Default::default();
			p.rows.push(row);
		}

		p
	}
}

pub struct Positions {
	pub data: [u8; 128]
}

impl fmt::Debug for Positions {
    fn fmt(&self, formatter: &mut fmt::Formatter) -> fmt::Result {
        self.data[..].fmt(formatter)
    }
}


#[derive(Debug)]
pub struct PTModule {
	pub name: String, // [char; 20],
	pub sample_info: Vec<SampleInfo>, // [SampleInfo; DEFAULT_NUMBER_OF_SAMPLES]
	pub length: u8, // Length 1-128
	pub nt_restart: u8, // Set to 127
	pub positions: Positions, // The order in which to play patterns
	pub mk: [u8; 4], // Set to M.K.
	pub patterns: Vec<Pattern>,
	pub sample_data: Vec<Vec<u8>> // The bytes in the samples -127 - 127
}

impl Default for PTModule {
	fn default() -> PTModule {
		let mut ptmod = PTModule{name: String::new(), sample_info: Vec::new(), length:0, nt_restart: 127, 
			positions: Positions{data: [0; 128]}, mk: ['M' as u8, '.' as u8, 'K' as u8, '.' as u8], patterns: Vec::new(), sample_data: Vec::new() };
			
		for _ in 0..DEFAULT_NUMBER_OF_SAMPLES {
			let si: SampleInfo = Default::default();
			ptmod.sample_info.push(si);
		}
		
		ptmod
	}
}

fn write_or_panic(writer: &mut Write, data: &[u8]) {
	match writer.write_all(&data) {
		Ok(_) => (),
		_ => panic!("Failed to write bytes")
	};
}

fn write_0_padded_string(writer: &mut Write, str: &String, len: usize) {
	let mut data = vec![0u8; len];
	let str_buf = str.as_bytes();
	let m = cmp::min(data.len(),str_buf.len());
	for i in 0..m {
		data[i] = str_buf[i];
	}
	
	write_or_panic(writer, &data);
}

fn write_big_endian_u16(writer: &mut Write, val: u16) {
	let mut data = [0u8; 2];
	data[0] = (val >> 8) as u8;
	data[1] = (val & 0xff) as u8;
	
	write_or_panic(writer, &data);
}

fn write_u8(writer: &mut Write, val: u8) {
	let mut data = [0u8; 1];
	data[0] = val;
	
	write_or_panic(writer, &data);
}

pub fn write_mod(writer: &mut Write, module: &mut PTModule) {

	// First write songname, 20 bytes, pad with 0
	write_0_padded_string(writer,&module.name,20);
	// Then write all 32 samples
	// TODO Handle the case when the vector has less than 31 samples
	let mut num_samples = 0;
	for i in 0..DEFAULT_NUMBER_OF_SAMPLES {
		let ref si = module.sample_info[i];
		// Sample name
		write_0_padded_string(writer, &si.name, 22);
		// Sample length
		write_big_endian_u16(writer, si.length);
		if si.length > 0 {
			num_samples += 1;
		}
		// Finetune
		write_u8(writer, si.finetune);
		// Volume
		write_u8(writer, si.volume);
		// Repeat start
		write_big_endian_u16(writer, si.repeat_start);
		// Repeat length
		write_big_endian_u16(writer, si.repeat_length);
	}
	
	let num_samples = num_samples;
	// Songlength
	write_u8(writer, module.length);
	// nt_restart
	write_u8(writer, module.nt_restart);
	// Song positions
	write_or_panic(writer,&module.positions.data);
	// M.K.
	write_or_panic(writer,&module.mk);
	
	// All patterns
	for pattern_it in module.patterns.iter() {
		// All rows
		for row_it in pattern_it.rows.iter() {
			// All channels
			for channel_it in row_it.channels.iter() {
				let mut data = [0u8; 4];
				
				data[0] = (channel_it.sample_number & 0xf0) | ((channel_it.period & 0xf00) >> 8) as u8;
				data[1] = (channel_it.period & 0xff) as u8;
				data[2] = ((channel_it.sample_number & 0x0f) << 4) | ((channel_it.effect & 0xf00) >> 8) as u8;
				data[3] = (channel_it.effect & 0xff) as u8;
				
				write_or_panic(writer,&data);
			}
		}
	}
	
	// And finally all samples
	for sample_it in module.sample_data.iter() {
		write_or_panic(writer,sample_it);
	}
	
	if num_samples != module.sample_data.len() {
		println!("Warning! Number of samples does not match sample data");
	}
}
