use std::io::Write;
use std::io::Read;
use std::fmt;
use std::cmp;
use std::io;
use std::num::Wrapping;

const DEFAULT_NUMBER_OF_SAMPLES:usize = 31;
const DEFAULT_NUMBER_OF_ROWS_PER_PATTERN:usize = 64;
const DEFAULT_NUMBER_OF_CHANNELS_PER_ROW:usize = 4;
const MAGIC_MK:[u8; 4] =['M' as u8, '.' as u8, 'K' as u8, '.' as u8];
const MAGIC_MK99:[u8; 4] =['M' as u8, '!' as u8, 'K' as u8, '!' as u8];
const MAGIC_FLT4:[u8; 4] =['F' as u8, 'L' as u8, 'T' as u8, '4' as u8];
const MAGIC_4CHN:[u8; 4] =['4' as u8, 'C' as u8, 'H' as u8, 'N' as u8];
const MAGIC_6CHN:[u8; 4] =['6' as u8, 'C' as u8, 'H' as u8, 'N' as u8];
const MAGIC_8CHN:[u8; 4] =['8' as u8, 'C' as u8, 'H' as u8, 'N' as u8];

///
/// Periods from http://greg-kennedy.com/tracker/modformat.html
///
///          C    C#   D    D#   E    F    F#   G    G#   A    A#   B
/// Octave 1: 856, 808, 762, 720, 678, 640, 604, 570, 538, 508, 480, 453
/// Octave 2: 428, 404, 381, 360, 339, 320, 302, 285, 269, 254, 240, 226
/// Octave 3: 214, 202, 190, 180, 170, 160, 151, 143, 135, 127, 120, 113
///
/// Octave 0:1712,1616,1525,1440,1357,1281,1209,1141,1077,1017, 961, 907
/// Octave 4: 107, 101,  95,  90,  85,  80,  76,  71,  67,  64,  60,  57
///
pub static PERIODS: &'static [u16] = &[
    1712,1616,1525,1440,1357,1281,1209,1141,1077,1017, 961, 907,
    856,  808, 762, 720, 678, 640, 604, 570, 538, 508, 480, 453,
    428,  404, 381, 360, 339, 320, 302, 285, 269, 254, 240, 226,
    214,  202, 190, 180, 170, 160, 151, 143, 135, 127, 120, 113,
	107,  101,  95,  90,  85,  80,  76,  71,  67,  64,  60,  57,
];

pub static NOTE_NAMES: &'static [&'static str] = &["C", "C#", "D", "D#", "E", "F", "F#", "G", "G#", "A", "A#", "B"];

static DELTA_4BIT: &'static [u8] = &[0x00, 0x01, 0x02, 0x04, 0x08, 0x10, 0x20, 0x40, 
									 0x80, 0xc0, 0xe0, 0xf0, 0xf8, 0xfc, 0xfe, 0xff];

pub static EFFECT_NAMES: &'static [&'static str] = &[
		// Normal commads 0-F
		"Arpeggio", "Slide up", "Slide down", "Slide to note",
		"Vibrato", "Slide to note and volume slide", "Vibrato and volume slide", "Tremolo", 
		"undefined", "Set sample offset", "Volume slide", "Position jump",
		"Set volume", "Pattern break", "undefined", "Set speed",
		
		// E command. Subcommand + 16
		"Set filter on/off", "Fineslide up", "Fineslide down", "Set glissando on/off",
		"Set vibrato waveform", "Set finetune value", "Loop pattern", "Set tremolo waveform",
		"Sync effect to music", "Retrigger sample", "Fine volume slide up", "Fine volume slide down",
		"Cut sample", "Delay sample", "Delay pattern", "Invert loop"
		];
						 
/// Custom error enum
#[derive(Debug)]
pub enum PTMFError {
	/// IO errors
	Io(io::Error),
	/// Parse errors
	Parse(String)
}

impl From<io::Error> for PTMFError {
    fn from(err: io::Error) -> PTMFError {
        PTMFError::Io(err)
    }
}

/// Info about each sample
#[derive(Debug)]
pub struct SampleInfo {
	/// Name of sample, 22 characters
	pub name: String, // [char; 22],
	/// Length of sample in words
	pub length: u16, // In words, so multiply by 2
	/// Finetune value -7 to +7
	pub finetune: u8,
	/// Volume 0 to 64
	pub volume: u8, // 0-64
	/// Repeat start in words
	pub repeat_start: u16, // In words from start of sample data, so multiply by 2
	/// Repeat length in words
	pub repeat_length: u16, // Number of words to loop, multiply by 2
	/// The sample data. 8 bit signed.
	pub data: Vec<u8> // The bytes in the samples -127 - 127
}

impl SampleInfo {
	pub fn new() -> SampleInfo {
		SampleInfo{name: String::new(), length:0, finetune:0, volume:0, repeat_start:0, 
			repeat_length:0, data: Vec::new()}
	}
}

/// Data for one channel
#[derive(Debug, Clone)]
pub struct Channel {
	/// The period value used on Amiga for playback. Represents the frequency.
	pub period: u16, // Really u12
	/// Which sample to use when playing. 0 means last used sample.
	pub sample_number: u8,
	/// The effect to use.
	pub effect: u16 // Really u12
}

impl Channel {
	pub fn new() -> Channel {
		Channel{period: 0, sample_number: 0, effect: 0}
	}
}

/// A single row in a Pattern.
/// Normally there are 4 channels per Row.
#[derive(Debug, Clone)]
pub struct Row {
	/// Data for each Channel
	pub channels: Vec<Channel>
}

impl Row {
	pub fn new(num_channels: usize) -> Row {
		let mut row = Row{channels: Vec::new()};
		for _ in 0..num_channels {
			row.channels.push(Channel::new());
		}
		
		row
	}
}

/// A Pattern with multiple Rows and Channels
#[derive(Debug, Clone)]
pub struct Pattern {
	/// Data for each Channel
	pub rows: Vec<Row>
}

impl Pattern {
	pub fn new(num_rows:usize,num_channels:usize) -> Pattern {
		let mut p = Pattern{rows: Vec::new()};

		for _ in 0..num_rows {
			p.rows.push(Row::new(num_channels));
		}

		p
	}
}

/// The order in which to play Patterns
pub struct Positions {
	/// The order in which to play Patterns
	pub data: [u8; 128]
}

impl fmt::Debug for Positions {
    fn fmt(&self, formatter: &mut fmt::Formatter) -> fmt::Result {
        self.data[..].fmt(formatter)
    }
}

/// A complete ProTracker module,
/// including samples, descriptions and patterns
#[derive(Debug)]
pub struct PTModule {
	/// Songname 20 characters
	pub name: String, // [char; 20],
	/// Info about each sample. Usually there are 31 samples.
	pub sample_info: Vec<SampleInfo>, // [SampleInfo; DEFAULT_NUMBER_OF_SAMPLES]
	/// Length of song, i.e. number of patterns to play
	pub length: u8, // Length 1-128
	/// Unknown value. Set it to 127.
	pub nt_restart: u8, // Set to 127
	/// The order in which to play patterns
	pub positions: Positions, // The order in which to play patterns
	/// Magic bytes, either M.K. or FLT4
	pub mk: [u8; 4], // Set to M.K.
	/// The patterns
	pub patterns: Vec<Pattern>,
}

impl PTModule {
	pub fn new() -> PTModule {
		let mut ptmod = PTModule{name: String::new(), sample_info: Vec::new(), length:0, 
			nt_restart: 127, positions: Positions{data: [0; 128]}, mk: MAGIC_MK, 
			patterns: Vec::new() };
			
		for _ in 0..DEFAULT_NUMBER_OF_SAMPLES {
			ptmod.sample_info.push(SampleInfo::new());
		}
		
		ptmod
	}
}

fn read_all(reader: &mut Read, data: &mut [u8]) -> io::Result<usize> {
	let mut pos = 0;
	while pos < data.len() {
		let slice = &mut data[pos..];
		let n = try!(reader.read(slice));
		if n == 0 {
			return Err(io::Error::new(io::ErrorKind::Other, "0 was returned from read"))
		}
		pos += n;
	}
	
	Ok(pos)
}

fn write_all(writer: &mut Write, data: &[u8]) -> io::Result<()> {
	let n = try!{writer.write_all(&data)};
	
	Ok(n)
}

fn read_0_padded_string(reader: &mut Read, len: usize) -> Result<String, io::Error> {
	let mut data = vec![0u8; len];
	try!(read_all(reader,&mut data));
	let mut str = String::new();
	for byte in data {
		str.push(byte as char);
	}

	Ok(str)
}

fn write_0_padded_string(writer: &mut Write, str: &String, len: usize) -> io::Result<()> {
	let mut data = vec![0u8; len];
	let str_buf = str.as_bytes();
	let m = cmp::min(data.len(),str_buf.len());
	for i in 0..m {
		data[i] = str_buf[i];
	}
	
	let n = try!(write_all(writer, &data));
	
	Ok(n)
}

fn read_big_endian_u16(reader: &mut Read) -> Result<u16, io::Error> {
	let mut data_arr = [0u8; 2];
	try!(read_all(reader,&mut data_arr));

	let mut data:u16 = 0;
	
	for n in data_arr.iter() {
		data = (data << 8) + *n as u16;
	}
	
	Ok(data)
}

fn write_big_endian_u16(writer: &mut Write, val: u16)  -> io::Result<()> {
	let mut data = [0u8; 2];
	data[0] = (val >> 8) as u8;
	data[1] = (val & 0xff) as u8;
	
	let n = try!(write_all(writer, &data));
	
	Ok(n)
}

fn read_u8(reader: &mut Read) -> Result<u8, io::Error> {
	let mut data = [0u8; 1];
	
	try!(read_all(reader, &mut data));
	
	Ok(data[0])
}


fn write_u8(writer: &mut Write, val: u8)  -> io::Result<()> {
	let mut data = [0u8; 1];
	data[0] = val;
	
	let n = try!(write_all(writer, &data));
	
	Ok(n)
}

/// Write a 31 sample Amiga ProTracker mod-file
pub fn write_mod(writer: &mut Write, module: &mut PTModule) -> Result<(),PTMFError> {

	// First write songname, 20 bytes, pad with 0
	try!(write_0_padded_string(writer,&module.name,20));
	// Then write all 32 samples
	// TODO Handle the case when the vector has less than 31 samples
	let mut num_samples = 0;
	for i in 0..DEFAULT_NUMBER_OF_SAMPLES {
		let ref si = module.sample_info[i];
		// Sample name
		try!(write_0_padded_string(writer, &si.name, 22));
		// Sample length
		try!(write_big_endian_u16(writer, si.length));
		if si.length > 0 {
			num_samples += 1;
		}
		// Finetune
		try!(write_u8(writer, si.finetune));
		// Volume
		try!(write_u8(writer, si.volume));
		// Repeat start
		try!(write_big_endian_u16(writer, si.repeat_start));
		// Repeat length
		try!(write_big_endian_u16(writer, si.repeat_length));
	}
	
	let num_samples = num_samples;
	// Songlength
	try!(write_u8(writer, module.length));
	// nt_restart
	try!(write_u8(writer, module.nt_restart));
	// Song positions
	try!(write_all(writer,&module.positions.data));
	// M.K.
	try!(write_all(writer,&module.mk));
	
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
				
				try!(write_all(writer,&data));
			}
		}
	}
	
	// And finally all samples
	let mut num_written = 0;
	for sample_it in module.sample_info.iter().filter(|si| si.length > 0) {
		try!(write_all(writer,&sample_it.data));
		num_written += 1;
	}
	
	if num_samples != num_written {
		return Err(PTMFError::Parse(format!("Warning! Number of samples '{}' does not match sample data '{}'", num_samples, num_written)));
	}
	
	Ok(())
}

/// Read a 31 sample Amiga ProTracker mod-file
pub fn read_mod(reader: &mut Read, ignore_file_size_check: bool) -> Result<PTModule, PTMFError> {
	let mut module = PTModule::new();

	// First read 20 bytes songname
	module.name = try!(read_0_padded_string(reader, 20));

	// Read all sample info
	// TODO Handle 15 sample files
	for i in 0..DEFAULT_NUMBER_OF_SAMPLES {
		let si = &mut module.sample_info[i];
		// Sample name
		si.name = try!(read_0_padded_string(reader, 22));
		si.length = try!(read_big_endian_u16(reader));
		// Finetune
		si.finetune = try!(read_u8(reader));
		// Volume
		si.volume = try!(read_u8(reader));
		// Repeat start
		si.repeat_start = try!(read_big_endian_u16(reader));
		// Repeat length
		si.repeat_length = try!(read_big_endian_u16(reader));
	}
	
	// Songlength
	module.length = try!(read_u8(reader));
	// nt_restart
	module.nt_restart = try!(read_u8(reader));
	// Song positions
	try!(read_all(reader,&mut module.positions.data));
	// M.K.
	try!(read_all(reader,&mut module.mk));
	if module.mk != MAGIC_MK &&
		module.mk != MAGIC_MK99 &&
		module.mk != MAGIC_FLT4 &&
		module.mk != MAGIC_4CHN &&
		module.mk != MAGIC_6CHN &&
		module.mk != MAGIC_8CHN {
		return Err(PTMFError::Parse(format!("Unknown format {:?}", module.mk)));
	}
	let num_channels = 
		if  module.mk == MAGIC_6CHN {
			6
		} else if  module.mk == MAGIC_8CHN {
			8
		} else {
			4
		}
	;

	// Read all patterns
	let mut num_patterns = 0;
	for n in module.positions.data.iter() {
		num_patterns = cmp::max(num_patterns,*n);
	}
	num_patterns += 1;
	for _ in 0..num_patterns {
		let mut pattern = Pattern::new(DEFAULT_NUMBER_OF_ROWS_PER_PATTERN,num_channels);
		for row in &mut pattern.rows {
			for channel in &mut row.channels {
				let mut data  = [0u8;4];
				try!(read_all(reader,&mut data));
				channel.sample_number = (data[0] & 0xf0) | ((data[2] & 0xf0) >> 4);
				channel.period = (((data[0] & 0x0f) as u16) << 8) | (data[1] as u16);
				channel.effect = (((data[2] & 0x0f) as u16) << 8) | (data[3] as u16);
			}
		}
		module.patterns.push(pattern);
	}
	
	// Read all samples
	for si in &mut module.sample_info {
		if si.length > 0 {
			let length_in_bytes = si.length * 2;
			let mut data = vec![0u8; length_in_bytes as usize];
			try!(read_all(reader,&mut data));
			si.data = data;
		}
	}
	
	// Now we have read all data. 
	if !ignore_file_size_check {
		// Sanity check that the reader is empty
		let mut data = [0u8; 1];
		match reader.read(&mut data) {
			Ok(n) if n == data.len() => return Err(PTMFError::Parse(format!("Unread data left in file"))),
			_ => ()
		};
	}

	Ok(module)
}

fn decode_p61_effect(effect_type:u8, params: u8) -> (bool, u16) {

	let effect:u16;
	let mut eop = false;

	let effect_type = effect_type & 0x0f;
	
	if (effect_type == 0x5 || effect_type == 0x6 || 
		effect_type == 0xa) && (params & 0x80 == 0x80) {
		// P61con changes two nibble commands to signed/unsigned
		// Let's change it back
		let fixed_params = (0x100 - params as u16) << 4;
		
		effect = ((effect_type as u16) << 8) | (fixed_params as u16);

	} else if effect_type == 0x8 {
		// P61con changes effect 0 to 8
		// Let's change it back
		effect = params as u16;		

	} else if effect_type == 0xb {
		// End
		effect = ((effect_type as u16) << 8) | (params as u16);
		eop = true;
		
	} else if effect_type == 0xd {
		// End
		effect = ((effect_type as u16) << 8) | (params as u16);
		eop = true;

	} else {
		effect = ((effect_type as u16) << 8) | (params as u16);
	}

	(eop, effect)
}

/// Decode at least one row
/// Returns true if the pattern ends here due to effect 0xd or 0xb
fn decode_p61_row(data: &Vec<u8>, current_pos: &mut usize, pattern_number: usize, row_number: &mut usize, channel_number: usize, module: &mut PTModule) -> bool {
	let mut end_of_pattern = false;

	let first_byte = data[*current_pos];
	*current_pos += 1;
	
	// If first bit is set, has compression info
	let has_compression_info = first_byte & 0x80 == 0x80;

	// Is empty?
	let is_empty = first_byte & 0x7f == 0x7f;
	// Only an effect, no note or instrument
	let is_only_effect = first_byte & 0b01110000 == 0b01100000;
	// No effect, but note and instrument
	let is_no_effect = first_byte & 0b01111000 == 0b01110000;
		
	if is_empty {
		*row_number += 1;
	} else if is_only_effect {
		// Only effect command
		// One more byte
		let second_byte = data[*current_pos];
		*current_pos += 1;
		
		let (eop, effect) = decode_p61_effect(first_byte & 0x0f, second_byte);
		if eop && has_compression_info {
			panic!("Should never happen");
		}
		end_of_pattern = eop;
				
		let channel = &mut module.patterns[pattern_number].rows[*row_number].channels[channel_number];
		channel.effect = effect;
		
		*row_number += 1;		
	} else if is_no_effect {
		// Note and instrument, but no effect
		let second_byte = data[*current_pos];
		*current_pos += 1;

		let noteno = (((first_byte & 0b00000111) << 3) | ((second_byte & 0b11100000) >> 5)) as usize;
		let instrument = second_byte & 0b00011111;

		let period = if noteno == 0 {
			0
		} else {
			PERIODS[noteno+11]
		};
					
		let channel = &mut module.patterns[pattern_number].rows[*row_number].channels[channel_number];
		channel.period = period;
		channel.sample_number = instrument;
		*row_number += 1;
		
	} else {
		// Full command
		// Two more bytes
		let second_byte = data[*current_pos];
		*current_pos += 1;

		let third_byte = data[*current_pos];
		*current_pos += 1;
		
		let noteno = (first_byte & 0b01111110) as usize >> 1;
		let instrument = ((first_byte & 0b00000001) << 4) | ((second_byte & 0b11110000) >> 4);

		let (eop, effect) = decode_p61_effect(second_byte & 0x0f, third_byte);
		if eop && has_compression_info {
			panic!("Should never happen");
		}
		end_of_pattern = eop;
		
		let period = if noteno == 0 {
			0
		} else {
			PERIODS[noteno+11]
		};
		
		let channel = &mut module.patterns[pattern_number].rows[*row_number].channels[channel_number];
		channel.period = period;
		channel.sample_number = instrument;
		channel.effect = effect;
		
		*row_number += 1;
	}
	
	if has_compression_info {
		let first_byte = data[*current_pos];
		*current_pos += 1;
		
		let ctype = first_byte & 0b11000000;
		if ctype == 0b00000000 {
			// Empty rows
			let num_empty_rows = first_byte & 0b00111111;
			*row_number += num_empty_rows as usize;
		} else if ctype  == 0b10000000 {
			// Repeat current row n times
			let repeat_count = first_byte & 0b00111111;
			let channel = module.patterns[pattern_number].rows[*row_number-1].channels[channel_number].clone();
			for _ in 0..repeat_count {
				let new_channel = &mut module.patterns[pattern_number].rows[*row_number].channels[channel_number];
				new_channel.period = channel.period;
				new_channel.sample_number = channel.sample_number;
				new_channel.effect = channel.effect;
				
				*row_number += 1;
			}
		} else if ctype == 0b01000000 || ctype == 0b11000000 {
			// Copy previous data from offset
			let copy_offset:u16;
			
			if is_empty {
				// Special handling when empty ... WTF
				*row_number -= 1;
			}
			
			let repeat_count = (first_byte & 0b00111111) + 1;
			
			if ctype == 0b01000000 {
				// Copy previous data from 8-bit offset
				let second_byte = data[*current_pos] as u16;
				*current_pos += 1;
				
				copy_offset = second_byte;
			
			} else {
				// Copy previous data from 16-bit offset
				let second_byte = data[*current_pos] as u16;
				*current_pos += 1;

				let third_byte = data[*current_pos] as u16;
				*current_pos += 1;
				
				copy_offset = (second_byte << 8) | third_byte;
			}
			
			let mut new_pos = *current_pos - copy_offset as usize;

			// Recurse
			for n in 0..repeat_count {
				let eop = decode_p61_row(&data, &mut new_pos, pattern_number, row_number, channel_number, module);
				if eop {
					end_of_pattern = eop;
					if n != repeat_count-1 {
						panic!("Should never happen {} {}",n,repeat_count);
					}
				}
			}
		} else {
			panic!("Should never happen!");
		}		
	}
	
	end_of_pattern
}

/// Read an Amiga ProTracker file packed with The Player 6.1
pub fn read_p61(reader: &mut Read) -> Result<PTModule, PTMFError> {
	let mut module = PTModule::new();
	
	// Read the entire file to a Vec<u8>
	// since the format uses a lot of offsets
	// back and forth in the file.
	let mut data:Vec<u8> = Vec::new();
	let data_size = try!(reader.read_to_end(&mut data));
	if data.len() != data_size {
		return Err(PTMFError::Parse(format!("Failed to read all bytes {} {}", data.len(), data_size)));	
	}
	
	let mut pos = 0;

	if data[0] == 'P' as u8 && 
		data[1] == '6' as u8 &&
		data[2] == '1' as u8 &&
		data[3] == 'A' as u8 {
		// Remove header
		data.remove(0);
		data.remove(0);
		data.remove(0);
		data.remove(0);
	}

	let sample_offset = ((data[pos] as u16) << 8) | data[pos+1] as u16;
	let num_patterns = data[pos+2];
	let num_samples = data[pos+3];
	pos += 4;
	
	let is_delta_8_bit = num_samples & 0x80 == 0x80;
	let is_delta_4_bit = num_samples & 0x40 == 0x40;
	let num_samples = num_samples & 0b00111111;
	
	let mut unpacked_samples_length:u32 = 0; 
	if is_delta_4_bit {
		for _ in 0..4 {
			unpacked_samples_length = (unpacked_samples_length << 8) + data[pos] as u32;
			pos += 1;
		}
	}
	
	let mut sample_start = sample_offset as usize;
	for i in 0..num_samples as usize {
		let sample_length = ((data[pos] as u16) << 8) | data[pos+1] as u16;
		let finetune = data[pos+2];
		let volume = data[pos+3];
		let mut repeat_start = ((data[pos+4] as u16) << 8) | data[pos+5] as u16;
		let mut repeat_length = 0;
		if repeat_start == 0xffff {
			repeat_start = 0;
		} else {
			repeat_length = sample_length - repeat_start;
		}
				
		let signed_sample_length = sample_length as i16;
		if signed_sample_length < 0 && signed_sample_length >= -31 {
			// This means that the sample uses the same sample data as
			// another sample
			let sample_index = (-1 * signed_sample_length - 1) as usize;
			
			let length = module.sample_info[sample_index].length;
			let data = module.sample_info[sample_index].data.clone();

			// Fix repeat length
			if repeat_start == 0xffff {
				repeat_start = 0;
			} else {
				repeat_length = length - repeat_start;
			}
			
			let si = &mut module.sample_info[i];
			si.length = length;
			si.finetune = finetune & 0x0f;
			si.volume = volume;
			si.repeat_start = repeat_start;
			si.repeat_length = repeat_length;
			si.data = data;

		} else {
		
			let is_sample_delta_4_bit = finetune & 0x80 == 0x80;
		
			let si = &mut module.sample_info[i];
			si.length = sample_length;
			si.finetune = finetune & 0x0f;
			si.volume = volume;
			si.repeat_start = repeat_start;
			si.repeat_length = repeat_length;

			// 4-bit delta can be enabled/disabled per sample, bit 7 of finetune (0x80)
			// It is possible to combine 8-bit and 4-bit delta
			if is_delta_4_bit && is_sample_delta_4_bit {
				let sample_end = sample_start + sample_length as usize; // Packed length is half of unpacked length
				let mut delta:u8 = 0;
				for i in sample_start..sample_end {
					let hi = ((data[i] & 0xf0) >> 4) as usize;
					let lo = (data[i] & 0x0f) as usize;
					let subhi = DELTA_4BIT[hi];
					delta = (Wrapping(delta) - Wrapping(subhi)).0;
					si.data.push(delta);
					let sublo = DELTA_4BIT[lo];
					delta = (Wrapping(delta) - Wrapping(sublo)).0;
					si.data.push(delta);				
				}
				
				// Move to next sample
				sample_start = sample_end;
				
			} else if is_delta_8_bit {
				let sample_end = sample_start + (sample_length as usize) * 2;
				let mut delta:u8 = data[sample_start];
				// First byte get copied unmodified
				si.data.push(delta);
				
				for i in sample_start+1..sample_end {
					delta = (Wrapping(delta) - Wrapping(data[i])).0;
					si.data.push(delta);
				}
				
				// Move to next sample
				sample_start = sample_end;
			
			} else {
				let sample_end = sample_start + (sample_length as usize) * 2;
				let sample_data = &data[sample_start..sample_end];
				si.data = sample_data.to_vec();

				// Move to next sample
				sample_start = sample_end;
				
			}
		}
		
		// Move to next sample
		pos = pos+6;
	}
	
	let mut pattern_offsets:Vec<usize> = Vec::new();
	for _ in 0..num_patterns {
		module.patterns.push(Pattern::new(DEFAULT_NUMBER_OF_ROWS_PER_PATTERN, DEFAULT_NUMBER_OF_CHANNELS_PER_ROW));
		for _ in 0..DEFAULT_NUMBER_OF_CHANNELS_PER_ROW {
			let offset = (((data[pos] as u16) << 8) | data[pos+1] as u16) as usize;
			pattern_offsets.push(offset);
			
			pos += 2;
		}
	}
	
	let mut length = 0;
	loop {
		let position = data[pos];
		if position == 0xff {
			break;
		}
		module.positions.data[length] = position;
		
		length += 1;
		pos += 1;
	}
	
	pos += 1;

	module.length = length as u8;
	
	let pattern_start_offset = pos;
	
	// Pop removes from the end of Vec
	pattern_offsets.reverse();
	
	for pattern_number in 0..num_patterns as usize {
	
		let mut row_number = [0usize; DEFAULT_NUMBER_OF_CHANNELS_PER_ROW];
		let mut current_pos = [0usize; DEFAULT_NUMBER_OF_CHANNELS_PER_ROW];
		
		for channel_number in 0..DEFAULT_NUMBER_OF_CHANNELS_PER_ROW as usize {
			row_number[channel_number] = 0;
			current_pos[channel_number] = pattern_start_offset + pattern_offsets.pop().unwrap();
		}
		
		let mut truncate_pos = DEFAULT_NUMBER_OF_ROWS_PER_PATTERN;
		loop {
			
			// We always need to decode the channel with the lowest row number first
			// to see if it contains a pattern break or jump
			
			let mut current_channel = 0;
			let mut lowest = DEFAULT_NUMBER_OF_ROWS_PER_PATTERN;
			for channel_number in 0..DEFAULT_NUMBER_OF_CHANNELS_PER_ROW as usize {
				if row_number[channel_number] < lowest {
					lowest = row_number[channel_number];
					current_channel = channel_number;
				}
			}
			
			if lowest >= truncate_pos {
				break;
			}

//			println!("P {} C {} R {} P {:X}",pattern_number,current_channel,row_number[current_channel],current_pos[current_channel]);
			let eop = decode_p61_row(&data, &mut current_pos[current_channel], pattern_number, &mut row_number[current_channel], current_channel, &mut module);				
			if eop {
				// This will make sure we exit the while loop early
				// but still process any remaining channels
				truncate_pos = row_number[current_channel];
			}
						
		} // loop
		
		// If we exit the while loop early above, make sure that we clear out any data
		// that we parsed but shouldn't really be there
		for row_number in truncate_pos..DEFAULT_NUMBER_OF_ROWS_PER_PATTERN as usize {
			for channel_number in 0..DEFAULT_NUMBER_OF_CHANNELS_PER_ROW as usize {
				let channel = &mut module.patterns[pattern_number].rows[row_number].channels[channel_number];
				channel.period = 0;
				channel.sample_number = 0;
				channel.effect = 0;
			}
		}		
	}

	Ok(module)
}

/// Encode one Channel data into p61 format 
fn encode_p61_channel(channel: &Channel) -> u32 {

	let mut note_index = 0;
	for i in 0..PERIODS.len() {
		if PERIODS[i] == channel.period {
			note_index = (i - 11) as u32;
			break;
		}
	}
	
	let note_index = (note_index & 0b111111) as u32; 
	let sample_number = (channel.sample_number & 0b11111) as u32;
	let mut effect = (channel.effect & 0b111111111111) as u32;
	let cmd = (effect & 0xf00) >> 8 as u8;
	let params = effect & 0xff;
	let param1 = (effect & 0xf0) >> 4;
	let param2 = effect & 0xf;
	
	// Some effects must be rewritten
	if cmd == 0xE {
		// E commands are special, handled below
		match param1 {
			0 => effect = (effect & 0xff) | ((param2 & 1) * 2),
			1 | 2 | 9 | 0xA | 0xB | 0xD | 0xE if param2 == 0 => effect = 0, // No effect if empty paramters,
			3 | 4 | 5 | 6 | 7 | 8 | 0xF => (),
			0xC if param2 == 0 => effect = 0xC00, // Replace with empty volume
			_ => ()
		}
		
	} else {
		match cmd {
			0 if params != 0 => effect |= 0x800,	// Effect 0 replaced with 8
			1 | 2 | 0xA if params == 0 => effect = 0, // No effect if parameters empty
			3 | 4 | 7 | 9 | 0xF => (),
			5 if params == 0 => effect = 0x300,
			6 if params == 0 => effect = 0x400,
			5 | 6 | 0xA if params != 0 && param1 == 0 => effect &= 0xf0f,
			5 | 6 | 0xA if params != 0 && param1 != 0 => {
				let p = 0 - param2;
				effect = (effect & 0xf00) | (p & 0xff);
			},
			8 => effect = 0xe80 | param2, // Effect 8 replaced with E8
			0xB => (), // TODO Handle break
			0xC if params > 64 => effect = 0xC40,
			0xD => (), // TODO Handle jump
			_ => ()
		}

	}
	
	let p61 = (note_index << 25) | (sample_number << 20) | (effect << 8);
//	println!("P: {:X} Note: {:b}", p61,p61);
	p61
}


/// Write a 31 sample Amiga ProTracker mod-file as if packed with The Player
pub fn write_p61(writer: &mut Write, module: &mut PTModule) -> Result<(),PTMFError> {

	// TODO 
	// Magic bytes P61A
	// Offset to samples
	// Number of patterns
	// Number of samples + bits for sample packing
	// Optional unpacked sample length

	// For each sample
	// Sample length
	// Finetune + bit for sample packing
	// Volume
	// Repeat start
	
	// For each pattern and each channel
	// Offset to channel data start
	
	// Pattern play order
	// Byte sequence terminated by $ff
	
	// Pattern data
	
	// Loop through all patterns channel by channel
	// Encode as The Player, keep track of pattern breaks and pattern jumps
	
	let mut encoded_patterns:Vec<u32> = Vec::new();
	let mut encoded_offsets:Vec<usize> = Vec::new();
	for pattern_number in 0..module.patterns.len() {
	
		// Check for pattern break / pattern jump
		let mut break_pos = DEFAULT_NUMBER_OF_ROWS_PER_PATTERN;
		for row_number in 0..break_pos {
		
			let mut found_break = false;
			for channel_number in 0..DEFAULT_NUMBER_OF_CHANNELS_PER_ROW {
				let effect = module.patterns[pattern_number].rows[row_number].channels[channel_number].effect & 0xf00;
				if effect == 0xB00 || effect == 0xD00 {
					println!("E: {:X}", effect);
					if found_break {
						// Remove effect
						module.patterns[pattern_number].rows[row_number].channels[channel_number].effect = 0;
					} else {
						found_break = true;
						break_pos = row_number + 1;
						println!("Break: {}",break_pos);
					}
				}
			}
		}

		// Encode
		for channel_number in 0..DEFAULT_NUMBER_OF_CHANNELS_PER_ROW {
			let channel_offset = encoded_patterns.len();
			encoded_offsets.push(channel_offset);
			
			let mut first = true;
			for row_number in 0..break_pos {
//				println!("Data: {:?}", pattern_it.rows[row_number].channels[channel_number]);
				let current = encode_p61_channel(&module.patterns[pattern_number].rows[row_number].channels[channel_number]);
				if first {
					// First time only
					first = false;
					encoded_patterns.push(current);
					continue;
				}
			
				// Check if we can add compression info
				let last_index = encoded_patterns.len()-1;
				let previous = encoded_patterns[last_index];
				let has_compression_info = (0x80000000 & previous) > 0;
				let compression_num_empty_row = (previous & 0b11000000) == 0;
				let compression_num_repeat_row = (previous & 0b11000000) == 0b10000000;
				let same_row = (current & 0x7fffffff) == (previous & 0x7fffffff);
				let is_empty = current == 0;
				
				match (has_compression_info,compression_num_empty_row,compression_num_repeat_row,same_row,is_empty) {
					(false,_,false,_,true) => {
						encoded_patterns[last_index] |= 0x80000000 | 0b00000001;
						println!("f t f f t");
					},
					(true,true,false,_,true) => {
						encoded_patterns[last_index] = (previous & 0xffffff00) | ((previous + 1) & 0xff);
						println!("t t f f t");
					},
					(false,_,false,true,false) => {
						encoded_patterns[last_index] |= 0x80000000 | 0b10000001;
						println!("f f t t f");
					},
					(true,false,true,true,false) => {
						encoded_patterns[last_index] = (previous & 0xffffff00) | ((previous + 1) & 0xff);
						println!("t f t t f");
					},
					_ => {
						encoded_patterns.push(current);
						println!("L: {} P: {:X} {:b} C: {:X} {:b}",last_index,encoded_patterns[last_index],encoded_patterns[last_index],current,current);
					}
				}
								
			} // for row...
		}	// for channel...
		break;
	} // pattern

	println!("Offsets: {:?}",encoded_offsets);
	println!("Data: {:?}",encoded_patterns);

	// Sample data
	
	Ok(())
}
