use std::io::Write;
use std::io::Read;
use std::fmt;
use std::cmp;
use std::io;
use std::num::Wrapping;
use serde::{Serialize, Deserialize};


use serde_big_array::big_array;

big_array! { BigArray; }

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
#[derive(Debug, PartialEq, Clone, Serialize, Deserialize)]
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
#[derive(Debug, PartialEq, Clone, Serialize, Deserialize)]
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

	pub fn is_empty(&self) -> bool {
		self.period == 0 && self.sample_number == 0 && self.effect == 0
	}
}

/// A single row in a Pattern.
/// Normally there are 4 channels per Row.
#[derive(Debug, PartialEq, Clone, Serialize, Deserialize)]
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
#[derive(Debug, PartialEq, Clone, Serialize, Deserialize)]
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
#[derive(Clone, PartialEq, Serialize, Deserialize)]
pub struct Positions {
	/// The order in which to play Patterns
	#[serde(with = "BigArray")]
	pub data: [u8; 128]
}

impl fmt::Debug for Positions {
    fn fmt(&self, formatter: &mut fmt::Formatter) -> fmt::Result {
        self.data[..].fmt(formatter)
    }
}

/// A complete ProTracker module,
/// including samples, descriptions and patterns
#[derive(Debug, PartialEq, Clone, Serialize, Deserialize)]
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
	/// Number of channels per row
	pub num_channels: usize,
	/// The patterns
	pub patterns: Vec<Pattern>,
}

impl PTModule {
	pub fn new() -> PTModule {
		let ptmod = PTModule{name: String::new(), sample_info: Vec::new(), length:0, 
			nt_restart: 127, positions: Positions{data: [0; 128]}, mk: MAGIC_MK,
			num_channels: DEFAULT_NUMBER_OF_CHANNELS_PER_ROW, patterns: Vec::new() };
			
		ptmod
	}

	pub fn find_unused_samples(&self) -> Vec<u8> {
		let mut unused:Vec<u8> = Vec::new();
		let mut used = [0u8;32];
	
		// Find all used samples
		for pattern_no in 0..self.patterns.len() {
			let ref pattern = self.patterns[pattern_no];
			for row_no in 0..pattern.rows.len() {
				let ref row = pattern.rows[row_no];
				for channel_no in 0..row.channels.len() {
					let ref channel = row.channels[channel_no];
					let number = channel.sample_number as usize;
					if number > 0 {
						if number > 31 {
							println!("Error: Invalid sample number in Pattern '{}' Row '{}' Channel '{}' Sample number '{}'",pattern_no,row_no,channel_no,number);
						} else {
							used[number] = 1;
						}
					}				
				}
			}
		}
	
		// Find all unused samples
		for i in 1..self.sample_info.len()+1 {
			if used[i] == 0 {
				unused.push(i as u8);
			}
		}
		
		unused
	}
	
	pub fn remove_unused_samples(&mut self) {
		let mut unused = self.find_unused_samples();
		// MUST remove highest sample first
		unused.sort();
		unused.reverse();
	
		for i in unused {
			let index = i as usize - 1;
			
			// Remove sample info
			self.sample_info.remove(index);
				
			// Rewrite instrument references
			// TODO optimize this
			for pattern in &mut self.patterns {
				for row in &mut pattern.rows {
					for channel in &mut row.channels {
						let number = channel.sample_number;
						if number > i {
							channel.sample_number -= 1;
						}				
					}
				}
			}
		}
	}

	pub fn find_unused_patterns(&self) -> Vec<u8> {
		let mut unused:Vec<u8> = Vec::new();
		let positions = &self.positions.data[0..self.length as usize];
		let num_patterns = self.patterns.len();
		for i in 0..num_patterns as u8 {
			if !positions.contains(&i) {
				unused.push(i);
			}
		}
	
		unused
	}

	fn remove_pattern(&mut self, idx:u8) {
		// Remove pattern
		self.patterns.remove(idx as usize);
	
		// Adjust play positions
		for j in 0..self.positions.data.len() {
			let j = j as usize;
			if self.positions.data[j] > idx {
				self.positions.data[j] -= 1;
			}
		}
	}

	pub fn remove_unused_patterns(&mut self) {
		let mut unused = self.find_unused_patterns();
		unused.reverse();
		
		// MUST Remove highest pattern first
		for i in unused {
			self.remove_pattern(i);
		}
	}

	pub fn find_duplicate_patterns(&self) -> Vec<(u8,u8)> {
		let mut duplicate:Vec<(u8,u8)> = Vec::new();

		let num_patterns = self.patterns.len();
		for i in 0..num_patterns {
			let current_pattern = &self.patterns[i];
			for j in i+1..num_patterns {
				let next_pattern = &self.patterns[j];
				if current_pattern == next_pattern {
					duplicate.push((i as u8,j as u8));
				} 
			}
		}

		duplicate
	}

	pub fn remove_duplicate_patterns(&mut self) {
		let duplicates = self.find_duplicate_patterns();
		let mut removed:Vec<u8> = Vec::new();

		for pair in duplicates {
			let (src, dst) = pair;

			// Adjust play positions
			for i in 0..self.positions.data.len() {
				if self.positions.data[i] == dst {
					self.positions.data[i] = src;
				}
			}

			if !removed.contains(&dst) {
				removed.push(dst);
			}
		}

		// Must remove highest pattern first
		removed.sort();
		removed.reverse();

		for idx in removed {
			self.remove_pattern(idx);
		}
	}

	pub fn find_duplicate_samples(&self) -> Vec<(u8,u8)> {
		let mut duplicate:Vec<(u8,u8)> = Vec::new();

		for i in 0..self.sample_info.len() {
			let current_sample = &self.sample_info[i];
			for j in i+1..self.sample_info.len() {
				let next_sample = &self.sample_info[j];
				// We only compare some of the fields
				if current_sample.data == next_sample.data &&
					current_sample.length == next_sample.length &&
					current_sample.repeat_start == next_sample.repeat_start &&
					current_sample.repeat_length == next_sample.repeat_length &&
					current_sample.volume == next_sample.volume &&
					current_sample.finetune == next_sample.finetune {
					duplicate.push((i as u8,j as u8));
				}
			}
		}

		duplicate
	}

	pub fn remove_duplicate_samples(&mut self) {
		let duplicates = self.find_duplicate_samples();
		let mut removed:Vec<u8> = Vec::new();

		for pair in duplicates {
			let (src, dst) = pair;
			// sample numbers in mod starts at 1
			// 0 is used for no sample
			let src1 = src+1;
			let dst1 = dst+1;

			// Rewrite instrument references
			// TODO optimize this
			for pattern in &mut self.patterns {
				for row in &mut pattern.rows {
					for channel in &mut row.channels {
						if channel.sample_number == dst1 {
							channel.sample_number = src1;
						}				
					}
				}
			}

			if !removed.contains(&dst) {
				removed.push(dst);
			}
		}

		// Must remove highest sample first
		removed.sort();
		removed.reverse();

		for idx in removed {
			// Remove sample info
			self.sample_info.remove(idx as usize);
		}
	}

	pub fn truncate_samples(&mut self) {
		for si in &mut self.sample_info {
			// Check if this is a looping sample
			if si.repeat_length > 1 || si.repeat_start > 0 {
				// The Player truncates samples to repeat_end
				// which is repeat_start + repeat_length
				// repeat_end is in words, so convert to bytes
				let repeat_end = (2*(si.repeat_start+si.repeat_length)) as usize;
				if si.data.len() < repeat_end {
					println!("Error: repeat end '{}' is greater then sample length '{}' Sample '{}'",repeat_end,si.data.len(),si.name);
				}

				si.data.truncate(repeat_end);
			}
		}
	}

	pub fn truncate_patterns(&mut self) {
		for pattern in &mut self.patterns {
			let mut found = false;
			for i in 0..pattern.rows.len() {
				for channel in &pattern.rows[i].channels {
					let effect = (channel.effect & 0xf00) >> 8;
					if effect == 0xd || effect == 0xb {
						found = true;
						break;
					}
				}
				// Did we find a break?
				if found {
					pattern.rows.truncate(i+1);
					break;
				}
			} // for rows
		} // for patterns
	}
}

fn read_all(reader: &mut dyn Read, data: &mut [u8]) -> io::Result<usize> {
	let mut pos = 0;
	while pos < data.len() {
		let slice = &mut data[pos..];
		let n = reader.read(slice)?;
		if n == 0 {
			return Err(io::Error::new(io::ErrorKind::Other, "0 was returned from read"))
		}
		pos += n;
	}
	
	Ok(pos)
}

fn write_all(writer: &mut dyn Write, data: &[u8]) -> io::Result<()> {
	let n = {writer.write_all(&data)}?;
	
	Ok(n)
}

fn read_0_padded_string(reader: &mut dyn Read, len: usize) -> Result<String, io::Error> {
	let mut data = vec![0u8; len];
	read_all(reader,&mut data)?;
	let mut str = String::new();
	for byte in data {
		str.push(byte as char);
	}

	Ok(str)
}

fn write_0_padded_string(writer: &mut dyn Write, str: &String, len: usize) -> io::Result<()> {
	let mut data = vec![0u8; len];
	let str_buf = str.as_bytes();
	let m = cmp::min(data.len(),str_buf.len());
	for i in 0..m {
		data[i] = str_buf[i];
	}
	
	let n = write_all(writer, &data)?;
	
	Ok(n)
}

fn read_big_endian_u16(reader: &mut dyn Read) -> Result<u16, io::Error> {
	let mut data_arr = [0u8; 2];
	read_all(reader,&mut data_arr)?;

	let mut data:u16 = 0;
	
	for n in data_arr.iter() {
		data = (data << 8) + *n as u16;
	}
	
	Ok(data)
}

fn write_big_endian_u16(writer: &mut dyn Write, val: u16)  -> io::Result<()> {
	let mut data = [0u8; 2];
	data[0] = (val >> 8) as u8;
	data[1] = (val & 0xff) as u8;
	
	let n = write_all(writer, &data)?;
	
	Ok(n)
}

fn read_u8(reader: &mut dyn Read) -> Result<u8, io::Error> {
	let mut data = [0u8; 1];
	
	read_all(reader, &mut data)?;
	
	Ok(data[0])
}


fn write_u8(writer: &mut dyn Write, val: u8)  -> io::Result<()> {
	let mut data = [0u8; 1];
	data[0] = val;
	
	let n = write_all(writer, &data)?;
	
	Ok(n)
}

/// Write a 31 sample Amiga ProTracker mod-file
pub fn write_mod(writer: &mut dyn Write, module: &PTModule) -> Result<(),PTMFError> {

	// First write songname, 20 bytes, pad with 0
	write_0_padded_string(writer,&module.name,20)?;
	// Then write all 31 samples
	let mut num_sample_info = 0;
	let mut num_samples = 0;
	for si in &module.sample_info {
		// Sample name
		write_0_padded_string(writer, &si.name, 22)?;
		// Sample length
		write_big_endian_u16(writer, si.length)?;
		if si.length > 0 {
			num_samples += 1;
		}
		// Finetune
		write_u8(writer, si.finetune)?;
		// Volume
		write_u8(writer, si.volume)?;
		// Repeat start
		write_big_endian_u16(writer, si.repeat_start)?;
		// Repeat length
		write_big_endian_u16(writer, si.repeat_length)?;
		num_sample_info += 1;
	}
	// write extra empty sampleinfo to make it 31
	let si = SampleInfo::new();
	for _ in 0..DEFAULT_NUMBER_OF_SAMPLES-module.sample_info.len() {
		// Sample name
		write_0_padded_string(writer, &si.name, 22)?;
		// Sample length
		write_big_endian_u16(writer, si.length)?;
		// Finetune
		write_u8(writer, si.finetune)?;
		// Volume
		write_u8(writer, si.volume)?;
		// Repeat start
		write_big_endian_u16(writer, si.repeat_start)?;
		// Repeat length
		write_big_endian_u16(writer, si.repeat_length)?;
		num_sample_info += 1;
	}
	if num_sample_info != DEFAULT_NUMBER_OF_SAMPLES {
		println!("Failed len {} required {} written {}",module.sample_info.len(),DEFAULT_NUMBER_OF_SAMPLES,num_sample_info);
		return Err(PTMFError::Parse(format!("Error! Wrong number of sample info '{}'", num_sample_info)));
	}
	
	let num_samples = num_samples;
	// Songlength
	write_u8(writer, module.length)?;
	// nt_restart
	write_u8(writer, module.nt_restart)?;
	// Song positions
	write_all(writer,&module.positions.data)?;
	// M.K.
	write_all(writer,&module.mk)?;
	
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
				
				write_all(writer,&data)?;
			}
		}
		// Write extra rows so that there are 64 rows
		for _ in 0..DEFAULT_NUMBER_OF_ROWS_PER_PATTERN-pattern_it.rows.len() {
			let data = [0u8; 4];
			for _ in 0..module.num_channels {
				write_all(writer,&data)?;
			}
		}
	}
	
	// And finally all samples
	let mut num_written = 0;
	for sample_it in module.sample_info.iter().filter(|si| si.length > 0) {
		write_all(writer,&sample_it.data)?;
		num_written += 1;
	}
	
	if num_samples != num_written {
		return Err(PTMFError::Parse(format!("Warning! Number of samples '{}' does not match sample data '{}'", num_samples, num_written)));
	}
	
	Ok(())
}

/// Read a 31 sample Amiga ProTracker mod-file
pub fn read_mod(reader: &mut dyn Read, ignore_file_size_check: bool) -> Result<PTModule, PTMFError> {
	let mut module = PTModule::new();

	// First read 20 bytes songname
	module.name = read_0_padded_string(reader, 20)?;

	// Read all sample info
	// TODO Handle 15 sample files
	for _i in 0..DEFAULT_NUMBER_OF_SAMPLES {
		let mut si = SampleInfo::new();
		// Sample name
		si.name = read_0_padded_string(reader, 22)?;
		si.length = read_big_endian_u16(reader)?;
		// Finetune
		si.finetune = read_u8(reader)?;
		// Volume
		si.volume = read_u8(reader)?;
		// Repeat start
		si.repeat_start = read_big_endian_u16(reader)?;
		// Repeat length
		si.repeat_length = read_big_endian_u16(reader)?;

		module.sample_info.push(si);
	}
	
	// Songlength
	module.length = read_u8(reader)?;
	// nt_restart
	module.nt_restart = read_u8(reader)?;
	// Song positions
	read_all(reader,&mut module.positions.data)?;
	// M.K.
	read_all(reader,&mut module.mk)?;
	if module.mk != MAGIC_MK &&
		module.mk != MAGIC_MK99 &&
		module.mk != MAGIC_FLT4 &&
		module.mk != MAGIC_4CHN &&
		module.mk != MAGIC_6CHN &&
		module.mk != MAGIC_8CHN {
		return Err(PTMFError::Parse(format!("Unknown format {:?}", module.mk)));
	}
	module.num_channels = 
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
		let mut pattern = Pattern::new(DEFAULT_NUMBER_OF_ROWS_PER_PATTERN,module.num_channels);
		for row in &mut pattern.rows {
			for channel in &mut row.channels {
				let mut data  = [0u8;4];
				read_all(reader,&mut data)?;
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
			read_all(reader,&mut data)?;
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
pub fn read_p61(reader: &mut dyn Read) -> Result<PTModule, PTMFError> {
	let mut module = PTModule::new();
	module.num_channels = DEFAULT_NUMBER_OF_CHANNELS_PER_ROW;
	
	// Read the entire file to a Vec<u8>
	// since the format uses a lot of offsets
	// back and forth in the file.
	let mut data:Vec<u8> = Vec::new();
	let data_size = reader.read_to_end(&mut data)?;
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
		module.sample_info.push(SampleInfo::new());
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
	println!("patterndata address: {:X}",pattern_start_offset);
	
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
fn qqq_encode_p61_channel(channel: &Channel) -> Vec<u8> {

	let mut data:Vec<u8> = Vec::new();

// 4 different cases:
// pnnnnnni => 2 more bytes follow iiiicccc bbbbbbbb => Note, instrument and effect
// p110cccc => 1 more byte follows bbbbbbbb => Only effect
// p1110nnn => 1 more byte follows => nnniiiii => Note and instrument, no effect
// p1111111 => 0 more bytes follow => Empty row

	let has_note_or_instrument = (channel.period > 0) || (channel.sample_number > 0);
	let has_effect = channel.effect > 0;

	// if channel.period == 0 then there is no note
	// else try to find it in the array
	// TODO create a map/hash/dict for fast lookup
	let period_idx = if channel.period == 0 {
		0 as u8
	} else {
		PERIODS.iter().position(|&element| element == channel.period).unwrap() as u8 - 11
	};
	let instrument_idx = channel.sample_number;
	let mut effect = channel.effect;

	let cmd = (effect & 0xf00) >> 8;
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

	// Now we have all data we need to encode the command

	if has_note_or_instrument && has_effect {
		// note, instrument and effect
		data.push(0);
		data.push(0);
		data.push(0);

		data[0] = ((period_idx & 0b111111) << 1) | ((instrument_idx & 0b10000) >> 4);
		data[1] = ((instrument_idx & 0b1111) << 4) | ((effect & 0xf00) >> 8) as u8;
		data[2] = (effect & 0xff) as u8;
		
	} else if has_note_or_instrument {
		// note and instrument
		data.push(0);
		data.push(0);

		data[0] = (0b01110000) | ((period_idx & 0b111111) >> 3);
		data[1] = ((period_idx & 0b111111) << 5) | (instrument_idx & 0b11111);

	} else if has_effect {
		// effect
		data.push(0);
		data.push(0);

		data[0] = (0b01100000) | ((effect & 0xf00) >> 8) as u8;
		data[1] = (effect & 0xff) as u8;

	} else {
		// Must be empty
		data.push(0);
		data[0] = 0b01111111;
	}


	data
}


fn encode_p61_channel(channel: &Channel) -> Vec<u8> {
	let mut data:Vec<u8> = Vec::new();

	let period_idx = if channel.period == 0 {
		0 as u8
	} else {
		PERIODS.iter().position(|&element| element == channel.period).unwrap() as u8 - 11
	};
	let instrument_idx = channel.sample_number;
	let mut effect = channel.effect;

	let cmd = (effect & 0xf00) >> 8;
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

	// result should be 4 bytes
	// pnnnnnni iiiicccc bbbbbbbb 0
	// p = compression flag
	// n = note index
	// i = instrument index
	// c = effect
	// b = effect params
	// 0 = compression info

	let byte1 = 0 | ((period_idx << 1) & 0b01111110) as u8 | ((instrument_idx >> 4) & 0b00000001) as u8;
	let byte2 = ((instrument_idx & 0b00001111) << 4) as u8 | ((effect & 0xf00) >> 8) as u8;
	let byte3 = (effect & 0xff) as u8; 
	let byte4 = 0;
	data.push(byte1);
	data.push(byte2);
	data.push(byte3);
	data.push(byte4);

	data
}

/// read one command from stream
/// convert it and write it to packed_stream
/// TODO this might be better to do with Read and Write traits
fn p61_putdata(stream:&Vec<u8>,stream_offset:&mut usize,packed_stream:&mut Vec<u8>) {

	// input
	// pnnnnnni iiiicccc bbbbbbbb 0
	// p = compression flag
	// n = note index
	// i = instrument index
	// c = effect
	// b = effect params
	// 0 = compression info

	let byte1 = stream[*stream_offset+0];
	let byte2 = stream[*stream_offset+1];
	let byte3 = stream[*stream_offset+2];
	let byte4 = stream[*stream_offset+3];

	let word1 = (byte1 as u16) << 8 | byte2 as u16;
	let compression_flag = byte1 & 0x80;

	// output
	// 4 different cases:
	// pnnnnnni => 2 more bytes follow iiiicccc bbbbbbbb => Note, instrument and effect
	// p110cccc => 1 more byte follows bbbbbbbb => Only effect
	// p1110nnn => 1 more byte follows => nnniiiii => Note and instrument, no effect
	// p1111111 => 0 more bytes follow => Empty row

	if byte1 & 0x7e != 0 || word1 & 0x1f0 != 0 {
		if byte2 & 0xf != 0 {
			// note, instrument and command
			// pnnnnnni => 2 more bytes follow iiiicccc bbbbbbbb => Note, instrument and effect
			packed_stream.push(byte1);
			packed_stream.push(byte2);
			packed_stream.push(byte3);
		} else {
			// Note and instrument
			// p1110nnn => 1 more byte follows => nnniiiii => Note and instrument, no effect
			packed_stream.push(compression_flag | 0b01110000 | (byte1 & 0b01110000) >> 4);
			packed_stream.push((byte1 & 0b1111) << 4 | (byte2 & 0b11110000) >> 4);
		}
	} else if byte2 & 0xf == 0 {
		// empty
		// p1111111 => 0 more bytes follow => Empty row
		packed_stream.push(compression_flag | 0x7f);
	} else {
		// command
		// encode as only command
		// p110cccc => 1 more byte follows bbbbbbbb => Only effect
		packed_stream.push(compression_flag | 0b01100000 | byte2 & 0xf);
		packed_stream.push(byte3);
	}

	if compression_flag == 0x80 {
		packed_stream.push(byte4);
	}

	*stream_offset += 4;
}


/// Write a 31 sample Amiga ProTracker mod-file as if packed with The Player
pub fn write_p61(writer: &mut dyn Write, module: &PTModule) -> Result<(),PTMFError> {
	// TODO fail if not 4 channels

	let mut workmodule = module.clone();

	// Prepare all data first
	workmodule.truncate_samples();
	workmodule.remove_duplicate_samples();
	workmodule.remove_unused_samples();
	workmodule.truncate_patterns();
	workmodule.remove_duplicate_patterns();
	workmodule.remove_unused_patterns();




	// Then we write it


	Ok(())
}


#[cfg(test)]
mod tests {
    // Note this useful idiom: importing names from outer (for mod tests) scope.
    use super::*;
	use std::fs::File;
	use std::io::BufWriter;
	use std::io::BufReader;

	fn load_module(infilename:String) -> Result<PTModule,()> {
		let file = match File::open(&infilename) {
			Ok(file) => file,
			Err(e) => {
				println!("Failed to open file: '{}' Error: '{}'", infilename, e);
				return Err(())
			}
		};
		
		let mut reader = BufReader::new(&file);
		let module = match read_mod(&mut reader, true) {
			Ok(module) => module,
			Err(e) => {
				println!("Failed to parse file: '{}' Error: '{:?}'", infilename, e);
				return Err(())
			}
		};

		// Close file
		drop(file);
		Ok(module)
	}

	fn save_module(outfilename:String, module:PTModule) -> Result<(),()> {
		let file = match File::create(&outfilename) {
			Ok(file) => file,
			Err(e) => {
				println!("Failed to open file: '{}' Error: '{:?}'", outfilename, e);
				return Err(())
			}
		};

		let mut writer = BufWriter::new(&file);		
		match write_mod(&mut writer,&module) {
			Ok(_) => Ok(()),
			Err(e) => {
				println!("Failed to write module {}. Error: '{:?}'", outfilename, e);
				return Err(());
			}
		}
	}
	
    #[test]
    fn test_write_p61() -> Result<(),()> {
		let basedir = env!("CARGO_MANIFEST_DIR");

		let infilename = format!("{}/{}",basedir, "incy-wincy.mod");
		let mut module = load_module(infilename)?;

		let outfilename = format!("{}/{}",basedir, "P61.test_write_p61.mod");
		let file = match File::create(&outfilename) {
			Ok(file) => file,
			Err(e) => {
				println!("Failed to open file: '{}' Error: '{:?}'", outfilename, e);
				return Err(())
			}
		};

		let mut writer = BufWriter::new(&file);
		let _res = match write_p61(&mut writer,&mut module) {
			Ok(e) => e,
			Err(e) => {
				println!("Failed to open file: '{}' Error: '{:?}'", outfilename, e);
				return Err(())
			}
		};
		Ok(())
    }

	#[test]
	fn test_read_p61() -> Result<(),()> {
		let basedir = env!("CARGO_MANIFEST_DIR");
		let infilename = format!("{}/{}",basedir, "P61.incy-wincy.mod");

		let file = match File::open(&infilename) {
			Ok(file) => file,
			Err(e) => {
				println!("Failed to open file: '{}' Error: '{}'", infilename, e);
				return Err(())
			}
		};
		
		let mut reader = BufReader::new(&file);
		let module = match read_p61(&mut reader) {
			Ok(module) => module,
			Err(e) => {
				println!("Failed to parse file: '{}' Error: '{:?}'", infilename, e);
				return Err(())
			}
		};

		// Close file
		drop(file);

//		let samples = module.sample_info.iter().filter(|si| si.length > 0);
//		let num_samples = samples.count();
//		assert!(num_samples == 18);
//		assert!(module.sample_info.len() == 31);
//		assert!(module.length == 25);
//		assert!(module.patterns.len() == 13);

		Ok(())
	}

	#[test]
	fn test_read_mod() -> Result<(),()> {
		let basedir = env!("CARGO_MANIFEST_DIR");
		let infilename = format!("{}/testdata/{}",basedir, "spiderfunk.mod");

		let file = match File::open(&infilename) {
			Ok(file) => file,
			Err(e) => {
				println!("Failed to open file: '{}' Error: '{}'", infilename, e);
				return Err(())
			}
		};
		
		let mut reader = BufReader::new(&file);
		let module = match read_mod(&mut reader, true) {
			Ok(module) => module,
			Err(e) => {
				println!("Failed to parse file: '{}' Error: '{:?}'", infilename, e);
				return Err(())
			}
		};

		// Close file
		drop(file);

		let samples = module.sample_info.iter().filter(|si| si.length > 0);
		let num_samples = samples.count();
		assert!(num_samples == 18);
		assert!(module.sample_info.len() == 31);
		assert!(module.length == 25);
		assert!(module.patterns.len() == 13);

		Ok(())
	}

	#[test]
	fn test_write_mod() -> Result<(),()> {
		let basedir = env!("CARGO_MANIFEST_DIR");
		let infilename = format!("{}/testdata/{}",basedir, "spiderfunk.mod");

		let mut file = match File::open(&infilename) {
			Ok(file) => file,
			Err(e) => {
				println!("Failed to open file: '{}' Error: '{}'", infilename, e);
				return Err(())
			}
		};

		let mut org_data:Vec<u8> = Vec::new();
		let _data_size = match file.read_to_end(&mut org_data) {
			Ok(file) => file,
			Err(e) => {
				println!("Failed to read file: '{}' Error: '{}'", infilename, e);
				return Err(())
			}
		};

		let module = match read_mod(&mut org_data.as_slice(), true) {
			Ok(module) => module,
			Err(e) => {
				println!("Failed to parse file: '{}' Error: '{:?}'", infilename, e);
				return Err(())
			}
		}; 
		let mut new_data:Vec<u8> = Vec::new();
		match write_mod(&mut new_data, &module) {
			Ok(_) => (),
			Err(e) => {
				println!("Failed to write module. Error: '{:?}'", e);
				return Err(());
			}
		};

		assert!(org_data == new_data);

		Ok(())
	}

	#[test]
	fn test_write_mod_truncated() -> Result<(),()> {
		let basedir = env!("CARGO_MANIFEST_DIR");
		let infilename = format!("{}/testdata/{}",basedir, "truncate_patterns.mod");

		let mut file = match File::open(&infilename) {
			Ok(file) => file,
			Err(e) => {
				println!("Failed to open file: '{}' Error: '{}'", infilename, e);
				return Err(())
			}
		};

		let mut org_data:Vec<u8> = Vec::new();
		let _data_size = match file.read_to_end(&mut org_data) {
			Ok(file) => file,
			Err(e) => {
				println!("Failed to read file: '{}' Error: '{}'", infilename, e);
				return Err(())
			}
		};

		let mut module = match read_mod(&mut org_data.as_slice(), true) {
			Ok(module) => module,
			Err(e) => {
				println!("Failed to parse file: '{}' Error: '{:?}'", infilename, e);
				return Err(())
			}
		}; 

		// remove som rows
		for _ in 0..10 {
			module.patterns[0].rows.pop();
			module.patterns[2].rows.pop();
			module.patterns[5].rows.pop();
			module.patterns[7].rows.pop();
		}

		let mut new_data:Vec<u8> = Vec::new();
		match write_mod(&mut new_data, &module) {
			Ok(_) => (),
			Err(e) => {
				println!("Failed to write module. Error: '{:?}'", e);
				return Err(());
			}
		};

		assert!(org_data == new_data);

		Ok(())
	}

    #[test]
	fn test_find_duplicate_patterns() -> Result<(),()> {
		let basedir = env!("CARGO_MANIFEST_DIR");

		let infilename = format!("{}/testdata/{}",basedir, "duplicate_patterns.mod");
		let module = load_module(infilename)?;

		let duplicates = module.find_duplicate_patterns();
		assert!(duplicates.len() > 0);
		assert!(duplicates.len() == 4467);

		Ok(())
	}

	#[test]
	fn test_remove_duplicate_patterns() -> Result<(),()> {
		let basedir = env!("CARGO_MANIFEST_DIR");

		let infilename = format!("{}/testdata/{}",basedir, "duplicate_patterns.mod");
		let mut module = load_module(infilename)?;

		module.remove_duplicate_patterns();
		assert!(module.patterns.len() == 4);

		let outfilename = format!("{}/{}",basedir, "test_remove_duplicate_patterns.mod");
		save_module(outfilename,module)?;

		Ok(())
	}

	#[test]
	fn test_find_unused_samples_all_used() -> Result<(),()> {
		let basedir = env!("CARGO_MANIFEST_DIR");

		let infilename = format!("{}/testdata/{}",basedir, "all_used_samples.mod");
		let module = load_module(infilename)?;

		let unused = module.find_unused_samples();
		assert!(unused.len() == 0);

		Ok(())
	}

	#[test]
	fn test_find_unused_samples_no_used() -> Result<(),()> {
		let basedir = env!("CARGO_MANIFEST_DIR");

		let infilename = format!("{}/testdata/{}",basedir, "no_used_samples.mod");
		let module = load_module(infilename)?;

		let unused = module.find_unused_samples();
		assert!(unused.len() == 31);

		Ok(())
	}

	#[test]
	fn test_find_unused_samples_1() -> Result<(),()> {
		let basedir = env!("CARGO_MANIFEST_DIR");

		let infilename = format!("{}/testdata/{}",basedir, "unused_samples_1.mod");
		let module = load_module(infilename)?;

		let unused = module.find_unused_samples();
		assert!(unused.len() == 1);
		assert!(unused[0] == 1);

		Ok(())
	}

	#[test]
	fn test_find_unused_samples_2() -> Result<(),()> {
		let basedir = env!("CARGO_MANIFEST_DIR");

		let infilename = format!("{}/testdata/{}",basedir, "unused_samples_2.mod");
		let module = load_module(infilename)?;

		let unused = module.find_unused_samples();
		assert!(unused.len() == 1);
		assert!(unused[0] == 2);

		Ok(())
	}

	#[test]
	fn test_find_unused_samples_3() -> Result<(),()> {
		let basedir = env!("CARGO_MANIFEST_DIR");

		let infilename = format!("{}/testdata/{}",basedir, "unused_samples_3.mod");
		let module = load_module(infilename)?;

		let unused = module.find_unused_samples();
		assert!(unused.len() == 1);
		assert!(unused[0] == 3);

		Ok(())
	}

	#[test]
	fn test_find_unused_samples_1d() -> Result<(),()> {
		let basedir = env!("CARGO_MANIFEST_DIR");

		let infilename = format!("{}/testdata/{}",basedir, "unused_samples_1d.mod");
		let module = load_module(infilename)?;

		let unused = module.find_unused_samples();
		assert!(unused.len() == 1);
		assert!(unused[0] == 0x1d);

		Ok(())
	}

	#[test]
	fn test_find_unused_samples_1e() -> Result<(),()> {
		let basedir = env!("CARGO_MANIFEST_DIR");

		let infilename = format!("{}/testdata/{}",basedir, "unused_samples_1e.mod");
		let module = load_module(infilename)?;

		let unused = module.find_unused_samples();
		assert!(unused.len() == 1);
		assert!(unused[0] == 0x1e);

		Ok(())
	}

	#[test]
	fn test_find_unused_samples_1f() -> Result<(),()> {
		let basedir = env!("CARGO_MANIFEST_DIR");

		let infilename = format!("{}/testdata/{}",basedir, "unused_samples_1f.mod");
		let module = load_module(infilename)?;

		let unused = module.find_unused_samples();
		assert!(unused.len() == 1);
		assert!(unused[0] == 0x1f);

		Ok(())
	}

	#[test]
	fn test_remove_unused_samples_all_used() -> Result<(),()> {
		let basedir = env!("CARGO_MANIFEST_DIR");

		let infilename = format!("{}/testdata/{}",basedir, "all_used_samples.mod");
		let mut module = load_module(infilename)?;

		module.remove_unused_samples();
		assert!(module.sample_info.len() == DEFAULT_NUMBER_OF_SAMPLES);
		let samples = module.sample_info.iter().filter(|si| si.length > 0);
		let num_samples = samples.count();
		assert!(num_samples == 31);

		let outfilename = format!("{}/{}",basedir, "test_remove_unused_samples_all_used.mod");
		save_module(outfilename,module)?;

		Ok(())
	}

	#[test]
	fn test_remove_unused_samples_no_used() -> Result<(),()> {
		let basedir = env!("CARGO_MANIFEST_DIR");

		let infilename = format!("{}/testdata/{}",basedir, "no_used_samples.mod");
		let mut module = load_module(infilename)?;

		module.remove_unused_samples();
		assert!(module.sample_info.len() == 0);
		let samples = module.sample_info.iter().filter(|si| si.length > 0);
		let num_samples = samples.count();
		assert!(num_samples == 0);

		let outfilename = format!("{}/{}",basedir, "test_remove_unused_samples_no_used.mod");
		save_module(outfilename,module)?;

		Ok(())
	}

	#[test]
	fn test_remove_unused_samples_1() -> Result<(),()> {
		let basedir = env!("CARGO_MANIFEST_DIR");

		let infilename = format!("{}/testdata/{}",basedir, "unused_samples_1.mod");
		let mut module = load_module(infilename)?;

		module.remove_unused_samples();
		assert!(module.sample_info.len() == 30);
		let samples = module.sample_info.iter().filter(|si| si.length > 0);
		let num_samples = samples.count();
		assert!(num_samples == 30);

		let outfilename = format!("{}/{}",basedir, "test_remove_unused_samples_1.mod");
		save_module(outfilename,module)?;

		Ok(())
	}

	#[test]
	fn test_find_duplicate_samples_same_sample() -> Result<(),()> {
		let basedir = env!("CARGO_MANIFEST_DIR");

		let infilename = format!("{}/testdata/{}",basedir, "same_sample.mod");
		let module = load_module(infilename)?;

		let duplicates = module.find_duplicate_samples();
		assert!(duplicates.len() > 0);
		assert!(duplicates.len() == 465);

		Ok(())
	}

	#[test]
	fn test_find_duplicate_samples_same_sample_ft() -> Result<(),()> {
		let basedir = env!("CARGO_MANIFEST_DIR");

		let infilename = format!("{}/testdata/{}",basedir, "same_sample_ft.mod");
		let module = load_module(infilename)?;

		let duplicates = module.find_duplicate_samples();
		assert!(duplicates.len() > 0);
		assert!(duplicates.len() == 353);

		Ok(())
	}

	#[test]
	fn test_find_duplicate_samples_same_sample_rep() -> Result<(),()> {
		let basedir = env!("CARGO_MANIFEST_DIR");

		let infilename = format!("{}/testdata/{}",basedir, "same_sample_rep.mod");
		let module = load_module(infilename)?;

		let duplicates = module.find_duplicate_samples();
		assert!(duplicates.len() > 0);
		assert!(duplicates.len() == 406);

		Ok(())
	}

	#[test]
	fn test_remove_duplicate_samples_same_sample() -> Result<(),()> {
		let basedir = env!("CARGO_MANIFEST_DIR");

		let infilename = format!("{}/testdata/{}",basedir, "same_sample.mod");
		let mut module = load_module(infilename)?;

		module.remove_duplicate_samples();
		assert!(module.sample_info.len() == 1);
		let samples = module.sample_info.iter().filter(|si| si.length > 0);
		let num_samples = samples.count();
		assert!(num_samples == 1);

		let outfilename = format!("{}/{}",basedir, "test_remove_duplicate_samples_same_sample.mod");
		save_module(outfilename,module)?;

		Ok(())
	}

	#[test]
	fn test_remove_duplicate_samples_same_sample_ft() -> Result<(),()> {
		let basedir = env!("CARGO_MANIFEST_DIR");

		let infilename = format!("{}/testdata/{}",basedir, "same_sample_ft.mod");
		let mut module = load_module(infilename)?;

		module.remove_duplicate_samples();
		let samples = module.sample_info.iter().filter(|si| si.length > 0);
		assert!(module.sample_info.len() == 3);
		let num_samples = samples.count();
		assert!(num_samples == 3);

		let outfilename = format!("{}/{}",basedir, "test_remove_duplicate_samples_same_sample_ft.mod");
		save_module(outfilename,module)?;

		Ok(())
	}

	#[test]
	fn test_remove_duplicate_samples_same_sample_rep() -> Result<(),()> {
		let basedir = env!("CARGO_MANIFEST_DIR");

		let infilename = format!("{}/testdata/{}",basedir, "same_sample_rep.mod");
		let mut module = load_module(infilename)?;

		module.remove_duplicate_samples();
		let samples = module.sample_info.iter().filter(|si| si.length > 0);
		assert!(module.sample_info.len() == 3);
		let num_samples = samples.count();
		assert!(num_samples == 3);

		let outfilename = format!("{}/{}",basedir, "test_remove_duplicate_samples_same_sample_rep.mod");
		save_module(outfilename,module)?;

		Ok(())
	}

	#[test]
	fn test_truncate_samples() -> Result<(),()> {
		let basedir = env!("CARGO_MANIFEST_DIR");

		let infilename = format!("{}/testdata/{}",basedir, "truncate_samples.mod");
		let mut module = load_module(infilename)?;

		module.truncate_samples();

		assert!(module.sample_info[0].data.len() == 7820);
		assert!(module.sample_info[1].data.len() == 7820);
		assert!(module.sample_info[2].data.len() == 5278);
		assert!(module.sample_info[3].data.len() == 2540);
		assert!(module.sample_info[4].data.len() == 7820);

		let outfilename = format!("{}/{}",basedir, "test_truncate_samples.mod");
		save_module(outfilename,module)?;

		Ok(())
	}

	#[test]
	fn test_truncate_patterns() -> Result<(),()> {
		let basedir = env!("CARGO_MANIFEST_DIR");

		let infilename = format!("{}/testdata/{}",basedir, "truncate_patterns.mod");
		let mut module = load_module(infilename)?;

		module.truncate_patterns();

		assert!(module.patterns[0].rows.len() == 33);
		assert!(module.patterns[1].rows.len() == 33);
		assert!(module.patterns[2].rows.len() == 49);
		assert!(module.patterns[3].rows.len() == 49);
		assert!(module.patterns[4].rows.len() == 41);

		let outfilename = format!("{}/{}",basedir, "test_remove_duplicate_patterns.mod");
		save_module(outfilename,module)?;

		Ok(())
	}

	#[test]
	fn test_encode_p61_channel() -> Result<(),()> {
		let mut ch = Channel::new();

		// Empty
		ch.effect = 0;
		ch.sample_number = 0;
		ch.period = 0;
		let res = encode_p61_channel(&ch);
		assert!(res.len() == 4);
		assert!(res[0] == 0);
		assert!(res[1] == 0);
		assert!(res[2] == 0);
		assert!(res[3] == 0);

		// Only effect
		ch.effect = 0xc3f;
		ch.sample_number = 0;
		ch.period = 0;
		let res = encode_p61_channel(&ch);
		assert!(res.len() == 4);
		assert!(res[0] == 0);
		assert!(res[1] == 0xc);
		assert!(res[2] == 0x3f);
		assert!(res[3] == 0);
	
		// all periods and alla samples and no effect
		for i in 12..PERIODS.len() {
			ch.period = PERIODS[i];
			for sample_nr in 1..32 {
				ch.sample_number = sample_nr;
				ch.effect = 0;

				let res = encode_p61_channel(&ch);
				let period_idx = (i-11) as u8;
				let instrument_idx = sample_nr as u8;
				assert!(res.len() == 4);
				assert!(res[0] == ((period_idx << 1) | ((instrument_idx & 0b10000) >> 4)));
				assert!(res[1] == ((instrument_idx & 0b1111) << 4));
				assert!(res[2] == 0);
				assert!(res[3] == 0);
			}
		}


		// all periods and alla samples and effect
		for i in 12..PERIODS.len() {
			ch.period = PERIODS[i];
			for sample_nr in 1..32 {
				ch.sample_number = sample_nr;
				ch.effect = 0xc30;

				let res = encode_p61_channel(&ch);
				let period_idx = (i-11) as u8;
				let instrument_idx = sample_nr as u8;
				let cmd = (ch.effect >> 8) as u8;
				let params = (ch.effect & 0xff) as u8;
				assert!(res.len() == 4);
				assert!(res[0] == ((period_idx << 1) | ((instrument_idx & 0b10000) >> 4)));
				assert!(res[1] == ((instrument_idx & 0b1111) << 4) | (cmd & 0b1111));
				assert!(res[2] == params);
				assert!(res[3] == 0);
			}
		}

		// periods and effects
		for i in 12..PERIODS.len() {
			ch.period = PERIODS[i];
			ch.sample_number = 0;
			ch.effect = 0xc3f;

			let res = encode_p61_channel(&ch);
			let period_idx = (i-11) as u8;
			let instrument_idx = ch.sample_number as u8;
			let cmd = (ch.effect >> 8) as u8;
			let params = (ch.effect & 0xff) as u8;
			assert!(res.len() == 4);
			assert!(res[0] == ((period_idx << 1) | ((instrument_idx & 0b10000) >> 4)));
			assert!(res[1] == ((instrument_idx & 0b1111) << 4) | (cmd & 0b1111));
			assert!(res[2] == params);
			assert!(res[3] == 0);
	}

		// instrument and effects
		for sample_nr in 1..32 {
			ch.period = 0;
			ch.sample_number = sample_nr;
			ch.effect = 0xc3f;

			let res = encode_p61_channel(&ch);
			let period_idx = 0 as u8;
			let instrument_idx = ch.sample_number as u8;
			let cmd = (ch.effect >> 8) as u8;
			let params = (ch.effect & 0xff) as u8;
			assert!(res.len() == 4);
			assert!(res[0] == ((period_idx << 1) | ((instrument_idx & 0b10000) >> 4)));
			assert!(res[1] == ((instrument_idx & 0b1111) << 4) | (cmd & 0b1111));
			assert!(res[2] == params);
			assert!(res[3] == 0);
		}
	
		Ok(())
	}

	#[test]
	fn test_p61_putdata() -> Result<(),()> {

		// input
		// pnnnnnni iiiicccc bbbbbbbb 0
		// p = compression flag
		// n = note index
		// i = instrument index
		// c = effect
		// b = effect params
		// 0 = compression info

		// output
		// 4 different cases:
		// pnnnnnni => 2 more bytes follow iiiicccc bbbbbbbb => Note, instrument and effect
		// p110cccc => 1 more byte follows bbbbbbbb => Only effect
		// p1110nnn => 1 more byte follows => nnniiiii => Note and instrument, no effect
		// p1111111 => 0 more bytes follow => Empty row

		// empty
		let data:Vec<u8> = vec![0,0,0,0];
		let mut offset = 0;
		let mut result:Vec<u8> = vec![];
		p61_putdata(&data, &mut offset, &mut result);
		assert!(offset == 4);
		assert!(result.len() == 1);
		assert!(result[0] == 0x7f);

		// empty with compression
		let data:Vec<u8> = vec![0x80,0,0,0x23];
		let mut offset = 0;
		let mut result:Vec<u8> = vec![];
		p61_putdata(&data, &mut offset, &mut result);
		assert!(offset == 4);
		assert!(result.len() == 2);
		assert!(result[0] == 0xff);
		assert!(result[1] == 0x23);

		// Note only
		let data:Vec<u8> = vec![4<<1,0,0,0];
		let mut offset = 0;
		let mut result:Vec<u8> = vec![];
		p61_putdata(&data, &mut offset, &mut result);
		assert!(offset == 4);
		assert!(result.len() == 2);
		assert!(result[0] == 0b01110000);
		assert!(result[1] == 4 << 5);

		// Note only and compression
		let data:Vec<u8> = vec![0x80 | 4<<1,0,0,0x84];
		let mut offset = 0;
		let mut result:Vec<u8> = vec![];
		p61_putdata(&data, &mut offset, &mut result);
		assert!(offset == 4);
		assert!(result.len() == 3);
		assert!(result[0] == 0x80 | 0b01110000);
		assert!(result[1] == 4 << 5);
		assert!(result[2] == 0x84);

		// Note and instrument
		let data:Vec<u8> = vec![4<<1,7<<4,0,0];
		let mut offset = 0;
		let mut result:Vec<u8> = vec![];
		p61_putdata(&data, &mut offset, &mut result);
		assert!(offset == 4);
		assert!(result.len() == 2);
		assert!(result[0] == 0b01110000);
		assert!(result[1] == 4 << 5 | 7);

		// Note and instrument and compression
		let data:Vec<u8> = vec![0x80 | 4<<1,7<<4,0,8];
		let mut offset = 0;
		let mut result:Vec<u8> = vec![];
		p61_putdata(&data, &mut offset, &mut result);
		assert!(offset == 4);
		assert!(result.len() == 3);
		assert!(result[0] == 0x80 | 0b01110000);
		assert!(result[1] == 4 << 5 | 7);
		assert!(result[2] == 8);

		// Instrument only
		let data:Vec<u8> = vec![0,7<<4,0,0];
		let mut offset = 0;
		let mut result:Vec<u8> = vec![];
		p61_putdata(&data, &mut offset, &mut result);
		assert!(offset == 4);
		assert!(result.len() == 2);
		assert!(result[0] == 0b01110000);
		assert!(result[1] == 7);

		// Instrument and compression
		let data:Vec<u8> = vec![0x80 ,7<<4,0,8];
		let mut offset = 0;
		let mut result:Vec<u8> = vec![];
		p61_putdata(&data, &mut offset, &mut result);
		assert!(offset == 4);
		assert!(result.len() == 3);
		assert!(result[0] == 0x80 | 0b01110000);
		assert!(result[1] == 7);
		assert!(result[2] == 8);

		// Effect only
		let data:Vec<u8> = vec![0,9,0x45,0];
		let mut offset = 0;
		let mut result:Vec<u8> = vec![];
		p61_putdata(&data, &mut offset, &mut result);
		assert!(offset == 4);
		assert!(result.len() == 2);
		assert!(result[0] == 0b01100000 | 9);
		assert!(result[1] == 0x45);
		
		// Effect only and compression
		let data:Vec<u8> = vec![0x80,9,0x45,0x89];
		let mut offset = 0;
		let mut result:Vec<u8> = vec![];
		p61_putdata(&data, &mut offset, &mut result);
		assert!(offset == 4);
		assert!(result.len() == 3);
		assert!(result[0] == 0x80 | 0b01100000 | 9);
		assert!(result[1] == 0x45);
		assert!(result[2] == 0x89);

		// All data
		let data:Vec<u8> = vec![12<<1,7<<4|0xe,0x23,0];
		let mut offset = 0;
		let mut result:Vec<u8> = vec![];
		p61_putdata(&data, &mut offset, &mut result);
		assert!(offset == 4);
		assert!(result.len() == 3);
		assert!(result[0] == 12<<1);
		assert!(result[1] == 7<<4|0xe);
		assert!(result[2] == 0x23);

		// All data and compression
		let data:Vec<u8> = vec![0x80|12<<1,7<<4|0xe,0x23,0x84];
		let mut offset = 0;
		let mut result:Vec<u8> = vec![];
		p61_putdata(&data, &mut offset, &mut result);
		assert!(offset == 4);
		assert!(result.len() == 4);
		assert!(result[0] == 0x80|12<<1);
		assert!(result[1] == 7<<4|0xe);
		assert!(result[2] == 0x23);
		assert!(result[3] == 0x84);

		Ok(())
	}

}

/// Write a 31 sample Amiga ProTracker mod-file as if packed with The Player
pub fn qqqwrite_p61(writer: &mut dyn Write, module: &mut PTModule) -> Result<(),PTMFError> {

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
	

	// Sample data
	
	Ok(())
}
