use std::io::Write;
use std::io::Read;
use std::io::Cursor;
use std::io::Error;
use std::io::copy;

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
			} else {
				// The Player removes trailing 0
				// but only complete word
				while si.data.len() > 2 && 
					si.data[si.data.len()-1] == 0 && 
					si.data[si.data.len()-2] == 0 {
					si.data.pop();
					si.data.pop();
				}
			}
		}
	}

	pub fn truncate_patterns(&mut self) {
		for pattern in &mut self.patterns {
			let mut found = false;
			for i in 0..pattern.rows.len() {
				for channel in &mut pattern.rows[i].channels {
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

	pub fn remove_duplicate_breaks(&mut self) {
		for pattern in &mut self.patterns {
			let mut found = false;
			for i in 0..pattern.rows.len() {
				for channel in &mut pattern.rows[i].channels {
					let effect = (channel.effect & 0xf00) >> 8;
					if effect == 0xd || effect == 0xb {
						if found  {
							// already found in another channel
							// remove it
							channel.effect = 0;
						} else {
							found = true;
						}
					}
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

/// Read an Amiga ProTracker file packed with The Player 6.1
pub fn analyze_p61(reader: &mut dyn Read) -> Result<PTModule, PTMFError> {
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
		println!("has header");
		// Remove header
		data.remove(0);
		data.remove(0);
		data.remove(0);
		data.remove(0);
	}

	let sample_offset = ((data[pos] as u16) << 8) | data[pos+1] as u16;
	let num_patterns = data[pos+2];
	println!("number of patterns {}",num_patterns);
	let num_samples = data[pos+3];
	pos += 4;
	
	let is_delta_8_bit = num_samples & 0x80 == 0x80;
	let is_delta_4_bit = num_samples & 0x40 == 0x40;
	let num_samples = num_samples & 0b00111111;

	println!("number of samples {} 8 bit delta packing {} 4 bit delta packing {}",num_samples,is_delta_8_bit,is_delta_4_bit);
	
	let mut unpacked_samples_length:u32 = 0; 
	if is_delta_4_bit {
		for _ in 0..4 {
			unpacked_samples_length = (unpacked_samples_length << 8) + data[pos] as u32;
			pos += 1;
		}
		println!("unpacked sample length {}",unpacked_samples_length);
	}
	
	println!("samples:");
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
		
		println!("  sample length {} finetune {} volume {} repeat start {} repeat length {}",sample_length,finetune,volume,repeat_start,repeat_length);

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

			println!("   using the same sample data as {}",sample_index);
			println!("   sample length {} finetune {} volume {} repeat start {} repeat length {}",length,finetune & 0xf,volume,repeat_start,repeat_length);
			
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
				println!("   sample data is 4 bit delta packed");
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
				println!("   sample data is 8 bit delta packed");
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
	
	let mut offset_pos = pos;
	let mut pattern_offsets:Vec<usize> = Vec::new();
	for _ in 0..num_patterns {
		module.patterns.push(Pattern::new(DEFAULT_NUMBER_OF_ROWS_PER_PATTERN, DEFAULT_NUMBER_OF_CHANNELS_PER_ROW));
		for _ in 0..DEFAULT_NUMBER_OF_CHANNELS_PER_ROW {
			let offset = (((data[pos] as u16) << 8) | data[pos+1] as u16) as usize;
			pattern_offsets.push(offset);
			
			pos += 2;
		}
	}
	
	println!("positions:");
	print!("  ");
	let mut length = 0;
	loop {
		let position = data[pos];
		if position == 0xff {
			break;
		}
		module.positions.data[length] = position;
		print!("{} ", position);
		
		length += 1;
		pos += 1;
	}
	println!();
	
	pos += 1;

	module.length = length as u8;
	
	let pattern_start_offset = pos;

	println!("offsets:");
	for i in 0..num_patterns {
		print!("  {}: ",i);
		for _ in 0..DEFAULT_NUMBER_OF_CHANNELS_PER_ROW {
			let offset = (((data[offset_pos] as u16) << 8) | data[offset_pos+1] as u16) as usize + pattern_start_offset;

			print!("{:02x}({})  ",offset,offset);
			
			offset_pos += 2;
		}
		println!();
	}


	
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
			0 => effect = (effect & 0xf00) | ((param2 & 1) * 2),
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
				let p = !param1 + 1;
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
fn p61_putdata(stream:&mut dyn Read,packed_stream:&mut dyn Write) -> Result<(),Error> {

	// input
	// pnnnnnni iiiicccc bbbbbbbb 0
	// p = compression flag
	// n = note index
	// i = instrument index
	// c = effect
	// b = effect params
	// 0 = compression info

	let mut bytes = [0u8;4];
	stream.read_exact(&mut bytes)?;

	let byte1 = bytes[0];
	let byte2 = bytes[1];
	let byte3 = bytes[2];
	let byte4 = bytes[3];

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
			packed_stream.write_all(&[byte1])?;
			packed_stream.write_all(&[byte2])?;
			packed_stream.write_all(&[byte3])?;
		} else {
			// Note and instrument
			// p1110nnn => 1 more byte follows => nnniiiii => Note and instrument, no effect
			packed_stream.write_all(&[compression_flag | 0b01110000 | (byte1 & 0b01110000) >> 4])?;
			packed_stream.write_all(&[(byte1 & 0b1111) << 4 | (byte2 & 0b11110000) >> 4])?;
		}
	} else if byte2 & 0xf == 0 {
		// empty
		// p1111111 => 0 more bytes follow => Empty row
		packed_stream.write_all(&[compression_flag | 0x7f])?;
	} else {
		// command
		// encode as only command
		// p110cccc => 1 more byte follows bbbbbbbb => Only effect
		packed_stream.write_all(&[compression_flag | 0b01100000 | byte2 & 0xf])?;
		packed_stream.write_all(&[byte3])?;
	}

	if compression_flag == 0x80 {
		packed_stream.write_all(&[byte4])?;
	}

	Ok(())
}

/// convert a packed p61 channel row to u32 for easy comparision
/// used while searching for doing LZ77 style packing
fn p61_fetchdata(packed_stream: &mut dyn Read) -> Result<u32,Error> {
	let mut data:u32;
	let mut tmp = [0u8];

	packed_stream.read_exact(&mut tmp)?;
	data = tmp[0] as u32;
	let compression_flag = data & 0x80;
	
	if data & 0b01100000 != 0b01100000 {
		// all
		packed_stream.read_exact(&mut tmp)?;
		data = data << 8 | tmp[0] as u32;
		packed_stream.read_exact(&mut tmp)?;
		data = data << 8 | tmp[0] as u32;
		if compression_flag == 0x80 {
			// read extra byte for compression
			packed_stream.read_exact(&mut tmp)?;
			data = data << 8 | tmp[0] as u32;
		}
	} else if data & 0b01110000 == 0b01100000 || 
			  data & 0b01111000 == 0b01110000 {
		// onlycmd
		// note and/or instrument
		// these have the same length
		packed_stream.read_exact(&mut tmp)?;
		data = data << 8 | tmp[0] as u32;
		if compression_flag == 0x80 {
			// read extra byte for compression
			packed_stream.read_exact(&mut tmp)?;
			data = data << 8 | tmp[0] as u32;
		}
	} else {
		// empty
		if compression_flag == 0x80 {
			// read extra byte for compression
			packed_stream.read_exact(&mut tmp)?;
			data = data << 8 | tmp[0] as u32;
			if data & 0b11000000 == 0x40 {
				// read extra byte for byte offset
				packed_stream.read_exact(&mut tmp)?;
			} else if data & 0b11000000 == 0xc0 {
				// read extra bytes for word offset
				packed_stream.read_exact(&mut tmp)?;
				packed_stream.read_exact(&mut tmp)?;
			} 
		}
	}

	Ok(data)
}


/// Write a 31 sample Amiga ProTracker mod-file as if packed with The Player
pub fn write_p61(writer: &mut dyn Write, module: &PTModule) -> Result<(),PTMFError> {
	if module.num_channels != 4 {
		return Err(PTMFError::Parse(format!("Error! Only supported for 4 channels. This module has '{}' channels", module.num_channels)));
	}

	let mut workmodule = module.clone();

	// Prepare all data first

	// First remove patterns
	// to generate binary identical files
	// we have to leave duplicate patterns
	//workmodule.remove_duplicate_patterns();
	workmodule.remove_unused_patterns();

	// Then remove samples
	workmodule.truncate_samples();
	// To generate binary identical files,
	// we have to leave duplicate samples
	//workmodule.remove_duplicate_samples();
	workmodule.remove_unused_samples();

	// Can only do this after removing samples
	// since some samples might be used in
	// the parts that gets removed
	workmodule.truncate_patterns();
	workmodule.remove_duplicate_breaks();

	// The Player uses 2 passes
	// First pass: 
	// - some bit fiddling to reorder bits, still keeping it within one u32
	// - remove empty rows
	// - remove duplicate rows

	// Second pass:
	// - encode using fewer bits
	// - LZ77 style compression

	// First pass
	let mut pattern_offsets:Vec<Vec<usize>> = Vec::new();
	let mut stream:Vec<u8> = Vec::new();

	for channel_idx in 0..4 {
		for pattern_idx in 0..workmodule.patterns.len() {
			// Save offset for this channel and this pattern
			if pattern_offsets.len() == pattern_idx {
				pattern_offsets.push(vec![0; 4]);
			}
			pattern_offsets[pattern_idx][channel_idx] = stream.len();
			let pattern = &workmodule.patterns[pattern_idx];
			let channel = &pattern.rows[0].channels[channel_idx];
			let mut encoded = encode_p61_channel(&channel);

			let mut previous_idx = stream.len();
			stream.append(&mut encoded);

			for row_nr in 1..pattern.rows.len() {
				let channel = &pattern.rows[row_nr].channels[channel_idx];
				let mut encoded = encode_p61_channel(&channel);
				
				let has_compression_info = (stream[previous_idx+0] & 0x80) == 0x80;

				let is_empty = encoded[0] == 0 && 
								encoded[1] == 0 && 
								encoded[2] == 0 && 
								encoded[3] == 0;
				let is_same = encoded[0] == (stream[previous_idx+0] & 0x7f) &&
								encoded[1] == stream[previous_idx+1] &&
								encoded[2] == stream[previous_idx+2];

				if has_compression_info {
					let compression_type = stream[previous_idx+3] & 0x80;
					if is_empty && compression_type == 0 {
						// current is empty and compression type is empty
						stream[previous_idx+3] += 1;
					} else if is_same && compression_type == 0x80 {
						// current is same and compression type is same
						stream[previous_idx+3] += 1;
					} else {
						// new data
						previous_idx = stream.len();
						stream.append(&mut encoded);	
					}
				} else {
					// no compression info
					if is_empty {
						stream[previous_idx+0] |= 0x80;
						stream[previous_idx+3] = 0b00000001;
					} else if is_same {
						stream[previous_idx+0] |= 0x80;
						stream[previous_idx+3] = 0b10000001;
					} else {
						// new data
						previous_idx = stream.len();
						stream.append(&mut encoded);	
					}
				} // if has compression info

			} // for row_nr
		} // for pattern_idx
	} // for channel_idx

	// add an extra row with end offsets for each channel
	pattern_offsets.push(vec![
		pattern_offsets[0][1],
		pattern_offsets[0][2],
		pattern_offsets[0][3],
		stream.len()]);

	let mut packed_pattern_offsets:Vec<Vec<usize>> = Vec::new();
	let mut packed_stream:Vec<u8> = Vec::new();

	let mut stream_cursor = Cursor::new(stream);
	let mut packed_stream_cursor = Cursor::new(&mut packed_stream);

	let mut first = true;
	for channel_idx in 0..4 {
		for pattern_idx in 0..workmodule.patterns.len() {
			// Save offset for this channel and this pattern
			if packed_pattern_offsets.len() == pattern_idx {
				packed_pattern_offsets.push(vec![0; 4]);
			}
			packed_pattern_offsets[pattern_idx][channel_idx] = packed_stream_cursor.position() as usize;

//			println!("=>>> channel_idx {} pattern_idx {} stream_cursor.position() {} packed_stream_cursor.position() {}",channel_idx,pattern_idx,stream_cursor.position(),packed_stream_cursor.position());

			let channel_end_offset = pattern_offsets[pattern_idx+1][channel_idx] as u64;
			while stream_cursor.position() < channel_end_offset {
//				println!("stream_cursor.position() {} packed_stream_cursor.position() {}",stream_cursor.position(),packed_stream_cursor.position());
				if first {
					first = false;
					// First time is special and can not be compressed
					p61_putdata(&mut stream_cursor, &mut packed_stream_cursor)?;
					continue;
				}

				// save current positions
				let current_packed_stream_pos = packed_stream_cursor.position();
				let current_stream_pos = stream_cursor.position();

				let mut tmp_area = Vec::new();
				let mut tmp_cursor = Cursor::new(&mut tmp_area);

				// fetch next value from uncompressed stream
				tmp_cursor.set_position(0);
				p61_putdata(&mut stream_cursor, &mut tmp_cursor)?;
				tmp_cursor.set_position(0);
				let first_src_value = p61_fetchdata(&mut tmp_cursor)?;

//				println!(" first_src_value {:x}",first_src_value);

				let mut search_cursor = Cursor::new(packed_stream_cursor.get_ref());
				search_cursor.set_position(0);

				let mut best_match = false;
				let mut best_match_offset = 0;
				let mut best_match_length = 0;
				let mut best_match_length_bytes = 0;
				let mut best_match_cursor = 0;
				while search_cursor.position() < current_packed_stream_pos {

					// save start of search position
					// this is used to calculate the number of
					// compressed bytes
					let start_search_pos = search_cursor.position();

					// fetch one value from compressed stream
					let first_compressed_value = p61_fetchdata(&mut search_cursor)?;

					if first_src_value != first_compressed_value {
						// this does not match, keep searching
						continue;
					}

					// save this position and reset stream here
					// before next search
					let next_search_pos = search_cursor.position();

					// ok, the first one matched
					let mut src_cursor = Cursor::new(stream_cursor.get_ref());
					src_cursor.set_position(stream_cursor.position());

					if src_cursor.position() >= channel_end_offset {
						// end of unpacked data for this channel and pattern
						// no point in continuing searching
						break;
					}

					if search_cursor.position() >= current_packed_stream_pos {
						// end of packed data
						// no point in continuing searching
						break;
					}

					// fetch next value from uncompressed stream
					tmp_cursor.set_position(0);
					p61_putdata(&mut src_cursor, &mut tmp_cursor)?;
					tmp_cursor.set_position(0);
					let second_src_value = p61_fetchdata(&mut tmp_cursor)?;

					// fetch one value from compressed stream
					let second_compressed_value = p61_fetchdata(&mut search_cursor)?;

					if second_src_value != second_compressed_value {
						// No match
						// Reset search position and try again
						search_cursor.set_position(next_search_pos);
						continue;
					}

					// when we get here, two values have matched
					let mut match_length = 1;

					if src_cursor.position() < channel_end_offset &&
					search_cursor.position() < current_packed_stream_pos {

						while src_cursor.position() < channel_end_offset &&
							search_cursor.position() < current_packed_stream_pos {

							// fetch next value from uncompressed stream
							tmp_cursor.set_position(0);
							let src_position_if_no_match = src_cursor.position();
							p61_putdata(&mut src_cursor, &mut tmp_cursor)?;
							tmp_cursor.set_position(0);
							let next_src_value = p61_fetchdata(&mut tmp_cursor)?;

							// fetch one value from compressed stream
//							let search_position_if_no_match = search_cursor.position();
							let next_compressed_value = p61_fetchdata(&mut search_cursor)?;

							if next_src_value != next_compressed_value {
								// no match, abort
								// but first undo cursor

								// The Player lets the pointer for the compressed stream
								// move too far
								//search_cursor.set_position(search_position_if_no_match);

								// src_cursor is not used after this
								// but lets reset it anyway
								src_cursor.set_position(src_position_if_no_match);
								break;
							}

							// If we get here we have a match
							// increase counter and keep searching
							match_length += 1;
						}
					}

					// now we need to figure out the length in bytes
					// of the current match
					let mut match_length_bytes = (search_cursor.position() - start_search_pos) as i64;
					// pointer is at least 3 bytes
					match_length_bytes -= 3;

					// The Player seems to do it this way, 
					// but it is off by 3 bytes?
					let pointer_size = current_packed_stream_pos - start_search_pos;
					if pointer_size >= 256 {
						match_length_bytes -= 1;
					}

//					println!(" potential match at start_search_pos {} match_length_bytes {} match_length {} first_src_value {:x} second_src_value {:x}", start_search_pos, match_length_bytes, match_length, first_src_value, second_src_value);

					if match_length_bytes > best_match_length_bytes {
//						println!(" new best match");
						best_match = true;
						best_match_offset = start_search_pos;
						best_match_length = match_length;
						best_match_length_bytes = match_length_bytes;
						best_match_cursor = src_cursor.position();

						// and now keep searching
					}

					// Reset search position and try again
					search_cursor.set_position(next_search_pos);
				} // while search_cursor.position() < current_packed_stream_pos 

				// now we have searched
				if best_match {
					// and we have found
					// Rewind stream
					packed_stream_cursor.set_position(current_packed_stream_pos);
					// Jump forward in stream
					stream_cursor.set_position(best_match_cursor);

					// calc pointer
					let mut delta =  current_packed_stream_pos + 3 - best_match_offset;
					if delta < 256 {
//						println!(" write best match 8 bit {:02x} {:02x} {:02x}",0xff,0b01000000 | best_match_length,(delta & 0xff));
						// 8 bit pointer
						packed_stream_cursor.write_all(&[0xff])?;
						packed_stream_cursor.write_all(&[0b01000000 | best_match_length])?;
						packed_stream_cursor.write_all(&[(delta & 0xff) as u8])?;
					} else {
//						println!(" write best match 16 bit {:02x} {:02x} {:02x} {:02x}",0xff,0b11000000 | best_match_length,((delta >> 8) & 0xff),(delta & 0xff));
						// 16 bit pointer
						delta += 1;
						packed_stream_cursor.write_all(&[0xff])?;
						packed_stream_cursor.write_all(&[0b11000000 | best_match_length])?;
						packed_stream_cursor.write_all(&[((delta >> 8) & 0xff) as u8])?;
						packed_stream_cursor.write_all(&[(delta & 0xff) as u8])?;
					}
				} else {
//					println!(" no match ");
					// did not find any mathcing data

					// rewind source stream and write to packed stream
					stream_cursor.set_position(current_stream_pos);
					p61_putdata(&mut stream_cursor, &mut packed_stream_cursor)?;
				}

				// Time to loop again and try to pack next command

			} // stream_cursor.position() < channel_end_offset
		} // for pattern_idx 
	} // for channel_idx

	// Then we write it
	let mut final_data:Vec<u8> = Vec::new();
	let mut final_data_cursor = Cursor::new(&mut final_data);

	// optional P61A
	// TODO add as an optional parameter

	// sample data offset u16
	// we don't know this until we have written
	// all the other data
	let sample_offset_position = final_data_cursor.position();
	write_big_endian_u16(&mut final_data_cursor, 0)?;


	// number of patterns u8
	write_u8(&mut final_data_cursor, workmodule.patterns.len() as u8)?;
	
	// number of samples u8 6 bits, 0x80 means 8 bit delta packed, 0x40 means 4 bit delta packed
	// TODO add sample delta packing
	write_u8(&mut final_data_cursor, workmodule.sample_info.len() as u8)?;

	// optional unpacked sample length u32 for 4 bit delta packed samples
	// TODO add sample delta packing

	// for each sample
	// sample length u16, or negative index to another sample for using the same sample data
	// finetune u8, hi bit 0x80 means sample is 4 bit delta packed
	// volume u8
	// repeat start u16 or 0xffff u16
	let mut samples_to_write = Vec::new();
	for i in 0..workmodule.sample_info.len() {
		let mut same_as_another = false;
		let mut same_index = 0;
		{
			let si = &workmodule.sample_info[i];

			if si.data.len() > 0 {
				// only compare if sample isn't empty
				for j in 0..i {
					// Check if same sample data as another sample
					let si_other = &workmodule.sample_info[j];
					if si.data == si_other.data {
						same_as_another = true;
						same_index = j;
						break;
					}
				}	
			}
		} // end the borrow of si

		let si = &mut workmodule.sample_info[i];
		if same_as_another {
			// Sample that points to another samples data
			let sample_index = !(same_index as u16 + 1) + 1;
			write_big_endian_u16(&mut final_data_cursor, sample_index)?;
			write_u8(&mut final_data_cursor, si.finetune)?;
			write_u8(&mut final_data_cursor, si.volume)?;
			let repeat_start = if si.repeat_length > 1 {
				si.repeat_start
			} else {
				0xffff
			};
			write_big_endian_u16(&mut final_data_cursor, repeat_start)?;
		} else {
			// Normal sample
			// add this to the list of samples to write
			samples_to_write.push(i);

			if si.data.len() == 0 {
				// Empty samples not allowed, must be at least one word
				si.data.push(0);
				si.data.push(0);
			}

			// length in words
			let sample_length = si.data.len() as u16 / 2;
			write_big_endian_u16(&mut final_data_cursor, sample_length)?;
			write_u8(&mut final_data_cursor, si.finetune)?;
			write_u8(&mut final_data_cursor, si.volume)?;
			let repeat_start = if si.repeat_length > 1 {
				si.repeat_start
			} else {
				0xffff
			};
			write_big_endian_u16(&mut final_data_cursor, repeat_start)?;
		}

	}

	// for each pattern
	// ch1 pattern offset u16
	// ch2 pattern offset u16
	// ch3 pattern offset u16
	// ch4 pattern offset u16
	for offsets in packed_pattern_offsets {
		write_big_endian_u16(&mut final_data_cursor, offsets[0] as u16)?;
		write_big_endian_u16(&mut final_data_cursor, offsets[1] as u16)?;
		write_big_endian_u16(&mut final_data_cursor, offsets[2] as u16)?;
		write_big_endian_u16(&mut final_data_cursor, offsets[3] as u16)?;
	}

	// for each play position
	// position u8
	for idx in 0..workmodule.length as usize {
		let position = workmodule.positions.data[idx];
		write_u8(&mut final_data_cursor, position)?;
	}

	// end of positions
	// 0xff u8
	write_u8(&mut final_data_cursor, 0xff)?;

	// compressed pattern data
	let mut packed_stream_cursor = Cursor::new(&mut packed_stream);
	packed_stream_cursor.set_position(0);
	copy(&mut packed_stream_cursor,&mut final_data_cursor)?;

	// padding 0-1 u8
	if final_data_cursor.position() % 2 == 1 {
		write_u8(&mut final_data_cursor, 0)?;
	}

	let sample_offset = final_data_cursor.position();
	final_data_cursor.set_position(sample_offset_position);
	write_big_endian_u16(&mut final_data_cursor, sample_offset as u16)?;
	final_data_cursor.set_position(sample_offset);

	// TODO optional separate file for samples

	// for each sample 
	// sample data
	for idx in samples_to_write {
		let si = &workmodule.sample_info[idx];
		// TODO check that sample length is even
		let mut sample_cursor = Cursor::new(&si.data);
		copy(&mut sample_cursor, &mut final_data_cursor)?;
	}

	final_data_cursor.set_position(0);
	copy(&mut final_data_cursor,writer)?;

	Ok(())
}


#[cfg(test)]
mod tests {
    // Note this useful idiom: importing names from outer (for mod tests) scope.
    use super::*;
	use std::fs::File;
	use std::io::BufWriter;
	use std::io::BufReader;

	fn load_module(infilename:String) -> Result<PTModule,PTMFError> {
		let file = File::open(&infilename)?;
		
		let mut reader = BufReader::new(&file);
		let module = read_mod(&mut reader, true)?;

		// Close file
		drop(file);
		Ok(module)
	}

	fn save_module(outfilename:String, module:PTModule) -> Result<(),PTMFError> {
		let file = File::create(&outfilename)?;

		let mut writer = BufWriter::new(&file);		
		write_mod(&mut writer,&module)?;

		Ok(())
	}

	#[test]
	fn test_analyze_p61() -> Result<(),PTMFError> {
		let basedir = env!("CARGO_MANIFEST_DIR");
		let p61filename = format!("{}/testdata/{}",basedir, "P61.spiderfunk.mod");
		let file = File::open(&p61filename)?;
		let mut reader = BufReader::new(&file);
		analyze_p61(&mut reader)?;

		Ok(())
	}

    #[test]
    fn test_write_p61_spiderfunk() -> Result<(),PTMFError> {
		let basedir = env!("CARGO_MANIFEST_DIR");

		let infilename = format!("{}/testdata/{}",basedir, "spiderfunk.mod");
		let mut module = load_module(infilename)?;

		let mut created:Vec<u8> = Vec::new();
		let mut writer = Cursor::new(&mut created);

		write_p61(&mut writer,&mut module)?;
		let p61filename = format!("{}/testdata/{}",basedir, "P61.spiderfunk.mod");
		let file = File::open(&p61filename)?;
		let mut reader = BufReader::new(&file);
		let mut data:Vec<u8> = Vec::new();
		reader.read_to_end(&mut data)?;

		assert!(created.len() == data.len());
		assert!(created == data);

		Ok(())
    }

    #[test]
    fn test_write_p61_leftovers() -> Result<(),PTMFError> {
		let basedir = env!("CARGO_MANIFEST_DIR");

		let infilename = format!("{}/testdata/{}",basedir, "leftovers.mod");
		let mut module = load_module(infilename)?;

		let mut created:Vec<u8> = Vec::new();
		let mut writer = Cursor::new(&mut created);

		write_p61(&mut writer,&mut module)?;

		let p61filename = format!("{}/testdata/{}",basedir, "P61.leftovers.mod");
		let file = File::open(&p61filename)?;
		let mut reader = BufReader::new(&file);
		let mut data:Vec<u8> = Vec::new();
		reader.read_to_end(&mut data)?;

		assert!(created.len() == data.len());
		assert!(created == data);

		Ok(())
    }

	#[test]
	fn test_read_p61() -> Result<(),()> {
		let basedir = env!("CARGO_MANIFEST_DIR");
		let infilename = format!("{}/testdata/{}",basedir, "P61.spiderfunk.mod");

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

		let samples = module.sample_info.iter().filter(|si| si.length > 0);
		let num_samples = samples.count();
		assert!(num_samples == 0xd);
		assert!(module.length == 25);
		assert!(module.patterns.len() == 0xd);

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
	fn test_find_duplicate_patterns() -> Result<(),PTMFError> {
		let basedir = env!("CARGO_MANIFEST_DIR");

		let infilename = format!("{}/testdata/{}",basedir, "duplicate_patterns.mod");
		let module = load_module(infilename)?;

		let duplicates = module.find_duplicate_patterns();
		assert!(duplicates.len() > 0);
		assert!(duplicates.len() == 4467);

		Ok(())
	}

	#[test]
	fn test_remove_duplicate_patterns() -> Result<(),PTMFError> {
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
	fn test_find_unused_samples_all_used() -> Result<(),PTMFError> {
		let basedir = env!("CARGO_MANIFEST_DIR");

		let infilename = format!("{}/testdata/{}",basedir, "all_used_samples.mod");
		let module = load_module(infilename)?;

		let unused = module.find_unused_samples();
		assert!(unused.len() == 0);

		Ok(())
	}

	#[test]
	fn test_find_unused_samples_no_used() -> Result<(),PTMFError> {
		let basedir = env!("CARGO_MANIFEST_DIR");

		let infilename = format!("{}/testdata/{}",basedir, "no_used_samples.mod");
		let module = load_module(infilename)?;

		let unused = module.find_unused_samples();
		assert!(unused.len() == 31);

		Ok(())
	}

	#[test]
	fn test_find_unused_samples_1() -> Result<(),PTMFError> {
		let basedir = env!("CARGO_MANIFEST_DIR");

		let infilename = format!("{}/testdata/{}",basedir, "unused_samples_1.mod");
		let module = load_module(infilename)?;

		let unused = module.find_unused_samples();
		assert!(unused.len() == 1);
		assert!(unused[0] == 1);

		Ok(())
	}

	#[test]
	fn test_find_unused_samples_2() -> Result<(),PTMFError> {
		let basedir = env!("CARGO_MANIFEST_DIR");

		let infilename = format!("{}/testdata/{}",basedir, "unused_samples_2.mod");
		let module = load_module(infilename)?;

		let unused = module.find_unused_samples();
		assert!(unused.len() == 1);
		assert!(unused[0] == 2);

		Ok(())
	}

	#[test]
	fn test_find_unused_samples_3() -> Result<(),PTMFError> {
		let basedir = env!("CARGO_MANIFEST_DIR");

		let infilename = format!("{}/testdata/{}",basedir, "unused_samples_3.mod");
		let module = load_module(infilename)?;

		let unused = module.find_unused_samples();
		assert!(unused.len() == 1);
		assert!(unused[0] == 3);

		Ok(())
	}

	#[test]
	fn test_find_unused_samples_1d() -> Result<(),PTMFError> {
		let basedir = env!("CARGO_MANIFEST_DIR");

		let infilename = format!("{}/testdata/{}",basedir, "unused_samples_1d.mod");
		let module = load_module(infilename)?;

		let unused = module.find_unused_samples();
		assert!(unused.len() == 1);
		assert!(unused[0] == 0x1d);

		Ok(())
	}

	#[test]
	fn test_find_unused_samples_1e() -> Result<(),PTMFError> {
		let basedir = env!("CARGO_MANIFEST_DIR");

		let infilename = format!("{}/testdata/{}",basedir, "unused_samples_1e.mod");
		let module = load_module(infilename)?;

		let unused = module.find_unused_samples();
		assert!(unused.len() == 1);
		assert!(unused[0] == 0x1e);

		Ok(())
	}

	#[test]
	fn test_find_unused_samples_1f() -> Result<(),PTMFError> {
		let basedir = env!("CARGO_MANIFEST_DIR");

		let infilename = format!("{}/testdata/{}",basedir, "unused_samples_1f.mod");
		let module = load_module(infilename)?;

		let unused = module.find_unused_samples();
		assert!(unused.len() == 1);
		assert!(unused[0] == 0x1f);

		Ok(())
	}

	#[test]
	fn test_remove_unused_samples_all_used() -> Result<(),PTMFError> {
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
	fn test_remove_unused_samples_no_used() -> Result<(),PTMFError> {
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
	fn test_remove_unused_samples_1() -> Result<(),PTMFError> {
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
	fn test_find_duplicate_samples_same_sample() -> Result<(),PTMFError> {
		let basedir = env!("CARGO_MANIFEST_DIR");

		let infilename = format!("{}/testdata/{}",basedir, "same_sample.mod");
		let module = load_module(infilename)?;

		let duplicates = module.find_duplicate_samples();
		assert!(duplicates.len() > 0);
		assert!(duplicates.len() == 465);

		Ok(())
	}

	#[test]
	fn test_find_duplicate_samples_same_sample_ft() -> Result<(),PTMFError> {
		let basedir = env!("CARGO_MANIFEST_DIR");

		let infilename = format!("{}/testdata/{}",basedir, "same_sample_ft.mod");
		let module = load_module(infilename)?;

		let duplicates = module.find_duplicate_samples();
		assert!(duplicates.len() > 0);
		assert!(duplicates.len() == 353);

		Ok(())
	}

	#[test]
	fn test_find_duplicate_samples_same_sample_rep() -> Result<(),PTMFError> {
		let basedir = env!("CARGO_MANIFEST_DIR");

		let infilename = format!("{}/testdata/{}",basedir, "same_sample_rep.mod");
		let module = load_module(infilename)?;

		let duplicates = module.find_duplicate_samples();
		assert!(duplicates.len() > 0);
		assert!(duplicates.len() == 406);

		Ok(())
	}

	#[test]
	fn test_remove_duplicate_samples_same_sample() -> Result<(),PTMFError> {
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
	fn test_remove_duplicate_samples_same_sample_ft() -> Result<(),PTMFError> {
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
	fn test_remove_duplicate_samples_same_sample_rep() -> Result<(),PTMFError> {
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
	fn test_truncate_samples() -> Result<(),PTMFError> {
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
	fn test_truncate_patterns() -> Result<(),PTMFError> {
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
	fn test_p61_putdata() -> Result<(),Error> {

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

		let mut data:Vec<u8> = Vec::new();
		data.extend(vec![0,0,0,0]);
		data.extend(vec![0x80,0,0,0x23]);
		data.extend(vec![4<<1,0,0,0]);
		data.extend(vec![0x80 | 4<<1,0,0,0x84]);
		data.extend(vec![4<<1,7<<4,0,0]);
		data.extend(vec![0x80 | 4<<1,7<<4,0,8]);
		data.extend(vec![0,7<<4,0,0]);
		data.extend(vec![0x80 ,7<<4,0,8]);
		data.extend(vec![0,9,0x45,0]);
		data.extend(vec![0x80,9,0x45,0x89]);
		data.extend(vec![12<<1,7<<4|0xe,0x23,0]);
		data.extend(vec![0x80|12<<1,7<<4|0xe,0x23,0x84]);

		let mut stream = Cursor::new(data);

		// empty
		let mut result:Vec<u8> = vec![];
		p61_putdata(&mut stream, &mut result)?;
		assert!(result.len() == 1);
		assert!(result[0] == 0x7f);

		// empty with compression
		let mut result:Vec<u8> = vec![];
		p61_putdata(&mut stream, &mut result)?;
		assert!(result.len() == 2);
		assert!(result[0] == 0xff);
		assert!(result[1] == 0x23);

		// Note only
		let mut result:Vec<u8> = vec![];
		p61_putdata(&mut stream, &mut result)?;
		assert!(result.len() == 2);
		assert!(result[0] == 0b01110000);
		assert!(result[1] == 4 << 5);

		// Note only and compression
		let mut result:Vec<u8> = vec![];
		p61_putdata(&mut stream, &mut result)?;
		assert!(result.len() == 3);
		assert!(result[0] == 0x80 | 0b01110000);
		assert!(result[1] == 4 << 5);
		assert!(result[2] == 0x84);

		// Note and instrument
		let mut result:Vec<u8> = vec![];
		p61_putdata(&mut stream, &mut result)?;
		assert!(result.len() == 2);
		assert!(result[0] == 0b01110000);
		assert!(result[1] == 4 << 5 | 7);

		// Note and instrument and compression
		let mut result:Vec<u8> = vec![];
		p61_putdata(&mut stream, &mut result)?;
		assert!(result.len() == 3);
		assert!(result[0] == 0x80 | 0b01110000);
		assert!(result[1] == 4 << 5 | 7);
		assert!(result[2] == 8);

		// Instrument only
		let mut result:Vec<u8> = vec![];
		p61_putdata(&mut stream, &mut result)?;
		assert!(result.len() == 2);
		assert!(result[0] == 0b01110000);
		assert!(result[1] == 7);

		// Instrument and compression
		let mut result:Vec<u8> = vec![];
		p61_putdata(&mut stream, &mut result)?;
		assert!(result.len() == 3);
		assert!(result[0] == 0x80 | 0b01110000);
		assert!(result[1] == 7);
		assert!(result[2] == 8);

		// Effect only
		let mut result:Vec<u8> = vec![];
		p61_putdata(&mut stream, &mut result)?;
		assert!(result.len() == 2);
		assert!(result[0] == 0b01100000 | 9);
		assert!(result[1] == 0x45);
		
		// Effect only and compression
		let mut result:Vec<u8> = vec![];
		p61_putdata(&mut stream, &mut result)?;
		assert!(result.len() == 3);
		assert!(result[0] == 0x80 | 0b01100000 | 9);
		assert!(result[1] == 0x45);
		assert!(result[2] == 0x89);

		// All data
		let mut result:Vec<u8> = vec![];
		p61_putdata(&mut stream, &mut result)?;
		assert!(result.len() == 3);
		assert!(result[0] == 12<<1);
		assert!(result[1] == 7<<4|0xe);
		assert!(result[2] == 0x23);

		// All data and compression
		let mut result:Vec<u8> = vec![];
		p61_putdata(&mut stream, &mut result)?;
		assert!(result.len() == 4);
		assert!(result[0] == 0x80|12<<1);
		assert!(result[1] == 7<<4|0xe);
		assert!(result[2] == 0x23);
		assert!(result[3] == 0x84);

		Ok(())
	}

	#[test]
	fn test_p61_fetchdata() -> Result<(),Error> {

		let mut encoded:Vec<u8> = Vec::new();

		encoded.extend(&vec![0x7f]);
		encoded.extend(&vec![0xff,0x23u8]);
		encoded.extend(&vec![0b01110000,4 << 5]);
		encoded.extend(&vec![0x80 | 0b01110000,4 << 5,0x84]);
		encoded.extend(&vec![0b01110000,4 << 5 | 7]);
		encoded.extend(&vec![0x80 | 0b01110000,4 << 5 | 7,8]);
		encoded.extend(&vec![0b01110000,7]);
		encoded.extend(&vec![0x80 | 0b01110000,7,8]);
		encoded.extend(&vec![0b01100000 | 9,0x45]);
		encoded.extend(&vec![0x80 | 0b01100000 | 9,0x45, 0x89]);
		encoded.extend(&vec![12<<1,7<<4|0xe, 0x23]);
		encoded.extend(&vec![0x80|12<<1,7<<4|0xe, 0x23,0x84]);
		encoded.extend(&vec![0xff,0b01000000|32,0x56]);
		encoded.extend(&vec![0xff,0b11000000|32,0x56,0x65]);

		let mut cursor = Cursor::new(encoded);

		// empty
		let res = p61_fetchdata(&mut cursor)?;
		assert!(res == 0x7f);

		// empty with compression
		let res = p61_fetchdata(&mut cursor)?;
		assert!(res == 0xff23);

		// Note only
		let res = p61_fetchdata(&mut cursor)?;
		assert!(res == 0x7080);

		// Note only and compression
		let res = p61_fetchdata(&mut cursor)?;
		assert!(res == 0xf08084);


		// Note and instrument
		let res = p61_fetchdata(&mut cursor)?;
		assert!(res == 0x7087);

		// Note and instrument and compression
		let res = p61_fetchdata(&mut cursor)?;
		assert!(res == 0xf08708);

		// Instrument only
		let res = p61_fetchdata(&mut cursor)?;
		assert!(res == 0x7007);

		// Instrument and compression
		let res = p61_fetchdata(&mut cursor)?;
		assert!(res == 0xf00708);

		// Effect only
		let res = p61_fetchdata(&mut cursor)?;
		assert!(res == 0x6945);

		// Effect only and compression
		let res = p61_fetchdata(&mut cursor)?;
		assert!(res == 0xe94589);

		// All data
		let res = p61_fetchdata(&mut cursor)?;
		assert!(res == 0x187e23);

		// All data and compression
		let res = p61_fetchdata(&mut cursor)?;
		assert!(res == 0x987e2384);

		// 8 bit back reference
		let res = p61_fetchdata(&mut cursor)?;
		assert!(res == 0xff60);

		// 16 bit back reference
		let res = p61_fetchdata(&mut cursor)?;
		assert!(res == 0xffe0);

		Ok(())
	}



}
