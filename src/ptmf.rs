use std::io::Write;
use std::io::Read;
use std::fmt;
use std::cmp;
use std::io;

const DEFAULT_NUMBER_OF_SAMPLES:usize = 31;
const DEFAULT_NUMBER_OF_ROWS_PER_PATTERN:usize = 64;
const DEFAULT_NUMBER_OF_CHANNELS_PER_ROW:usize = 4;
const MAGIC_MK:[u8; 4] =['M' as u8, '.' as u8, 'K' as u8, '.' as u8];
const MAGIC_FLT4:[u8; 4] =['F' as u8, 'L' as u8, 'T' as u8, '4' as u8];

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
#[derive(Debug)]
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
#[derive(Debug)]
pub struct Row {
	/// Data for each Channel
	pub channels: Vec<Channel>
}

impl Row {
	pub fn new() -> Row {
		let mut row = Row{channels: Vec::new()};
		for _ in 0..DEFAULT_NUMBER_OF_CHANNELS_PER_ROW {
			row.channels.push(Channel::new());
		}
		
		row
	}
}

/// A Pattern with multiple Rows and Channels
#[derive(Debug)]
pub struct Pattern {
	/// Data for each Channel
	pub rows: Vec<Row>
}

impl Pattern {
	pub fn new() -> Pattern {
		let mut p = Pattern{rows: Vec::new()};

		for _ in 0..DEFAULT_NUMBER_OF_ROWS_PER_PATTERN {
			p.rows.push(Row::new());
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
pub fn read_mod(reader: &mut Read) -> Result<PTModule, PTMFError> {
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
		module.mk != MAGIC_FLT4 {
		return Err(PTMFError::Parse(format!("Unknown format {:?}", module.mk)));
	}
	
	// Read all patterns
	let mut num_patterns = 0;
	for n in module.positions.data.iter() {
		num_patterns = cmp::max(num_patterns,*n);
	}
	num_patterns += 1;
	for _ in 0..num_patterns {
		let mut pattern = Pattern::new();
		for mut row in &mut pattern.rows {
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
	// Sanity check that the reader is empty
	let mut data = [0u8; 1];
	match reader.read(&mut data) {
		Ok(n) if n == data.len() => return Err(PTMFError::Parse(format!("Unread data left in file"))),
		_ => ()
	};

	Ok(module)
}
