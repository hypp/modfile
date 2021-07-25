[![Build Status](https://travis-ci.org/hypp/modfile.svg?branch=master)](https://travis-ci.org/hypp/modfile)

# About
This is a Rust crate for reading and writing Amiga ProTracker MOD-files.
It can also read (some?) MOD-files packed with The Player 6.1 , including 
8-bit and 4-bit delta packed samples. It can also generate The Player 6.1
compatible files.

Please feel free to report bugs and contribute in anyway you like.

# License
Released under MIT License, please see the file LICENSE.

# Usage
See the examples dir, but basically it is:

```
...
extern crate modfile;
use modfile::ptmf;
...
let mut reader = BufReader::new(&file);
let mut module = ptmf::read_mod(&mut reader)?;
...
let mut reader_p61 = BufReader::new(&file_p61);
let mut module_p61 = ptmf::read_p61(&mut reader_p61)?;
...
let mut writer = BufWriter::new(&new_file);
ptmf::write_mod(&mut writer, &mut module).?;
...
let mut writer = BufWriter::new(&new_file);
ptmf::write_p61(&mut writer, &mut module).?;
...
```

And put this in your Cargo.toml:
```
...
[dependencies.modfile]
git = "https://github.com/hypp/modfile"
...
```

