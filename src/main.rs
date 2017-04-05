/*
 * Copyright (c) 2016 Michal Srb <michalsrb@gmail.com>
 *
 * Permission is hereby granted, free of charge, to any person
 * obtaining a copy of this software and associated documentation
 * files (the "Software"), to deal in the Software without
 * restriction, including without limitation the rights to use,
 * copy, modify, merge, publish, distribute, sublicense, and/or sell
 * copies of the Software, and to permit persons to whom the
 * Software is furnished to do so, subject to the following
 * conditions:
 *
 * The above copyright notice and this permission notice shall be
 * included in all copies or substantial portions of the Software.
 *
 * THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
 * EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES
 * OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND
 * NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT
 * HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY,
 * WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING
 * FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR
 * OTHER DEALINGS IN THE SOFTWARE.
 *
 */

#![feature(rustc_private)]
#![feature(libc)]
#![feature(box_syntax)]
#![feature(box_patterns)]

extern crate getopts;
extern crate rustc;
extern crate rustc_driver;
extern crate syntax;
extern crate libc;

extern crate byteorder;

mod analyzer;
mod duchain;

use std::env;
use std::io::{ Read, Write, Error };
use std::path::{ Path, PathBuf };
use std::collections::HashMap;
use std::vec::Vec;

use syntax::codemap::{ FileLoader, RealFileLoader };

use byteorder::{ LittleEndian, ReadBytesExt };

use getopts::{ optopt, optflag, getopts, OptGroup, usage };

use analyzer::analyze;
use duchain::{ BinaryDUChainWriter, DebugDUChainWriter };


struct MyFileLoader {
    real_file_loader: RealFileLoader,
    stored_files: HashMap<PathBuf, String>
}

impl MyFileLoader {
    pub fn new() -> Self {
        MyFileLoader {
            real_file_loader: RealFileLoader,
            stored_files: HashMap::new()
        }
    }

    pub fn store_file(&mut self, path: PathBuf, content: String) {
        self.stored_files.insert(path, content);
    }

    pub fn erase_file(&mut self, path: &Path) {
        self.stored_files.remove(path);
    }
}

impl FileLoader for MyFileLoader {
    fn file_exists(&self, path: &Path) -> bool {
        self.stored_files.contains_key(path) || self.real_file_loader.file_exists(path)
    }

    fn read_file(&self, path: &Path) -> std::io::Result<String> {
        match self.stored_files.get(path) {
            Some(stored_content) => { Ok(stored_content.clone()) }
            None => {
                self.real_file_loader.read_file(path)
            }
        }
    }

    fn abs_path(&self, path: &Path) -> Option<PathBuf> {
        self.real_file_loader.abs_path(path) // This should hopefully work for the stored files too.
    }
}

enum MessageIn {
    SetFile = 0,
    Analyze = 1,
}

impl MessageIn {
    fn from_u8(n: u8) -> Option<Self> {
        if n <= 1 {
            Some(unsafe { std::mem::transmute(n) })
        } else {
            None
        }
    }
}

fn read_string<T: Read>(reader: &mut T) -> Result<String, Error> {
    let length = reader.read_u32::<LittleEndian>().unwrap() as usize;
    let mut string = String::with_capacity(length);
    reader.take(length as u64).read_to_string(&mut string)?;
    Ok(string)
}

fn read_string_list<T: Read>(reader: &mut T) -> Result<Vec<String>, Error> {
    let amount = reader.read_u32::<LittleEndian>().unwrap() as usize;
    let mut vector = Vec::with_capacity(amount);
    for _ in 0..amount {
        vector.push(read_string(reader)?);
    }
    Ok(vector)
}

fn receive() -> Result<(), Error> {
    let mut reader = std::io::stdin();

    let mut loader = box MyFileLoader::new();

    loop {
        let message = MessageIn::from_u8(reader.read_u8().unwrap());

        match message {
            Some(MessageIn::SetFile) => {
                let filename = read_string(&mut reader)?;
                let content = read_string(&mut reader)?;

                loader.store_file(PathBuf::from(filename), content);
            }
            Some(MessageIn::Analyze) => {
                let filename = read_string(&mut reader)?;
                let library_search_dirs = read_string_list(&mut reader)?;

                let duchain_writer = BinaryDUChainWriter::new(std::io::stdout());
                analyze(&filename, &library_search_dirs, duchain_writer, loader);

                return Ok(()); // For now we quit after one analysis
            }
            None => {
                writeln!(&mut std::io::stderr(), "Unknown message received!").unwrap();
                return Ok(());
            }
        }
    }
}

fn print_usage(program: &str, opts: &[OptGroup]) {
    let brief = format!("Usage: {} [options]", program);
    print!("{}", usage(&brief, opts));
}

fn main() {
    let args: Vec<String> = env::args().collect();
    let program = args[0].clone();

    let opts = &[
        optopt("d", "debug", "run debug compilation on given file", "NAME"),
        optflag("h", "help", "print this help menu")
    ];
    let matches = match getopts(&args[1..], opts) {
        Ok(m) => { m }
        Err(f) => { panic!(f.to_string()) }
    };
    if matches.opt_present("h") {
        print_usage(&program, opts);
        return;
    }

    if let Some(debug_input) = matches.opt_str("d") {
        let duchain_writer = DebugDUChainWriter::new(std::io::stdout());
        analyze(&debug_input, &[], duchain_writer, box RealFileLoader);
        return;
    }

    println!("OK");

    // TODO: Write out version

    receive().expect("Failed to receive commands.");
}
