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

use syntax::codemap::{ Loc, Pos };

use std::collections::BTreeMap;
use std::io::{ Error, Write };
use std::fmt::{ self, Display, Formatter };
use std;

use byteorder::{ LittleEndian, WriteBytesExt };


pub struct MySpan(pub Loc, pub Loc);

impl Display for MySpan {
    fn fmt(&self, f: &mut Formatter) -> fmt::Result {
        write!(f, "({}: {}:{}..{}:{})", self.0.file.name, self.0.line, self.0.col.to_usize(), self.1.line, self.1.col.to_usize())
    }
}

pub struct MyDefId(pub u64);

impl Display for MyDefId {
    fn fmt(&self, f: &mut Formatter) -> fmt::Result {
        if self.0 == std::u64::MAX {
            write!(f, "unknown")
        } else {
            write!(f, "{}", self.0)
        }
    }
}

impl Into<u64> for MyDefId {
    fn into(self) -> u64 {
        self.0
    }
}

#[derive(Debug)]
pub enum DeclarationKind {
    Instance = 0,
    Function = 1,
    Type = 2,
    Namespace = 3
}

enum DeclarationFlags {
    IsDefinition = 0x1,
    UseLastType = 0x2
}

#[derive(Debug)]
pub enum ContextKind {
    Namespace = 0,
    Class = 1,
    Function = 2,
    Other = 3
}

enum ContextFlags {
    BelongsToLastDeclaration = 0x1
}

#[derive(Debug)]
pub enum TypeKind {
    U8 = 0,
    U16 = 1,
    U32 = 2,
    U64 = 3,
    I8 = 4,
    I16 = 5,
    I32 = 6,
    I64 = 7,
    Usize = 8,
    Isize = 9,
    F32 = 10,
    F64 = 11,
    Bool = 12,
    Char = 13,
    Str = 14,
    Array = 15,
    Ref = 16,
    RawPtr = 17,
    Struct = 18,
    Tuple = 19,
    Function = 20
}

pub trait DUChainWriter {
    fn write_start(&mut self) -> Result<(), Error>;
    fn write_stop(&mut self) -> Result<(), Error>;

    fn build_type(&mut self, kind: TypeKind, def_id: MyDefId, size: usize) -> Result<(), Error>;
    fn build_declaration(&mut self, kind: DeclarationKind, def_id: MyDefId, name: &str, span: &MySpan, is_definition: bool, use_last_type: bool) -> Result<(), Error>;
    fn open_context(&mut self, kind: ContextKind, name: &str, span: &MySpan, belongs_to_last_declaration: bool) -> Result<(), Error>;
    fn close_context(&mut self) -> Result<(), Error>;
    fn build_use(&mut self, def_id: MyDefId, span: &MySpan) -> Result<(), Error>;
    fn assign_name(&mut self, def_id: MyDefId, name: &str) -> Result<(), Error>;
}

pub struct DebugDUChainWriter<T: Write> {
    out: T,
    indentation: u8
}

impl<T> DebugDUChainWriter<T> where T: Write {
    pub fn new(out: T) -> Self {
        DebugDUChainWriter {
            out: out,
            indentation: 0
        }
    }

    fn write_indentation(&mut self) -> Result<(), Error> {
        // TODO: Better way?
        for _ in 0..self.indentation {
            write!(self.out, "  ")?;
        }

        Ok(())
    }
}

impl<T> DUChainWriter for DebugDUChainWriter<T> where T: Write {
    fn write_start(&mut self) -> Result<(), Error> {
        writeln!(self.out, "Start mark")?;
        Ok(())
    }

    fn write_stop(&mut self) -> Result<(), Error> {
        writeln!(self.out, "Stop mark")?;
        Ok(())
    }

    fn build_type(&mut self, kind: TypeKind, def_id: MyDefId, size: usize) -> Result<(), Error> {
        self.write_indentation()?;

        writeln!(self.out, "Build type: kind={:?} def_id={} size={}", kind, def_id, size)?;
        Ok(())
    }

    fn build_declaration(&mut self, kind: DeclarationKind, def_id: MyDefId, name: &str, span: &MySpan, is_definition: bool, use_last_type: bool) -> Result<(), Error> {
        self.write_indentation()?;

        writeln!(self.out, "Build declaration: kind={:?} def_id={} name=\"{}\" span={} is_definition={} use_last_type={}", kind, def_id, name, span, is_definition, use_last_type)?;
        Ok(())
    }

    fn open_context(&mut self, kind: ContextKind, name: &str, span: &MySpan, belongs_to_last_declaration: bool) -> Result<(), Error> {
        self.write_indentation()?;

        writeln!(self.out, "Open context: kind={:?} name=\"{}\" span={} belongs_to_last_declaration={} {{", kind, name, span, belongs_to_last_declaration)?;

        self.indentation += 1;

        Ok(())
    }

    fn close_context(&mut self) -> Result<(), Error> {
        self.indentation -= 1;

        self.write_indentation()?;

        writeln!(self.out, "}}")?;
        Ok(())
    }

    fn build_use(&mut self, def_id: MyDefId, span: &MySpan) -> Result<(), Error> {
        self.write_indentation()?;

        writeln!(self.out, "Build use: def_id={} span={}", def_id, span)?;
        Ok(())
    }

    fn assign_name(&mut self, def_id: MyDefId, name: &str) -> Result<(), Error> {
        self.write_indentation()?;

        writeln!(self.out, "Assign name: def_id={} name={}", def_id, name)?;
        Ok(())
    }
}


enum MessageOut {
    StartMark = 0,
    StopMark = 1,
    BuildType = 2,
    BuildDeclaration = 3,
    OpenContext = 4,
    CloseContext = 5,
    BuildUse = 6,
    AssignName = 7
}

pub struct BinaryDUChainWriter<T: Write> {
    out: T,
    file_to_id: BTreeMap<String, u32>
}

impl<T> BinaryDUChainWriter<T> where T: Write {
    pub fn new(out: T) -> Self {
        BinaryDUChainWriter {
            out: out,
            file_to_id: BTreeMap::new()
        }
    }

    fn write_string(&mut self, string: &str) -> Result<(), Error> {
        self.out.write_u32::<LittleEndian>(string.len() as u32)?;
        self.out.write(string.as_bytes())?;
        Ok(())
    }

    fn write_cspan(&mut self, cspan: &MySpan) -> Result<(), Error> {
        let file_id = self.file_to_id.get(&cspan.0.file.name).map(|id| { *id });
        match file_id {
            Some(file_id) => {
                self.out.write_u32::<LittleEndian>(file_id)?;
            }
            None => {
                let new_file_id = self.file_to_id.len();
                self.file_to_id.insert(cspan.0.file.name.clone(), new_file_id as u32);
                self.out.write_u32::<LittleEndian>(new_file_id as u32)?;
                self.write_string(&cspan.0.file.name)?;
            }
        };

        self.out.write_u32::<LittleEndian>(cspan.0.line as u32)?;
        self.out.write_u32::<LittleEndian>(cspan.0.col.to_usize() as u32)?;
        self.out.write_u32::<LittleEndian>(cspan.1.line as u32)?;
        self.out.write_u32::<LittleEndian>(cspan.1.col.to_usize() as u32)?;
        Ok(())
    }
}

impl<T> DUChainWriter for BinaryDUChainWriter<T> where T: Write {
    fn write_start(&mut self) -> Result<(), Error> {
        self.out.write_u8(MessageOut::StartMark as u8)
    }

    fn write_stop(&mut self) -> Result<(), Error> {
        self.out.write_u8(MessageOut::StopMark as u8)
    }

    fn build_type(&mut self, kind: TypeKind, def_id: MyDefId, size: usize) -> Result<(), Error> {
        self.out.write_u8(MessageOut::BuildType as u8)?;
        self.out.write_u8(kind as u8)?;
        self.out.write_u64::<LittleEndian>(def_id.into())?;
        self.out.write_u32::<LittleEndian>(size as u32)?;
        Ok(())
    }

    fn build_declaration(&mut self, kind: DeclarationKind, def_id: MyDefId, name: &str, span: &MySpan, is_definition: bool, use_last_type: bool) -> Result<(), Error> {
        self.out.write_u8(MessageOut::BuildDeclaration as u8)?;
        self.out.write_u8(kind as u8)?;

        let mut flags: u8 = 0;
        if is_definition { flags |= DeclarationFlags::IsDefinition as u8; }
        if use_last_type { flags |= DeclarationFlags::UseLastType as u8; }
        self.out.write_u8(flags)?;

        self.out.write_u64::<LittleEndian>(def_id.into())?;

        self.write_cspan(span)?;
        self.write_string(name)?;
        Ok(())
    }

    fn open_context(&mut self, kind: ContextKind, name: &str, span: &MySpan, belongs_to_last_declaration: bool) -> Result<(), Error> {
        self.out.write_u8(MessageOut::OpenContext as u8)?;
        self.out.write_u8(kind as u8)?;

        let mut flags: u8 = 0;
        if belongs_to_last_declaration { flags |= ContextFlags::BelongsToLastDeclaration as u8; }
        self.out.write_u8(flags)?;

        self.write_cspan(span)?;
        self.write_string(name)?;
        Ok(())
    }

    fn close_context(&mut self) -> Result<(), Error> {
        self.out.write_u8(MessageOut::CloseContext as u8)?;
        Ok(())
    }

    fn build_use(&mut self, def_id: MyDefId, span: &MySpan) -> Result<(), Error> {
        self.out.write_u8(MessageOut::BuildUse as u8)?;
        self.out.write_u64::<LittleEndian>(def_id.into())?;
        self.write_cspan(span)?;
        Ok(())
    }

    fn assign_name(&mut self, def_id: MyDefId, name: &str) -> Result<(), Error> {
        self.out.write_u8(MessageOut::AssignName as u8)?;
        self.out.write_u64::<LittleEndian>(def_id.into())?;
        self.write_string(name)?;
        Ok(())
    }
}
