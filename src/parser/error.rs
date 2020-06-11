
use std::fmt::{Debug, Display};
use std::io::Write;

use core::base_types::Result;

use parser::tokenizer::Pos;

pub struct LocalizedError {
    start: Pos,
    end: Pos,
    message: String
}

impl LocalizedError {
    pub fn new(start: Pos, end: Pos, message: &str) -> LocalizedError {
        LocalizedError {start, end, message: message.to_string()}
    }
}

impl LocalizedError {
    fn format_pos(&self) -> String {
        if self.start.line == self.end.line {
            format!(
                "(l.{}, from {} to {})",
                self.start.line, self.start.column, self.end.column
            )
        } else {
            format!(
                "(from {}:{} to {}:{})",
                self.start.line, self.start.column,
                self.end.line, self.end.column
            )
        }
    }

    pub fn pretty_display(&self, out: &mut dyn Write, source: &str) -> Result {
        let l_start = self.start.line;
        let l_end = self.end.line;

        writeln!(out, "error : {}", self.message)?;
        if l_start == l_end {
            if let Some(l) = source.lines().nth(l_start - 1) {
                let pad = format!("{} ", l_start + 1).len();
                let under: String = (0..=l.len()).map(|i| {
                    if self.start.column <= i + 1 && i + 1 < self.end.column {
                        '^'
                    } else {
                        ' '
                    }
                }).collect();
                writeln!(out, "{:pad$} |", "", pad = pad)?;
                writeln!(out, "{:pad$} | {}", l_start, l, pad = pad)?;
                writeln!(out, "{:pad$} | {}", "", under, pad = pad)?;

                Ok(())
            } else {
                Err(std::fmt::Error)?
            }
        } else {
            let mut lines = source.lines();
            if l_start > 1 {
                lines.nth(l_start - 2);
            }
            let pad = format!("{} ", l_end + 1).len();

            let mut print_lines = |out: &mut dyn Write, start, end| -> Result {
                let mut flag = true;
                for i in start..=end {
                    if let Some(l) = lines.next() {
                        let sym = if flag {'/'} else {'\\'};
                        flag = !flag;
                        writeln!(out, "{:pad$} | {} {}", i, sym, l, pad = pad)?;
                    } else {
                        return Err(std::fmt::Error)?
                    }
                }
                Ok(())
            };

            writeln!(out, "{:pad$} |", "", pad = pad)?;
            if l_end - l_start < 6 {
                print_lines(out, l_start, l_end)?;
            } else {
                print_lines(out, l_start, l_start + 2)?;
                writeln!(out, " --- snip --- ")?;
                print_lines(out, l_end - 2, l_end)?;
            }
            writeln!(out, "{:pad$} |", "", pad = pad)?;
            Ok(())
        }
    }
}

impl Debug for LocalizedError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{} : {}", self.format_pos(), self.message)
    }
}

impl Display for LocalizedError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{} : {}", self.format_pos(), self.message)
    }
}

impl std::error::Error for LocalizedError {

}

pub type ParserResult<T> = std::result::Result<T, LocalizedError>;
