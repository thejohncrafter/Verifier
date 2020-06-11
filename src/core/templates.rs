
use std::io::Write;

use core::base_types::*;
use core::printing::*;

pub struct Template {
    name: String,
}

impl Template {
    pub fn new(name: &str) -> Template {
        Template {name: name.to_string()}
    }
}

impl VarTemplate for Template {
    fn format(&self, out: &mut dyn Write) -> Result {
        write!(out, "{}", self.name)?;
        Ok(())
    }
}

impl TquTemplate for Template {
    fn format(&self, out: &mut dyn Write, v: &dyn VarTemplate, t: &Term) -> Result {
        write!(out, "{}", self.name)?;
        v.format(out)?;
        write!(out, ", ")?;
        print_term(out, t)
    }
}

impl FunTemplate for Template {
    fn format(&self, out: &mut dyn Write, args: &Vec<Box<Term>>) -> Result {
        write!(out, "{}", self.name)?;
        print_term_list(out, args)
    }
}

impl RelTemplate for Template {
    fn format(&self, out: &mut dyn Write, args: &Vec<Box<Term>>) -> Result {
        write!(out, "{}", self.name)?;
        print_term_list(out, args)
    }
}

pub struct BinaryTemplate {
    name: String,
    parenthesised: bool,
}

impl BinaryTemplate {
    pub fn new(name: &str, parenthesised: bool) -> BinaryTemplate {
        BinaryTemplate {name: name.to_string(), parenthesised}
    }
}

impl FunTemplate for BinaryTemplate {
    fn format(&self, out: &mut dyn Write, args: &Vec<Box<Term>>) -> Result {
        match (args.get(0), args.get(1)) {
            (Some(l), Some(r)) => {
                if self.parenthesised {write!(out, "(")?};
                print_term(out, l)?;
                if self.parenthesised {write!(out, ")")?};
                write!(out, " {} ", self.name)?;
                if self.parenthesised {write!(out, "(")?};
                print_term(out, r)?;
                if self.parenthesised {write!(out, ")")?};
                Ok(())
            },
            _ => Err(Box::new(std::fmt::Error))
        }
    }
}

impl RelTemplate for BinaryTemplate {
    fn format(&self, out: &mut dyn Write, args: &Vec<Box<Term>>) -> Result {
        match &args[..] {
            [l, r] => {
                if self.parenthesised {write!(out, "(")?};
                print_term(out, l)?;
                if self.parenthesised {write!(out, ")")?};
                write!(out, " {} ", self.name)?;
                if self.parenthesised {write!(out, "(")?};
                print_term(out, r)?;
                if self.parenthesised {write!(out, ")")?};
                Ok(())
            },
            _ => Err(Box::new(std::fmt::Error))
        }
    }
}

pub struct TypeTemplate {
    name: String
}

impl TypeTemplate {
    pub fn new(name: &str) -> TypeTemplate {
        TypeTemplate {name: name.to_string()}
    }
}

impl FunTemplate for TypeTemplate {
    fn format(&self, out: &mut dyn Write, args: &Vec<Box<Term>>) -> Result {
        match &args[..] {
            [p] => {
                print_term(out, p)?;
                write!(out, " : {}", self.name)?;
                Ok(())
            },
            _ => Err(Box::new(std::fmt::Error))
        }
    }
}

pub struct CallTemplate {
}

impl CallTemplate {
    pub fn new() -> CallTemplate {
        CallTemplate {}
    }
}

impl FunTemplate for CallTemplate {
    fn format(&self, out: &mut dyn Write, args: &Vec<Box<Term>>) -> Result {
        if args.len() == 0 {
            write!(out, "(malformed)")?;
        } else {
            print_term(out, args.get(0).unwrap())?;
            write!(out, "(")?;
            args[1..].iter().enumerate().try_for_each(|(i, arg)| -> Result {
                if i != 0 {
                    write!(out, ", ")?;
                }
                print_term(out, arg)?;
                Ok(())
            })?;
            write!(out, ")")?;
        }

        Ok(())
    }
}
