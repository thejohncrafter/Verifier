
use std::rc::Rc;
use std::io::Write;

use core::base_types::*;

pub fn print_sequent(out: &mut dyn Write, seq: &Sequent) -> Result {
    if seq.hyp.is_empty() {
        write!(out, "∅")?;
    } else {
        Rc::clone(&seq.hyp).iter().collect::<Vec<_>>()
        .iter().rev().enumerate().try_for_each(|(i, j)| {
            if i != 0 {write!(out, ", ")?};
            match j {
                Jugement::Truth(t) => print_term(out, &t),
                Jugement::Intro(t) => {
                    write!(out, "var ")?;
                    t.format(out)
                },
            }
        })?
    }
    write!(out, " ⊦ ")?;
    match &seq.ccl {
        Some(t) => print_term(out, t.as_ref())?,
        None => write!(out, "∅")?
    };
    write!(out, "\n")?;
    Ok(())
}

pub fn print_term_list(out: &mut dyn Write, args: &Vec<Box<Term>>) -> Result {
    write!(out, "(")?;
    args.iter().enumerate().try_for_each(|(i, arg)| {
        if i != 0 {write!(out, ", ")?};
        print_term(out, arg)
    })?;
    write!(out, ")")?;
    Ok(())
}

pub fn print_term(out: &mut dyn Write, t: &Term) -> Result {
    match t {
        Term::Var(te) => te.format(out)?,
        Term::Qua(te, v, t) => te.format(out, v.as_ref(), t)?,
        Term::Fun(te, args) => te.format(out, args)?
    };
    Ok(())
}
