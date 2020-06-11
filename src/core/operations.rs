
use std::rc::Rc;

use core::base_types::*;

pub fn replace_term_var(term: &mut Term, te: &Rc<dyn VarTemplate>, with: &Term) -> Result {
    struct Visitor<'a> {
        template: Rc<dyn VarTemplate>,
        with: &'a Term,
    }

    impl<'a> Visitor<'a> {
        fn visit_term(&self, term: &mut Term) -> Result {
            let res: std::result::Result<bool, Box<dyn std::error::Error>> =
                match term {
                    Term::Var(te) => {
                        Ok(Rc::ptr_eq(&self.template, &te))
                    },
                    Term::Qua(_, _, t) => {
                        self.visit_term(t)?;
                        Ok(false)
                    },
                    Term::Fun(_, args) => {
                        args.iter_mut().try_for_each(|t| {self.visit_term(t)})?;
                        Ok(false)
                    }
                };
            if res? {
                *term = self.with.clone()
            }
            Ok(())
        }
    }

    let visitor = Visitor {template: Rc::clone(te), with};
    visitor.visit_term(term)?;
    Ok(())
}
