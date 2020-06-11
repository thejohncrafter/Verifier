
use std::rc::Rc;
use std::collections::HashMap;

use core::base_types::*;
use core::operations::*;

pub enum TermPattern {
    Meta(String),
    Var(String),
    Qua(Rc<dyn TquTemplate>, String, Box<TermPattern>),
    Fun(Rc<dyn FunTemplate>, Vec<TermPattern>),
    Replace(Box<TermPattern>, String, Box<TermPattern>),
}

pub enum JugementPattern {
    Intro(String),
    Truth(TermPattern)
}

pub enum HypPattern {
    Meta(String),
    Void,
    Cons(JugementPattern, Box<HypPattern>)
}

pub struct SequentPattern {
    hyp: HypPattern,
    ccl: Option<TermPattern>,
}

impl SequentPattern {
    pub fn new(hyp: HypPattern, ccl: Option<TermPattern>) -> SequentPattern {
        SequentPattern {hyp, ccl}
    }
}

pub struct Rule {
    vars: Vec<String>,
    funs: Vec<String>,
    hyps: Vec<SequentPattern>,
    ccl: SequentPattern,
}

impl Rule {
    pub fn new(
        vars: Vec<String>,
        funs: Vec<String>,
        hyps: Vec<SequentPattern>,
        ccl: SequentPattern
    ) -> Rule {
        Rule {vars, funs, hyps, ccl}
    }
}

pub struct Matcher<'a> {
    meta_terms: HashMap<String, &'a Term>,
    meta_vars: HashMap<String, Rc<dyn VarTemplate>>,
    meta_funs: HashMap<String, Rc<dyn FunTemplate>>,
    meta_hyps: HashMap<String, Rc<HypStack>>,
}

impl<'a> Matcher<'a> {
    pub fn new() -> Matcher<'a> {
        Matcher {
            meta_terms: HashMap::new(),
            meta_vars: HashMap::new(),
            meta_funs: HashMap::new(),
            meta_hyps: HashMap::new(),
        }
    }

    fn malformed() -> Result {
        Err(Error::create("Malformed sequent."))
    }

    pub fn match_rule(
        rule: &Rule,
        vars: Vec<Rc<dyn VarTemplate>>,
        funs: Vec<Rc<dyn FunTemplate>>,
        input: Vec<&Sequent>
    ) -> SequentResult {
        let mut matcher = Matcher::new();

        if
            rule.vars.len() != vars.len() ||
            rule.funs.len() != funs.len() ||
            rule.hyps.len() != input.len()
        {
            return Err(Error::create("Malformed input."))
        }

        rule.vars.iter().zip(vars).for_each(|(name, te)| {
            matcher.register_var(name.clone(), Rc::clone(&te))
        });
        rule.funs.iter().zip(funs).for_each(|(name, te)| {
            matcher.register_fun(name.clone(), te)
        });
        rule.hyps.iter().zip(input).try_for_each(|(pat, seq)| {
            matcher.match_sequent(pat, seq)
        })?;

        matcher.build_sequent(&rule.ccl)
    }

    pub fn register_var(&mut self, name: String, te: Rc<dyn VarTemplate>) {
        self.meta_vars.insert(name, te);
    }

    pub fn register_fun(&mut self, name: String, te: Rc<dyn FunTemplate>) {
        self.meta_funs.insert(name, te);
    }

    fn match_term(&mut self, pattern: &TermPattern, t: &'a Term) -> Result {
        match (pattern, t) {
            (TermPattern::Meta(name), _) => {
                match self.meta_terms.get(name) {
                    None => {
                        self.meta_terms.insert(name.clone(), t);
                        Ok(())
                    },
                    Some(other) => {
                        if *other == t {
                            Ok(())
                        } else {
                            Matcher::malformed()
                        }
                    }
                }
            },
            (TermPattern::Var(name), Term::Var(v)) => {
                match self.meta_vars.get(name) {
                    None => {
                        self.meta_vars.insert(name.clone(), Rc::clone(v));
                        Ok(())
                    },
                    Some(other) => {
                        if Rc::ptr_eq(v, other) {
                            Ok(())
                        } else {
                            Matcher::malformed()
                        }
                    }
                }
            },
            (TermPattern::Qua(te_pat, name, t_pat), Term::Qua(te, v, t)) => {
                if Rc::ptr_eq(te_pat, te) {
                    Ok(())
                } else {
                    Matcher::malformed()
                }?;
                match self.meta_vars.get(name) {
                    None => {
                        self.meta_vars.insert(name.clone(), Rc::clone(v));
                        Ok(())
                    },
                    Some(other) => {
                        if Rc::ptr_eq(v, other) {
                            Ok(())
                        } else {
                            Matcher::malformed()
                        }
                    }
                }?;
                self.match_term(t_pat, t)
            },
            (TermPattern::Fun(te, args), Term::Fun(te2, args2)) => {
                if
                    Rc::ptr_eq(te, te2) &&
                    args.len() == args2.len() &&
                    args.iter().zip(args2).all(|(p, t)| {
                        self.match_term(p, t).is_ok()
                    })
                {
                    Ok(())
                } else {
                    Matcher::malformed()
                }
            },
            (TermPattern::Replace(term_pat, name, with_pat), _) => {
                match self.meta_vars.get(name) {
                    None => Matcher::malformed(),
                    Some(te) => {
                        match (self.build_term(term_pat), self.build_term(with_pat)) {
                            (Some(mut term), Some(with)) => {
                                let term = term.as_mut();
                                replace_term_var(
                                    term,
                                    te,
                                    &*with
                                )?;
                                if term == t {
                                    Ok(())
                                } else {
                                    Matcher::malformed()
                                }
                            },
                            _ => Err(Error::create(
                                "Could not build terms, this is due to bad configuration."
                            ))
                        }
                    }
                }
            },
            _ => Matcher::malformed()
        }
    }

    fn match_jugement(&mut self, pattern: &JugementPattern, j: &'a Jugement) -> Result {
        match (pattern, j) {
            (JugementPattern::Intro(name), Jugement::Intro(te)) => {
                match self.meta_vars.get(name) {
                    None => {
                        self.meta_vars.insert(name.clone(), Rc::clone(te));
                        Ok(())
                    },
                    Some(other) => {
                        if Rc::ptr_eq(te, other) {
                            Ok(())
                        } else {
                            Matcher::malformed()
                        }
                    }
                }
            },
            (JugementPattern::Truth(pat), Jugement::Truth(f)) =>
                self.match_term(pat, f),
            _ => Matcher::malformed()
        }
    }

    fn match_hyp(&mut self, pattern: &HypPattern, hyp: &'a Rc<HypStack>) -> Result {
        match (pattern, hyp.as_ref()) {
            (HypPattern::Meta(name), _) => {
                match self.meta_hyps.get(name) {
                    None => {
                        self.meta_hyps.insert(name.clone(), Rc::clone(hyp));
                        Ok(())
                    },
                    Some(other) => {
                        if other == hyp {
                            Ok(())
                        } else {
                            Matcher::malformed()
                        }
                    }
                }
            },
            (HypPattern::Void, HypStack::Void) => {
                Ok(())
            },
            (HypPattern::Cons(j_pat, next_pat), HypStack::Cons(j, next)) => {
                self.match_hyp(next_pat, next)?;
                self.match_jugement(j_pat, j)?;
                Ok(())
            },
            _ => Matcher::malformed()
        }
    }

    pub fn match_sequent(&mut self, pattern: &SequentPattern, seq: &'a Sequent) -> Result {
        self.match_hyp(&pattern.hyp, &seq.hyp)?;
        match (&pattern.ccl, &seq.ccl) {
            (None, None) => Ok(()),
            (Some(pat), Some(ccl)) => self.match_term(pat, ccl),
            _ => Matcher::malformed()
        }
    }

    fn build_term(&self, pattern: &TermPattern) -> Option<Box<Term>> {
        match pattern {
            TermPattern::Meta(name) => {
                if let Some(t) = self.meta_terms.get(name) {
                    Some(Box::new((*t).clone()))
                } else {
                    None
                }
            },
            TermPattern::Var(name) => {
                if let Some(v) = self.meta_vars.get(name) {
                    Some(Box::new(Term::Var(Rc::clone(v))))
                } else {
                    None
                }
            }
            TermPattern::Qua(te, name, pat) => {
                Some(Box::new(Term::Qua(
                    Rc::clone(te),
                    Rc::clone(self.meta_vars.get(name)?),
                    self.build_term(pat)?
                )))
            },
            TermPattern::Fun(te, args) => {
                let mut built_args: Vec<Box<Term>> = Vec::with_capacity(args.len());
                args.iter().try_for_each(|p| {
                    built_args.push(self.build_term(p)?);
                    Ok(())
                })?;
                Some(Box::new(Term::Fun(Rc::clone(te), built_args)))
            },
            TermPattern::Replace(term_pat, name, with_pat) => {
                let te = self.meta_vars.get(name)?;
                let mut term = *(self.build_term(term_pat)?);
                match replace_term_var(
                    &mut term, te, self.build_term(with_pat)?.as_ref()
                ) {
                    Ok(_) => Some(()),
                    Err(_) => None
                }?;
                Some(Box::new(term))
            }
        }
    }

    fn build_jugement(&self, pattern: &JugementPattern) -> Option<Jugement> {
        Some(match pattern {
            JugementPattern::Intro(name) => Jugement::Intro(Rc::clone(self.meta_vars.get(name)?)),
            JugementPattern::Truth(pat) => Jugement::Truth(self.build_term(pat)?)
        })
    }

    fn build_hyp(&self, pattern: &HypPattern) -> Option<Rc<HypStack>> {
        match pattern {
            HypPattern::Meta(name) => {
                if let Some(hyp) = self.meta_hyps.get(name) {
                    Some(Rc::clone(hyp))
                } else {
                    None
                }
            },
            HypPattern::Void => {
                Some(Rc::new(HypStack::Void))
            },
            HypPattern::Cons(j_pat, next_pat) =>
                Some(Rc::new(self.build_hyp(next_pat)?.cons(self.build_jugement(j_pat)?)))
        }
    }

    pub fn build_sequent(&self, pattern: &SequentPattern) -> SequentResult {
        let err_msg = "Could not build sequent, this is due to bad configuration.";
        Ok(Sequent {
            hyp: if let Some(hyp) = self.build_hyp(&pattern.hyp) {
                hyp
            } else {
                return Err(Error::create(err_msg))
            },
            ccl: match &pattern.ccl {
                None => None,
                Some(pat) => Some(if let Some(t) = self.build_term(pat) {
                    t
                } else {
                    return Err(Error::create(err_msg))
                })
            }
        })
    }
}
