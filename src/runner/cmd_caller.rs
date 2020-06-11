
use std::io::Write;

use core::base_types::*;
use core::printing::*;
use core::test_theory::*;

use parser::error::LocalizedError;

use runner::proof_types::*;
use runner::stack_helper::*;

pub type LocRes = std::result::Result<(), LocalizedError>;
pub type LocSeqRes = std::result::Result<Sequent, LocalizedError>;

struct RuleCaller<'a> {
    h: &'a StackHelper<'a>,
    th: &'a TestTheory,
}

impl<'a> RuleCaller<'a> {
    fn call(&self, rule: &RulePattern) -> LocSeqRes {
        let catcher = |res: SequentResult| -> LocSeqRes {
            match res {
                Ok(res) => Ok(res),
                Err(e) => {
                    Err(LocalizedError::new(
                        rule.start,
                        rule.end,
                        &format!("{}", e)
                    ))
                }
            }
        };

        let res = match &rule.data {
            RuleData::Get(name) => self.h.get_prop(name),
            RuleData::OpDef(opname) => self.h.get_op_def(opname),
            RuleData::IntroAnd(a, b) => {
                self.th.intro_and(&self.call(a)?, &self.call(b)?)
            }
            RuleData::ElimAndL(a) => self.th.elim_and_l(&self.call(a)?),
            RuleData::ElimAndR(b) => self.th.elim_and_r(&self.call(b)?),
            RuleData::IntroOrL(a, b) => {
                self.th.intro_or_l(&self.call(a)?, &catcher(self.h.build_prop(b))?)
            },
            RuleData::IntroOrR(a, b) => {
                self.th.intro_or_r(&catcher(self.h.build_prop(a))?, &self.call(b)?)
            },
            RuleData::IntroIff(a, b) => {
                self.th.intro_iff(&self.call(a)?, &self.call(b)?)
            },
            RuleData::ElimIffL(a) => {
                self.th.elim_iff_l(&self.call(a)?)
            },
            RuleData::ElimIffR(a) => {
                self.th.elim_iff_r(&self.call(a)?)
            },
            RuleData::IntroTop() => {
                self.th.intro_top(&self.h.last_intro())
            },
            RuleData::IntroBot(a, b) => {
                self.th.intro_bot(&self.call(a)?, &self.call(b)?)
            },
            RuleData::ElimBot(a, b) => {
                self.th.elim_bot(
                    &catcher(self.h.build_prop(a))?,
                    &self.call(b)?
                )
            },

            RuleData::IntroExists(a, b, c, d) => {
                self.th.intro_exists(
                    a,
                    &catcher(self.h.build_term(c))?,
                    &catcher(self.h.intro_and_build_prop(a, b))?,
                    &self.call(d)?
                )
            },
            RuleData::IntroExistsUnique(a, b, c) => {
                self.th.intro_exists_unique(
                    a,
                    &catcher(self.h.intro_and_build_prop(a, b))?,
                    &self.call(c)?
                )
            },
            RuleData::ElimExistsUnique(a, b, c) => {
                self.th.elim_exists_unique(
                    a,
                    b,
                    &self.call(c)?
                )
            },

            RuleData::ElimThen(a, b) => {
                self.th.elim_then(&self.call(a)?, &self.call(b)?)
            },
            RuleData::ElimSoForall(b, c) => {
                self.th.elim_so_forall(
                    &catcher(self.h.build_prop(b))?,
                    &self.call(c)?
                )
            },
            RuleData::ElimForall(b, c) => {
                self.th.elim_forall(
                    &catcher(self.h.build_term(b))?,
                    &self.call(c)?
                )
            },
        };
        catcher(res)
    }
}

pub struct CmdCaller<'a> {
    h: &'a mut StackHelper<'a>,
    th: &'a TestTheory,
}

impl<'a> CmdCaller<'a> {
    pub fn new(h: &'a mut StackHelper<'a>, th: &'a TestTheory) -> CmdCaller<'a> {
        CmdCaller {h, th}
    }

    pub fn call(&mut self, out: &mut dyn Write, r: &CmdPattern) -> LocRes {
        let seq_catcher = |res| {
            match res {
                Ok(a) => Ok(a),
                Err(e) => {
                    Err(LocalizedError::new(
                        r.start,
                        r.end,
                        &format!("{}", e)
                    ))
                }
            }
        };
        let catcher = |res| {
            match res {
                Ok(()) => Ok(()),
                Err(e) => {
                    Err(LocalizedError::new(
                        r.start,
                        r.end,
                        &format!("{}", e)
                    ))
                }
            }
        };
        let res = match &r.data {
            CmdData::Clear => {
                catcher(write!(out, "{}[2J", 27 as char))?;
                catcher(write!(out, "{}[0;0H", 27 as char))?;
                catcher(writeln!(out))?;
                Ok(())
            },
            CmdData::Scope => {self.h.scope()},
            CmdData::Prop(name) => {self.h.prop(name)},
            CmdData::Hyp(name, pat) => {self.h.hyp(name, pat)},
            CmdData::Term(name) => {self.h.term(name)}
            CmdData::Instantiate(propname, varname, pat, exists) => {
                self.h.instantiate(
                    propname,
                    varname,
                    pat,
                    {
                        let caller = RuleCaller {h: &self.h, th: &self.th};
                        caller.call(exists)?
                    }
                )
            },
            CmdData::Reg(name, cmd) => {
                self.h.register_prop(name, {
                    let caller = RuleCaller {h: &self.h, th: &self.th};
                    caller.call(cmd)?
                })
            },

            CmdData::Switch(or_prop, l_name, l_prop, r_name, r_prop) => {
                self.h.switch(
                    {
                        let caller = RuleCaller {h: &self.h, th: &self.th};
                        caller.call(or_prop)?
                    },
                    l_name, l_prop,
                    r_name, r_prop
                )
            },
            CmdData::CclAndSwitch(ccl) => {
                self.h.ccl_and_switch({
                    let caller = RuleCaller {h: &self.h, th: &self.th};
                    caller.call(ccl)?
                })
            },
            CmdData::CclAndElim(name, ccl) => {
                self.h.ccl_and_elim(name, {
                    let caller = RuleCaller {h: &self.h, th: &self.th};
                    caller.call(ccl)?
                })
            }

            CmdData::MakeOp(name, places, cmd) => {
                self.h.register_op(name, name, *places, {
                    let caller = RuleCaller {h: &self.h, th: &self.th};
                    caller.call(cmd)?
                })
            },

            CmdData::Conclude(name, ccl_name) => {self.h.conclude(name, ccl_name)},
            CmdData::ConcludeOp(name, opname) => {self.h.conclude_op(name, opname)},
            CmdData::Then(name, ccl_name) => {self.h.then(name, ccl_name)},
            CmdData::SoForall(name, varname, ccl_name) => {
                self.h.so_forall(name, varname, ccl_name)
            },
            CmdData::Forall(name, varname, ccl_name) => {
                self.h.forall(name, varname, ccl_name)
            },
            CmdData::ElimExists(name, ccl_name, ccl_pat) => {
                self.h.elim_exists(name, ccl_name, ccl_pat)
            },

            CmdData::Print(name) => {
                match &seq_catcher(self.h.get_prop(name))?.ccl {
                    Some(ccl) => {
                        catcher(write!(out, "{:?} : ", name))?;
                        match print_term(out, ccl) {
                            Ok(()) => (),
                            Err(e) => return Err(LocalizedError::new(
                                r.start,
                                r.end,
                                &format!("{}", e)
                            ))
                        };
                        catcher(writeln!(out))?;
                    },
                    None => catcher(writeln!(out, "{:?} : void", name))?
                };
                Ok(())
            },
            CmdData::PrintOp(name) => {
                self.h.print_op(out, name)
            }
            CmdData::PrintAll => {
                self.h.print_all(out)
            }
        };
        match res {
            Ok(()) => Ok(()),
            Err(e) => {
                Err(LocalizedError::new(
                    r.start,
                    r.end,
                    &format!("{}", e)
                ))
            }
        }
    }
}
