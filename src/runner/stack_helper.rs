
use std::rc::Rc;
use std::collections::BTreeMap;
use std::io::Write;

use core::printing::*;
use core::base_types::*;
use core::test_theory::*;

use runner::proof_types::*;

struct InstanceInfo {
    varname: String,
    propname: String,
    var_intro: Sequent,
    prop_intro: Sequent,
    exists: Sequent
}

#[derive(PartialEq, Eq)]
enum SwitchState {
    L, R,
}

struct SwitchInfo {
    or_seq: Sequent,
    l_name: String,
    l_is_prop: Sequent,
    l_ccl: Option<Sequent>,
    r_name: String,
    r_is_prop: Sequent,
    state: SwitchState,
}

enum IntroInfo {
    Scope,
    Prop(String),
    Hyp(String),
    Term(String),
    Instance(InstanceInfo),
    Switch(SwitchInfo),
}

#[derive(Clone)]
struct OpInfo {
    decl_seq: Sequent,
    app_seq: Sequent,
    places: usize,
}

struct SeqScope {
    info: IntroInfo,
    intro_seq: Sequent,
    user_props: BTreeMap<String, Sequent>,
    user_ops: BTreeMap<String, OpInfo>,
}

pub struct StackHelper<'a> {
    th: &'a TestTheory,
    root: Sequent,
    global: BTreeMap<String, Sequent>,
    global_ops: BTreeMap<String, OpInfo>,
    hyps: Vec<SeqScope>,
}

#[derive(PartialEq, Eq)]
enum VarType {
    Prop,
    Term,
}

impl<'a> StackHelper<'a> {
    pub fn new(th: &TestTheory) -> StackHelper {
        StackHelper {
            th,
            root: Sequent {hyp: Rc::new(HypStack::new()), ccl: None},
            global: BTreeMap::new(),
            global_ops: BTreeMap::new(),
            hyps: vec!()
        }
    }

    pub fn last_intro(&self) -> &Sequent {
        match self.hyps.last() {
            Some(SeqScope {intro_seq, ..}) => &intro_seq,
            None => &self.root
        }
    }

    pub fn scope(&mut self) -> Result {
        self.hyps.push(SeqScope {
            info: IntroInfo::Scope,
            intro_seq: self.last_intro().clone(),
            user_props: BTreeMap::new(),
            user_ops: BTreeMap::new(),
        });
        Ok(())
    }

    pub fn prop(&mut self, name: &str) -> Result {
        let s = self.th.var_is_prop(name, self.last_intro())?;
        self.hyps.push(SeqScope {
            info: IntroInfo::Prop(name.to_string()),
            intro_seq: s,
            user_props: BTreeMap::new(),
            user_ops: BTreeMap::new(),
        });
        Ok(())
    }

    pub fn hyp(&mut self, name: &str, pat: &PropPattern) -> Result {
        let hyp_is_prop = self.build_prop(pat)?;
        let s = self.th.intro_hyp(&hyp_is_prop)?;
        self.hyps.push(SeqScope {
            info: IntroInfo::Hyp(name.to_string()),
            intro_seq: s,
            user_props: BTreeMap::new(),
            user_ops: BTreeMap::new(),
        });
        Ok(())
    }

    pub fn term(&mut self, name: &str) -> Result {
        let s = self.th.var_is_term(name, self.last_intro())?;
        self.hyps.push(SeqScope {
            info: IntroInfo::Term(name.to_string()),
            intro_seq: s,
            user_props: BTreeMap::new(),
            user_ops: BTreeMap::new(),
        });
        Ok(())
    }

    pub fn instantiate(
        &mut self, propname: &str, varname: &str, pat: &PropPattern, exists: Sequent
    ) -> Result {
        let hyp_is_prop = self.build_prop(&PropPattern::Exists(
            varname.to_string(),
            Box::new(pat.clone())
        ))?;
        let check_s = self.th.push_hyp(&self.th.intro_hyp(&hyp_is_prop)?, 0)?;

        if check_s.ccl == exists.ccl {
            let decl_var = self.th.var_is_term(varname, self.last_intro())?;
            let hyp_is_prop = self.build_prop_internals(
                &decl_var,
                &mut vec!((varname, VarType::Term, decl_var.clone())),
                pat
            )?;
            let intro_hyp = self.th.intro_hyp(&hyp_is_prop)?;
            self.hyps.push(SeqScope {
                info: IntroInfo::Instance(InstanceInfo {
                    varname: varname.to_string(),
                    propname: propname.to_string(),
                    var_intro: decl_var,
                    prop_intro: intro_hyp.clone(),
                    exists
                }),
                intro_seq: intro_hyp,
                user_props: BTreeMap::new(),
                user_ops: BTreeMap::new(),
            });
            Ok(())
        } else {
            Err(Error::create("The given property does not match the pattern."))
        }
    }

    pub fn switch(
        &mut self,
        or_seq: Sequent,
        l_name: &str, l_pat: &PropPattern,
        r_name: &str, r_pat: &PropPattern
    ) -> Result {
        let l_is_prop = self.build_prop(l_pat)?;
        let r_is_prop = self.build_prop(r_pat)?;
        let l_and_r = self.th.or_is_prop(&l_is_prop, &r_is_prop)?;
        let l_and_r_hyp = self.th.push_hyp(&self.th.intro_hyp(&l_and_r)?, 0)?;

        let l_hyp = self.th.intro_hyp(&l_is_prop)?;

        if or_seq.ccl == l_and_r_hyp.ccl {
            self.hyps.push(SeqScope {
                info: IntroInfo::Switch(SwitchInfo {
                    or_seq,
                    l_name: l_name.to_string(), l_is_prop,
                    r_name: r_name.to_string(), r_is_prop,
                    l_ccl: None,
                    state: SwitchState::L,
                }),
                intro_seq: l_hyp,
                user_props: BTreeMap::new(),
                user_ops: BTreeMap::new(),
            });
            Ok(())
        } else {
            Err(Error::create(
                &format!("{} {}",
                    "The given \"or\" property does not match the given",
                    "\"l\" and \"r\" properties."
                )
            ))
        }
    }

    pub fn ccl_and_switch(&mut self, ccl: Sequent) -> Result {
        if let Some(SeqScope {info: IntroInfo::Switch(info), ..}) = self.hyps.last() {
            if let SwitchInfo {state: SwitchState::L, ..} = info {

            } else {
                return Err(Error::create("Already switched."))
            }
        } else {
            return Err(Error::create("Not switching."))
        }

        match self.hyps.pop() {
            Some(SeqScope {info: IntroInfo::Switch(SwitchInfo {
                or_seq,
                l_name, l_is_prop,
                r_name, r_is_prop,
                ..
            }), ..}) => {
                let r_hyp = self.th.intro_hyp(&r_is_prop)?;
                self.hyps.push(SeqScope {
                    info: IntroInfo::Switch(SwitchInfo {
                        or_seq,
                        l_name, l_is_prop,
                        r_name, r_is_prop,
                        l_ccl: Some(ccl),
                        state: SwitchState::R,
                    }),
                    intro_seq: r_hyp,
                    user_props: BTreeMap::new(),
                    user_ops: BTreeMap::new(),
                });
                Ok(())
            },
            _ => panic!()
        }
    }

    fn build_term_internals(
        &self,
        intro_seq: &Sequent,
        added_vars: &mut Vec<(&str, VarType, Sequent)>,
        pat: &TermPattern
    ) -> SequentResult {
        match pat {
            TermPattern::Var(name) => {
                let res =
                if let Some((_, _, s)) = added_vars.iter().rev().find(|(name1, vartype, _)| {
                    *vartype == VarType::Term && name == name1
                }) {
                    Some(s)
                } else {
                    let mut i = self.hyps.len();
                    loop {
                        if i == 0 {
                            break None
                        }
                        i = i - 1;
                        if let Some(SeqScope {info, intro_seq, ..}) = self.hyps.get(i) {
                            if let IntroInfo::Term(name1) = info {
                                if name == name1 {
                                    break Some(intro_seq)
                                }
                            }
                            if let IntroInfo::Instance(
                                InstanceInfo {varname, var_intro, ..}
                            ) = info {
                                if name == varname {
                                    break Some(var_intro)
                                }
                            }
                        }
                    }
                };

                if let Some(s) = res {
                    let decl_base = self.th.push_hyp(s, 0)?;
                    let decl = self.th.share_ccl(&decl_base, &intro_seq)?;
                    Ok(decl)
                } else {
                    Err(Error::create(&format!("Could not find term {:?}", name)))
                }
            },

            TermPattern::OpCall(opname, args) => {
                if let Some(OpInfo {places, decl_seq, ..}) = self.get_op(opname) {
                    let mut arg_seqs = Vec::with_capacity(args.len());
                    args.iter().try_for_each(|pat| -> Result {
                        arg_seqs.push(
                            self.build_term_internals(intro_seq, added_vars, pat.as_ref())?
                        );
                        Ok(())
                    })?;
                    let decl = self.th.share_ccl(decl_seq, &intro_seq)?;
                    Ok(self.th.op_is_term(*places, &decl, arg_seqs)?)
                } else {
                    Err(Error::create(&format!("Can't find operator \"{}\".", opname)))
                }
            },
        }
    }

    fn build_prop_internals<'b>(
        &self,
        intro_seq: &Sequent,
        added_vars: &mut Vec<(&'b str, VarType, Sequent)>,
        pat: &'b PropPattern
    ) -> SequentResult {
        match pat {
            PropPattern::Var(name) => {
                let res =
                if let Some((_, _, s)) = added_vars.iter().rev().find(|(name1, vartype, _)| {
                    *vartype == VarType::Prop && name == name1
                }) {
                    Some(s)
                } else {
                    let mut i = self.hyps.len();
                    loop {
                        if i == 0 {
                            break None
                        }
                        i = i - 1;
                        if let Some(SeqScope {info, intro_seq, ..}) = self.hyps.get(i) {
                            if let IntroInfo::Prop(name1) = info {
                                if name == name1 {
                                    break Some(intro_seq)
                                }
                            }
                            if let IntroInfo::Instance(
                                InstanceInfo {propname, prop_intro, ..}
                            ) = info {
                                if name == propname {
                                    break Some(prop_intro)
                                }
                            }
                        }
                    }
                };
                if let Some(s) = res {
                    let decl_base = self.th.push_hyp(s, 0)?;
                    let decl = self.th.share_ccl(&decl_base, &intro_seq)?;
                    Ok(decl)
                } else {
                    Err(Error::create(&format!("Could not find prop {:?}", name)))
                }
            },

            PropPattern::Top() => {
                let decl = self.th.top_is_prop(intro_seq)?;
                Ok(decl)
            },
            PropPattern::Bot() => {
                let decl = self.th.bot_is_prop(intro_seq)?;
                Ok(decl)
            },

            PropPattern::SoForall(varname, pat_in) => {
                let p_decl = self.th.var_is_prop(varname, intro_seq)?;
                added_vars.push((varname, VarType::Prop, p_decl.clone()));
                let in_is_prop = self.build_prop_internals(&p_decl, added_vars, pat_in)?;
                added_vars.pop();
                let decl = self.th.so_forall_is_prop(varname, &in_is_prop)?;
                Ok(decl)
            },
            PropPattern::Forall(varname, pat_in) => {
                let t_decl = self.th.var_is_term(varname, intro_seq)?;
                added_vars.push((varname, VarType::Term, t_decl.clone()));
                let in_is_prop = self.build_prop_internals(&t_decl, added_vars, pat_in)?;
                added_vars.pop();
                let decl = self.th.forall_is_prop(varname, &in_is_prop)?;
                Ok(decl)
            },
            PropPattern::Exists(varname, pat_in) => {
                let t_decl = self.th.var_is_term(varname, intro_seq)?;
                added_vars.push((varname, VarType::Term, t_decl.clone()));
                let in_is_prop = self.build_prop_internals(&t_decl, added_vars, pat_in)?;
                added_vars.pop();
                let decl = self.th.exists_is_prop(varname, &in_is_prop)?;
                Ok(decl)
            },
            PropPattern::ExistsUnique(varname, pat_in) => {
                let t_decl = self.th.var_is_term(varname, intro_seq)?;
                added_vars.push((varname, VarType::Term, t_decl.clone()));
                let in_is_prop = self.build_prop_internals(&t_decl, added_vars, pat_in)?;
                added_vars.pop();
                let decl = self.th.exists_unique_is_prop(varname, &in_is_prop)?;
                Ok(decl)
            },

            PropPattern::And(pat_l, pat_r) => {
                let l_is_prop = self.build_prop_internals(intro_seq, added_vars, pat_l)?;
                let r_is_prop = self.build_prop_internals(intro_seq, added_vars, pat_r)?;
                let decl = self.th.and_is_prop(&l_is_prop, &r_is_prop)?;
                Ok(decl)
            },
            PropPattern::Or(pat_l, pat_r) => {
                let l_is_prop = self.build_prop_internals(intro_seq, added_vars, pat_l)?;
                let r_is_prop = self.build_prop_internals(intro_seq, added_vars, pat_r)?;
                let decl = self.th.or_is_prop(&l_is_prop, &r_is_prop)?;
                Ok(decl)
            },
            PropPattern::Then(pat_l, pat_r) => {
                let l_is_prop = self.build_prop_internals(intro_seq, added_vars, pat_l)?;
                let r_is_prop = self.build_prop_internals(intro_seq, added_vars, pat_r)?;
                let decl = self.th.then_is_prop(&l_is_prop, &r_is_prop)?;
                Ok(decl)
            },
            PropPattern::Iff(pat_l, pat_r) => {
                let l_is_prop = self.build_prop_internals(intro_seq, added_vars, pat_l)?;
                let r_is_prop = self.build_prop_internals(intro_seq, added_vars, pat_r)?;
                let decl = self.th.iff_is_prop(&l_is_prop, &r_is_prop)?;
                Ok(decl)
            },
            PropPattern::Not(pat_in) => {
                let in_is_prop = self.build_prop_internals(intro_seq, added_vars, pat_in)?;
                let decl = self.th.not_is_prop(&in_is_prop)?;
                Ok(decl)
            },

            PropPattern::Equ(pat_l, pat_r) => {
                let l_is_term = self.build_term_internals(intro_seq, added_vars, pat_l)?;
                let r_is_term = self.build_term_internals(intro_seq, added_vars, pat_r)?;
                let decl = self.th.eq_is_prop(&l_is_term, &r_is_term)?;
                Ok(decl)
            },
            PropPattern::In(pat_l, pat_r) => {
                let l_is_term = self.build_term_internals(intro_seq, added_vars, pat_l)?;
                let r_is_term = self.build_term_internals(intro_seq, added_vars, pat_r)?;
                let decl = self.th.in_is_prop(&l_is_term, &r_is_term)?;
                Ok(decl)
            }
        }
    }

    pub fn build_term(&self, pat: &TermPattern) -> SequentResult {
        self.build_term_internals(self.last_intro(), &mut vec!(), pat)
    }

    pub fn build_prop(&self, pat: &PropPattern) -> SequentResult {
        self.build_prop_internals(self.last_intro(), &mut vec!(), pat)
    }

    pub fn intro_and_build_prop(&self, varname: &str, pat: &PropPattern) -> SequentResult {
        let t_decl = self.th.var_is_term(varname, self.last_intro())?;
        let mut added_vars = vec!();
        added_vars.push((varname, VarType::Term, t_decl.clone()));
        let in_is_prop = self.build_prop_internals(&t_decl, &mut added_vars, pat)?;
        Ok(in_is_prop)
    }

    pub fn register_prop(&mut self, name: &str, seq: Sequent) -> Result {
        if seq.hyp != self.last_intro().hyp {
            Err(Error::create(
                "This sequent does not share its hypotheses with the top sequent on the stack."
            ))
        } else if seq.ccl == None {
            Err(Error::create(
                "This sequent has no conclusion."
            ))
        } else {
            match self.hyps.last_mut() {
                Some(SeqScope {user_props, ..}) => {
                    user_props.insert(name.to_string(), seq)
                },
                None => self.global.insert(name.to_string(), seq)
            };
            Ok(())
        }
    }

    pub fn get_prop(&self, name: &str) -> SequentResult {
        let mut i = self.hyps.len();
        let p = match loop {
            if i == 0 {
                break None
            }
            i = i - 1;
            if let Some(SeqScope {info, intro_seq, user_props, ..}) = self.hyps.get(i) {
                if let Some(s) = user_props.get(name) {
                    break Some(s.clone())
                }
                if let IntroInfo::Hyp(name1) = info {
                    if name == name1 {
                        let s = self.th.push_hyp(intro_seq, 0)?;
                        break Some(s)
                    }
                }
                if let IntroInfo::Instance(InstanceInfo {propname, prop_intro, ..}) = info {
                    if name == propname {
                        let s = self.th.push_hyp(prop_intro, 0)?;
                        break Some(s)
                    }
                }
                if let IntroInfo::Switch(SwitchInfo {
                    l_name, r_name, state, ..
                }) = info {
                    if
                        (*state == SwitchState::L && name == l_name) ||
                        (*state == SwitchState::R && name == r_name)
                    {
                        let s = self.th.push_hyp(intro_seq, 0)?;
                        break Some(s)
                    }
                }
            }
        } {
            Some(s) => Some(s),
            None => self.global.get(name).cloned()
        };

        if let Some(s) = p {
            let leveled = self.th.share_ccl(&s, self.last_intro())?;
            Ok(leveled)
        } else {
            Err(Error::create(&format!("Could not find property {:?}", name)))
        }
    }

    pub fn register_op(
        &mut self, name: &str, opname: &str, places: usize, seq: Sequent
    ) -> Result {
        let (decl_seq, app_seq) = self.th.make_op(places, opname, &seq)?;

        match self.hyps.last_mut() {
            Some(SeqScope {user_ops, ..}) => {
                user_ops.insert(name.to_string(), OpInfo {
                    decl_seq, app_seq, places
                });
            },
            None => {
                self.global_ops.insert(name.to_string(), OpInfo {
                    decl_seq, app_seq, places
                });
            }
        }

        Ok(())
    }

    fn get_op(&self, opname: &str) -> Option<&OpInfo> {
        let mut i = self.hyps.len();
        if let Some(info) = loop {
            if i == 0 {
                break None
            }
            i = i - 1;
            if let Some(SeqScope {user_ops, ..}) = self.hyps.get(i) {
                if let Some(info) = user_ops.get(opname) {
                    break Some(info)
                }
            }
        } {
            Some(info)
        } else {
            self.global_ops.get(opname)
        }
    }

    pub fn get_op_def(&self, opname: &str) -> SequentResult {
        if let Some(info) = self.get_op(opname) {
            Ok(info.app_seq.clone())
        } else {
            Err(Error::create(&format!("Could not find operator \"{}\".", opname)))
        }
    }

    pub fn conclude(&mut self, name: &str, ccl_name: &str) -> Result {
        let ccl = self.get_prop(ccl_name)?;
        self.hyps.pop();
        match self.hyps.last_mut() {
            Some(SeqScope {user_props, ..}) => {
                user_props.insert(name.to_string(), ccl);
                Ok(())
            },
            None => {
                self.global.insert(name.to_string(), ccl);
                Ok(())
            }
        }
    }

    pub fn conclude_op(&mut self, name: &str, opname: &str) -> Result {
        let info = match self.get_op(opname) {
            Some(info) => info.clone(),
            None => Err(Error::create(&format!("Could not find operator \"{}\".", opname)))?
        };
        self.hyps.pop();
        match self.hyps.last_mut() {
            Some(SeqScope {user_ops, ..}) => {
                user_ops.insert(name.to_string(), info);
                Ok(())
            },
            None => {
                self.global_ops.insert(name.to_string(), info);
                Ok(())
            }
        }
    }

    pub fn then_func(&mut self, ccl: &Sequent) -> SequentResult {
        if let Ok(s) = self.th.intro_then(&ccl) {
            self.hyps.pop();
            Ok(s)
        } else {
            Err(Error::create("Can't apply \"intro_then\"."))
        }
    }

    pub fn then(&mut self, name: &str, ccl_name: &str) -> Result {
        let ccl = self.get_prop(ccl_name)?;
        if let Ok(s) = self.th.intro_then(&ccl) {
            self.hyps.pop();
            match self.hyps.last_mut() {
                Some(SeqScope {user_props, ..}) => {
                    user_props.insert(name.to_string(), s);
                    Ok(())
                },
                None => {
                    self.global.insert(name.to_string(), s);
                    Ok(())
                }
            }
        } else {
            Err(Error::create("Can't apply \"intro_then\"."))
        }
    }

    pub fn so_forall_func(&mut self, varname: &str, ccl: &Sequent) -> SequentResult {
        if let Ok(s) = self.th.intro_so_forall(varname, ccl) {
            self.hyps.pop();
            Ok(s)
        } else {
            Err(Error::create("Can't apply \"intro_forall\"."))
        }
    }

    pub fn so_forall(&mut self, name: &str, varname: &str, ccl_name: &str) -> Result {
        let ccl = self.get_prop(ccl_name)?;
        if let Ok(s) = self.th.intro_so_forall(varname, &ccl) {
            self.hyps.pop();
            match self.hyps.last_mut() {
                Some(SeqScope {user_props, ..}) => {
                    user_props.insert(name.to_string(), s);
                    Ok(())
                },
                None => {
                    self.global.insert(name.to_string(), s);
                    Ok(())
                }
            }
        } else {
            Err(Error::create("Can't apply \"intro_so_forall\"."))
        }
    }

    pub fn forall(&mut self, name: &str, varname: &str, ccl_name: &str) -> Result {
        let ccl = self.get_prop(ccl_name)?;
        if let Ok(s) = self.th.intro_forall(varname, &ccl) {
            self.hyps.pop();
            match self.hyps.last_mut() {
                Some(SeqScope {user_props, ..}) => {
                    user_props.insert(name.to_string(), s);
                    Ok(())
                },
                None => {
                    self.global.insert(name.to_string(), s);
                    Ok(())
                }
            }
        } else {
            Err(Error::create("Can't apply \"intro_forall\"."))
        }
    }

    pub fn elim_exists(
        &mut self, name: &str, ccl_name: &str, ccl_pat: &PropPattern
    ) -> Result {
        let ccl = self.get_prop(ccl_name)?;
        let prev_top = self.hyps.pop();
        match &prev_top {
            Some(SeqScope {info: IntroInfo::Instance(InstanceInfo {exists, ..}), ..}) => {
                match self.build_prop(ccl_pat) {
                    Ok(ccl_is_prop) => {
                        match self.th.elim_exists(
                            &ccl_is_prop,
                            exists,
                            &ccl
                        ) {
                            Ok(s) => {
                                match self.hyps.last_mut() {
                                    Some(SeqScope {user_props, ..}) => {
                                        user_props.insert(name.to_string(), s);
                                        Ok(())
                                    },
                                    None => {
                                        self.global.insert(name.to_string(), s);
                                        Ok(())
                                    }
                                }
                            },
                            Err(e) => {
                                if let Some(top) = prev_top {
                                    self.hyps.push(top)
                                }
                                Err(e)
                            }
                        }
                    },
                    Err(e) => {
                        if let Some(top) = prev_top {
                            self.hyps.push(top)
                        }
                        Err(e)
                    }
                }
            },
            _ => Err(Error::create("There is no instanciated term to eliminate."))
        }
    }

    pub fn ccl_and_elim(&mut self, name: &str, r_ccl: Sequent) -> Result {
        if let Some(SeqScope {info: IntroInfo::Switch(info), ..}) = self.hyps.last() {
            if let SwitchInfo {state: SwitchState::R, ..} = info {

            } else {
                return Err(Error::create("Already switched."))
            }
        } else {
            return Err(Error::create("Not switching."))
        }

        match self.hyps.last() {
            Some(SeqScope {info: IntroInfo::Switch(SwitchInfo {
                or_seq,
                l_ccl: Some(l_ccl),
                ..
            }), ..}) => {
                if let Ok(s) = self.th.elim_or(&or_seq, &l_ccl, &r_ccl) {
                    self.hyps.pop();
                    match self.hyps.last_mut() {
                        Some(SeqScope {user_props, ..}) => {
                            user_props.insert(name.to_string(), s);
                            Ok(())
                        },
                        None => {
                            self.global.insert(name.to_string(), s);
                            Ok(())
                        }
                    }
                } else {
                    Err(Error::create("Can't apply \"elim_or\"."))
                }
            },
            _ => panic!()
        }
    }

    pub fn print_op(&self, out: &mut dyn Write, opname: &str) -> Result {
        if let Some(OpInfo {decl_seq, places, ..}) = self.get_op(opname) {
            writeln!(out, "operator {} :", opname)?;
            match &decl_seq.ccl {
                Some(t) => {
                    write!(out, "  ")?; print_term(out, t)?; writeln!(out)?;
                },
                None => writeln!(out, "  malformed !")?
            }
            if *places == 1 {
                writeln!(out, "  (1 place)")?;
            } else {
                writeln!(out, "  ({} places)", places)?;
            }
        } else {
            writeln!(out, "Can't find operator \"{}\".", opname)?
        }

        Ok(())
    }

    pub fn print_all(&self, out: &mut dyn Write) -> Result {
        let pad = 20;

        if self.global.is_empty() {
            writeln!(out, "(no global properties)")?;
        } else {
            writeln!(out, "global properties :")?;
            self.global.iter().try_for_each(|(k, v)| -> Result {
                write!(out, "{:^pad$} | ", k, pad = pad)?;
                match &v.ccl {
                    Some(ccl) => {
                        print_term(out, ccl)?;
                        writeln!(out, )?;
                    },
                    None => writeln!(out, "none")?
                }
                Ok(())
            })?
        }

        writeln!(out)?;

        if self.global_ops.is_empty() {
            writeln!(out, "(no global operators)")?;
        } else {
            writeln!(out, "global operators :")?;
            self.global_ops.iter().try_for_each(|(k, v)| -> Result {
                write!(out, "{:^pad$} | ", k, pad = pad)?;
                match &v.decl_seq.ccl {
                    Some(t) => {
                        print_term(out, t)?; writeln!(out)?;
                    },
                    None => writeln!(out, "malformed !")?
                }
                if v.places == 1 {
                    writeln!(out, "{:^pad$} | (1 place)", "", pad = pad)?;
                } else {
                    writeln!(out, "{:^pad$} | ({} places)", "", v.places, pad = pad)?;
                }
                Ok(())
            })?
        }

        self.hyps.iter().try_for_each(
            |SeqScope {info, intro_seq, user_props, user_ops}| -> Result {
            writeln!(out, )?;
            match info {
                IntroInfo::Scope => writeln!(out, "new scope")?,
                IntroInfo::Hyp(name) => write!(out, "hyp {} : ", name)?,
                IntroInfo::Prop(name) => write!(out, "prop {} : ", name)?,
                IntroInfo::Term(name) => write!(out, "term {} : ", name)?,
                IntroInfo::Instance(InstanceInfo {propname, varname, ..}) => {
                    write!(out, "term {} instantiated, verifies {} : ", propname, varname)?
                },
                IntroInfo::Switch(SwitchInfo {or_seq, l_name, r_name, state, ..}) => {
                    write!(out, "switching on ")?;
                    match &or_seq.ccl {
                        Some(ccl) => print_term(out, ccl)?,
                        None => write!(out, "(malformed)")?
                    }
                    write!(out, ", ")?;
                    match state {
                        SwitchState::L => {
                            writeln!(out, "left arm, hypothesis named \"{}\"", l_name)?
                        },
                        SwitchState::R => {
                            writeln!(out, "right arm, hypothesis named \"{}\"", r_name)?
                        }
                    }
                }
            }
            match info {
                IntroInfo::Scope => (),
                IntroInfo::Switch(_) => (),
                _ => {
                    match intro_seq.hyp.dest() {
                        Some((Jugement::Truth(s), _)) => {
                            print_term(out, s)?;
                            writeln!(out)?;
                        },
                        _ => writeln!(out, "malformed !")?
                    }
                }
            }

            if user_props.len() > 0 {
                writeln!(out, "properties :")?
            }
            user_props.iter().try_for_each(|(k, v)| -> Result {
                write!(out, "{:^pad$} | ", k, pad = pad)?;
                match &v.ccl {
                    Some(ccl) => {
                        print_term(out, ccl)?;
                        writeln!(out)?;
                    },
                    None => writeln!(out, "none")?
                }
                Ok(())
            })?;
            if user_ops.len() > 0 {
                writeln!(out, "operators :")?
            }
            user_ops.iter().try_for_each(|(k, v)| -> Result {
                write!(out, "{:^pad$} | ", k, pad = pad)?;
                match &v.decl_seq.ccl {
                    Some(t) => {
                        print_term(out, t)?; writeln!(out)?;
                    },
                    None => writeln!(out, "malformed !")?
                }
                if v.places == 1 {
                    writeln!(out, "{:^pad$} | (1 place)", "", pad = pad)?;
                } else {
                    writeln!(out, "{:^pad$} | ({} places)", "", v.places, pad = pad)?;
                }
                Ok(())
            })?;

            Ok(())
        })?;

        Ok(())
    }
}
