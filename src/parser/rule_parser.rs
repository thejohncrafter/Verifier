
use std::collections::HashMap;
use std::rc::Rc;

use core::base_types::{TquTemplate, FunTemplate};
use core::matcher::*;

use parser::error::{LocalizedError, ParserResult};
use parser::tokenizer::*;

enum TemplatePattern {
    Var(String),
    Fun(String),
}

pub struct RuleParser<'a> {
    quas: &'a HashMap<String, Rc<dyn TquTemplate>>,
    funs: &'a HashMap<String, Rc<dyn FunTemplate>>,
}

impl<'a> RuleParser<'a> {
    pub fn new(
        quas: &'a HashMap<String, Rc<dyn TquTemplate>>,
        funs: &'a HashMap<String, Rc<dyn FunTemplate>>
    ) -> RuleParser<'a> {
        RuleParser {quas, funs}
    }

    fn make_var_pattern(
        &self, start: Pos, end: Pos, args: &[Exp]
    ) -> ParserResult<TemplatePattern> {
        match args {
            [
                Exp {data: ExpData::Id(name), ..}
            ] => {
                Ok(TemplatePattern::Var(name.clone()))
            },
            _ => Err(LocalizedError::new(start, end, "Syntax is \"var <name>\"."))
        }
    }

    fn make_fun_pattern(
        &self, start: Pos, end: Pos, args: &[Exp]
    ) -> ParserResult<TemplatePattern> {
        match args {
            [
                Exp {data: ExpData::Id(name), ..}
            ] => {
                Ok(TemplatePattern::Fun(name.clone()))
            },
            _ => Err(LocalizedError::new(start, end, "Syntax is \"fun <name>\"."))
        }
    }

    fn parse_template(&self, exp: &Exp) -> ParserResult<TemplatePattern> {
        match &exp.data {
            ExpData::Id(_) => Err(LocalizedError::new(exp.start, exp.end, "Expected a function call.")),
            ExpData::Tuple(args) => {
                match args.first() {
                    Some(Exp {start, end, data: ExpData::Id(name), ..}) => {
                        let (start, callend) = (*start, args.last().unwrap().end);
                        let fun_args = &args[1..];
                        match name.as_str() {
                            "var" => {
                                self.make_var_pattern(start, callend, fun_args)
                            },
                            "fun" => {
                                self.make_fun_pattern(start, callend, fun_args)
                            },
                            _ => Err(LocalizedError::new(start, *end, "Unknown function in this context."))
                        }
                    },
                    Some(e) => Err(LocalizedError::new(e.start, e.end, "Expected a function name.")),
                    _ => Err(LocalizedError::new(exp.start, exp.end, "Expected a function name."))
                }
            }
        }
    }

    fn make_meta(&self, start: Pos, end: Pos, args: &[Exp]) -> ParserResult<TermPattern> {
        match args {
            [
                Exp {data: ExpData::Id(name), ..}
            ] => {
                Ok(TermPattern::Meta(name.clone()))
            },
            _ => Err(LocalizedError::new(start, end, "Syntax is \"meta <name>\"."))
        }
    }

    fn make_var(&self, start: Pos, end: Pos, args: &[Exp]) -> ParserResult<TermPattern> {
        match args {
            [
                Exp {data: ExpData::Id(name), ..}
            ] => {
                Ok(TermPattern::Var(name.clone()))
            },
            _ => Err(LocalizedError::new(start, end, "Syntax is \"var <name>\"."))
        }
    }

    fn make_qua(&self, start: Pos, end: Pos, args: &[Exp]) -> ParserResult<TermPattern> {
        match args {
            [
                Exp {data: ExpData::Id(te_name), start: te_start, end: te_end},
                Exp {data: ExpData::Id(v_name), ..},
                t_exp
            ] => {
                match self.quas.get(te_name) {
                    Some(te) => {
                        Ok(TermPattern::Qua(
                            Rc::clone(te), v_name.clone(), Box::new(self.parse_term(t_exp)?)
                        ))
                    },
                    None => {
                        Err(LocalizedError::new(*te_start, *te_end, "No such quantifier template."))
                    }
                }
            },
            _ => Err(LocalizedError::new(
                start, end,
                "Syntax is \"qua <template name> <var name> <term>\"."
            ))
        }
    }

    fn make_fun(&self, start: Pos, end: Pos, args: &[Exp]) -> ParserResult<TermPattern> {
        match args {
            [
                Exp {data: ExpData::Id(te_name), start: te_start, end: te_end},
                Exp {data: ExpData::Tuple(arg_exps), ..}
            ] => {
                match self.funs.get(te_name) {
                    Some(te) => {
                        let mut args = Vec::with_capacity(arg_exps.len());
                        arg_exps.iter().try_for_each(|exp| -> ParserResult<()> {
                            args.push(self.parse_term(exp)?);
                            Ok(())
                        })?;
                        Ok(TermPattern::Fun(
                            Rc::clone(te), args
                        ))
                    },
                    None => {
                        Err(LocalizedError::new(*te_start, *te_end, "No such function template."))
                    }
                }
            },
            _ => Err(LocalizedError::new(
                start, end,
                "Syntax is \"fun <template name> [<argument #1>, ..]\".")
            )
        }
    }

    fn make_replace(&self, start: Pos, end: Pos, args: &[Exp]) -> ParserResult<TermPattern> {
        match args {
            [
                t_exp,
                Exp {data: ExpData::Id(v_name), ..},
                with_exp
            ] => {
                Ok(TermPattern::Replace(
                    Box::new(self.parse_term(t_exp)?),
                    v_name.clone(),
                    Box::new(self.parse_term(with_exp)?)
                ))
            },
            _ => Err(LocalizedError::new(
                start, end,
                "Syntax is \"replace <term> <variable name> <term>\".")
            )
        }
    }

    pub fn parse_term(&self, exp: &Exp) -> ParserResult<TermPattern> {
        match &exp.data {
            ExpData::Id(_) => Err(LocalizedError::new(exp.start, exp.end, "Expected a function call.")),
            ExpData::Tuple(args) => {
                match args.first() {
                    Some(Exp {start, end, data: ExpData::Id(name), ..}) => {
                        let (start, callend) = (*start, args.last().unwrap().end);
                        let fun_args = &args[1..];
                        match name.as_str() {
                            "meta" => {
                                self.make_meta(start, callend, fun_args)
                            },
                            "var" => {
                                self.make_var(start, callend, fun_args)
                            },
                            "qua" => {
                                self.make_qua(start, callend, fun_args)
                            },
                            "fun" => {
                                self.make_fun(start, callend, fun_args)
                            },
                            "replace" => {
                                self.make_replace(start, callend, fun_args)
                            },
                            _ => Err(LocalizedError::new(start, *end, "Unknown function in this context."))
                        }
                    },
                    Some(e) => Err(LocalizedError::new(e.start, e.end, "Expected a function name.")),
                    _ => Err(LocalizedError::new(exp.start, exp.end, "Expected a function name."))
                }
            }
        }
    }

    fn make_intro(&self, start: Pos, end: Pos, args: &[Exp]) -> ParserResult<JugementPattern> {
        match args {
            [
                Exp {data: ExpData::Id(v_name), ..}
            ] => {
                Ok(JugementPattern::Intro(v_name.clone()))
            },
            _ => Err(LocalizedError::new(
                start, end,
                "Syntax is \"intro <variable name>\".")
            )
        }
    }

    fn make_truth(&self, start: Pos, end: Pos, args: &[Exp]) -> ParserResult<JugementPattern> {
        match args {
            [
                t_exp
            ] => {
                Ok(JugementPattern::Truth(self.parse_term(t_exp)?))
            },
            _ => Err(LocalizedError::new(
                start, end,
                "Syntax is \"truth <term>\".")
            )
        }
    }

    pub fn parse_jugement(&self, exp: &Exp) -> ParserResult<JugementPattern> {
        match &exp.data {
            ExpData::Id(_) => Err(LocalizedError::new(exp.start, exp.end, "Expected a function call.")),
            ExpData::Tuple(args) => {
                match args.first() {
                    Some(Exp {start, end, data: ExpData::Id(name), ..}) => {
                        let (start, callend) = (*start, args.last().unwrap().end);
                        let fun_args = &args[1..];
                        match name.as_str() {
                            "intro" => {
                                self.make_intro(start, callend, fun_args)
                            },
                            "truth" => {
                                self.make_truth(start, callend, fun_args)
                            },
                            _ => Err(LocalizedError::new(start, *end, "Unknown function in this context."))
                        }
                    },
                    Some(e) => Err(LocalizedError::new(e.start, e.end, "Expected a function name.")),
                    _ => Err(LocalizedError::new(exp.start, exp.end, "Expected a function name."))
                }
            }
        }
    }

    fn make_meta_hyp(&self, start: Pos, end: Pos, args: &[Exp]) -> ParserResult<HypPattern> {
        match args {
            [
                Exp {data: ExpData::Id(name), ..}
            ] => {
                Ok(HypPattern::Meta(name.clone()))
            },
            _ => Err(LocalizedError::new(start, end, "Syntax is \"meta <name>\"."))
        }
    }

    fn make_void_hyp(&self, _start: Pos, _end: Pos, _args: &[Exp]) -> ParserResult<HypPattern> {
        Ok(HypPattern::Void)
    }

    fn make_cons(&self, start: Pos, end: Pos, args: &[Exp]) -> ParserResult<HypPattern> {
        match args {
            [
                j_exp,
                hyp_exp
            ] => {
                Ok(HypPattern::Cons(
                    self.parse_jugement(j_exp)?,
                    Box::new(self.parse_hyp(hyp_exp)?)
                ))
            },
            _ => Err(LocalizedError::new(start, end, "Syntax is \"meta <name>\"."))
        }
    }

    pub fn parse_hyp(&self, exp: &Exp) -> ParserResult<HypPattern> {
        match &exp.data {
            ExpData::Id(_) => Err(LocalizedError::new(exp.start, exp.end, "Expected a function call.")),
            ExpData::Tuple(args) => {
                match args.first() {
                    Some(Exp {start, end, data: ExpData::Id(name), ..}) => {
                        let (start, callend) = (*start, args.last().unwrap().end);
                        let fun_args = &args[1..];
                        match name.as_str() {
                            "meta" => {
                                self.make_meta_hyp(start, callend, fun_args)
                            },
                            "void" => {
                                self.make_void_hyp(start, callend, fun_args)
                            },
                            "cons" => {
                                self.make_cons(start, callend, fun_args)
                            },
                            _ => Err(LocalizedError::new(start, *end, "Unknown function in this context."))
                        }
                    },
                    Some(e) => Err(LocalizedError::new(e.start, e.end, "Expected a function name.")),
                    _ => Err(LocalizedError::new(exp.start, exp.end, "Expected a function name."))
                }
            }
        }
    }

    fn make_sequent(
        &self, start: Pos, end: Pos, args: &[Exp]
    ) -> ParserResult<SequentPattern> {
        match args {
            [
                hyp_exp,
                ccl_exp
            ] => {
                let ccl = match &ccl_exp.data {
                    ExpData::Id(cmd) => {
                        if cmd == "none" {
                            None
                        } else {
                            Err(LocalizedError::new(
                                ccl_exp.start, ccl_exp.end,
                                "Expected \"none\"."
                            ))?
                        }
                    },
                    _ => Some(self.parse_term(ccl_exp)?)
                };
                Ok(SequentPattern::new(self.parse_hyp(hyp_exp)?, ccl))
            },
            _ => Err(LocalizedError::new(start, end, "Syntax is \"sequent <hyp> <term>\"."))
        }
    }

    pub fn parse_sequent(&self, exp: &Exp) -> ParserResult<SequentPattern> {
        match &exp.data {
            ExpData::Id(_) => Err(LocalizedError::new(exp.start, exp.end, "Expected a function call.")),
            ExpData::Tuple(args) => {
                match args.first() {
                    Some(Exp {start, end, data: ExpData::Id(name), ..}) => {
                        let (start, callend) = (*start, args.last().unwrap().end);
                        let fun_args = &args[1..];
                        match name.as_str() {
                            "sequent" => {
                                self.make_sequent(start, callend, fun_args)
                            },
                            _ => Err(LocalizedError::new(start, *end, "Unknown function in this context."))
                        }
                    },
                    Some(e) => Err(LocalizedError::new(e.start, e.end, "Expected a function name.")),
                    _ => Err(LocalizedError::new(exp.start, exp.end, "Expected a function name."))
                }
            }
        }
    }

    fn make_rule(
        &self, start: Pos, end: Pos, args: &[Exp]
    ) -> ParserResult<(String, Rule)> {
        match args {
            [
                Exp {data: ExpData::Id(name), ..},
                Exp {data: ExpData::Tuple(temps_exp), ..},
                Exp {data: ExpData::Tuple(hyps_exp), ..},
                ccl_exp
            ] => {
                let mut temps = Vec::with_capacity(temps_exp.len());
                let mut var_count = 0;
                temps_exp.iter().try_for_each(|exp| -> ParserResult<()> {
                    let temp = self.parse_template(exp)?;
                    match temp {
                        TemplatePattern::Var(_) => var_count = var_count + 1,
                        TemplatePattern::Fun(_) => ()
                    }
                    temps.push(temp);
                    Ok(())
                })?;
                let mut vars = Vec::with_capacity(var_count);
                let mut funs = Vec::with_capacity(temps_exp.len() - var_count);
                temps.into_iter().for_each(|temp| {
                    match temp {
                        TemplatePattern::Var(name) => vars.push(name),
                        TemplatePattern::Fun(name) => funs.push(name),
                    }
                });
                let mut hyps = Vec::with_capacity(hyps_exp.len());
                hyps_exp.iter().try_for_each(|exp| -> ParserResult<()> {
                    hyps.push(self.parse_sequent(exp)?);
                    Ok(())
                })?;
                Ok((name.clone(), Rule::new(vars, funs, hyps, self.parse_sequent(ccl_exp)?)))
            },
            _ => Err(LocalizedError::new(
                start, end,
                "Syntax is \"rule <name> ([<template #1>, ..]) ([<sequent #1>, ..]) <sequent>\"."
            ))
        }
    }

    pub fn parse_rule(&self, exp: &Exp) -> ParserResult<(String, Rule)> {
        match &exp.data {
            ExpData::Id(_) => Err(LocalizedError::new(exp.start, exp.end, "Expected a function call.")),
            ExpData::Tuple(args) => {
                match args.first() {
                    Some(Exp {start, end, data: ExpData::Id(name), ..}) => {
                        let (start, callend) = (*start, args.last().unwrap().end);
                        let fun_args = &args[1..];
                        match name.as_str() {
                            "rule" => {
                                self.make_rule(start, callend, fun_args)
                            },
                            _ => Err(LocalizedError::new(start, *end, "Unknown function in this context."))
                        }
                    },
                    Some(e) => Err(LocalizedError::new(e.start, e.end, "Expected a function name.")),
                    _ => Err(LocalizedError::new(exp.start, exp.end, "Expected a function name."))
                }
            }
        }
    }

    pub fn parse_rules(&self, exp: &Exp) -> ParserResult<HashMap<String, Rule>> {
        match &exp.data {
            ExpData::Id(_) => Err(LocalizedError::new(exp.start, exp.end, "Expected a function call.")),
            ExpData::Tuple(args) => {
                let mut rules = HashMap::new();
                args.iter().try_for_each(|arg| {
                    let (name, rule) = self.parse_rule(arg)?;
                    match rules.insert(name, rule) {
                        Some(_) => {
                            Err(LocalizedError::new(
                                arg.start, arg.end,
                                "A rule with this name already exists."
                            ))
                        },
                        None => Ok(())
                    }
                })?;
                Ok(rules)
            }
        }
    }
    // replace (meta P) x (meta t)
}
