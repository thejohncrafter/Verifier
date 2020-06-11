
use runner::proof_types::*;

use parser::error::{LocalizedError, ParserResult};
use parser::tokenizer::*;

macro_rules! fun_parser {
    (
        $fun_name:ident,
        $restype:ty,
        rules [$((
            $rule_name:literal, $variant:path,
            ($($varname:ident),*),
            ($($propname:ident),*),
            ($($termname:ident),*),
            ($($param:ident),*)
        )),*],
        raw
        [$((
            $name:literal,
            $pattern:pat,
            (
                $this:ident, $val:expr
            ),
            $synt:literal
        )), *]
    ) => {
        pub fn $fun_name(&self, exp: &Exp) -> ParserResult<$restype> {
            match &exp.data {
                ExpData::Id(_) => Err(LocalizedError::new(
                    exp.start, exp.end,
                    "Expected a function call."
                )),
                ExpData::Tuple(args) => {
                    match args.first() {
                        Some(Exp {start, end, data: ExpData::Id(name), ..}) => {
                            let (start, callend) = (*start, args.last().unwrap().end);
                            let fun_args = &args[1..];
                            match name.as_str() {
                                $($rule_name => {
                                    match fun_args {
                                        [
                                            $(Exp {data: ExpData::Id($varname), ..},)*
                                            $($propname,)*
                                            $($termname,)*
                                            $($param,)*
                                        ] => Ok($variant(
                                            $($varname.clone(),)*
                                            $(self.parse_prop($propname)?,)*
                                            $(self.parse_term($termname)?,)*
                                            $(Box::new(self.parse_rule($param)?),)*
                                        )),
                                        _ => Err(LocalizedError::new(
                                            start, callend,
                                            "The arguments do not fit the rule."
                                        ))
                                    }
                                },)*
                                $($name => {
                                    let $this = self;
                                    match fun_args {
                                        $pattern => $val,
                                        _ => Err(LocalizedError::new(start, callend, $synt))
                                    }
                                },)*
                                _ => Err(LocalizedError::new(
                                    start, *end,
                                    "Unknown function in this context."
                                ))
                            }
                        },
                        Some(e) => Err(LocalizedError::new(e.start, e.end, "Expected a function name.")),
                        _ => Err(LocalizedError::new(
                            exp.start, exp.end,
                            "Expected a function name."
                        ))
                    }
                }
            }
        }
    }
}

pub struct ProofParser;

impl ProofParser {
    pub fn new() -> ProofParser {
        ProofParser {}
    }

    pub fn parse_proof(&self, exp: &Exp) -> ParserResult<Vec<CmdPattern>> {
        match exp {
            Exp {data: ExpData::Tuple(args), ..} => {
                let mut cmds = Vec::with_capacity(args.len());
                args.iter().try_for_each(|arg| {
                    cmds.push(self.parse_cmd(arg)?);
                    Ok(())
                })?;
                Ok(cmds)
            },
            _ => Err(LocalizedError::new(exp.start, exp.end, "Expected a tuple."))
        }
    }

    pub fn parse_axioms(&self, exp: &Exp) -> ParserResult<Vec<(String, PropPattern)>> {
        match exp {
            Exp {data: ExpData::Tuple(axiom_exps), ..} => {
                let mut axioms = Vec::with_capacity(axiom_exps.len());
                axiom_exps.iter().try_for_each(|a_exp| -> ParserResult<()> {
                    let start = a_exp.start;
                    let end = a_exp.end;
                    match a_exp {
                        Exp {data: ExpData::Tuple(args), ..} => {
                            axioms.push(self.parse_axiom(start, end, &args)?);
                            Ok(())
                        },
                        _ => Err(LocalizedError::new(start, end, "Expected a tuple."))
                    }
                })?;
                Ok(axioms)
            },
            _ => Err(LocalizedError::new(exp.start, exp.end, "Expected a tuple."))
        }
    }

    fn parse_axiom(
        &self, start: Pos, end: Pos, args: &[Exp]
    ) -> ParserResult<(String, PropPattern)> {
        match args {
            [
                Exp {data: ExpData::Id(name), ..},
                p
            ] => {
                Ok((name.clone(), self.parse_prop(p)?))
            },
            _ => Err(LocalizedError::new(start, end, "Syntax is \"<name> <prop>\"."))
        }
    }

    fun_parser!(
        parse_term, TermPattern,
        rules [],
        raw [(
            "var",
            [
                Exp {data: ExpData::Id(name), ..}
            ],
            (_this, Ok(TermPattern::Var(name.clone()))),
            "Syntax is \"var <name>\"."
        ),
        (
            "call",
            [
                Exp {data: ExpData::Id(opname), ..},
                Exp {data: ExpData::Tuple(rawargs), ..}
            ],
            (this, {
                let mut args = Vec::with_capacity(rawargs.len() - 1);
                rawargs.iter().try_for_each(|pat| -> ParserResult<()> {
                    args.push(Box::new(this.parse_term(pat)?));
                    Ok(())
                })?;
                Ok(TermPattern::OpCall(opname.clone(), args))
            }),
            "Syntax is \"call <opname> [<argument #1>, ...]\"."
        )]
    );

    fun_parser!(
        parse_prop, PropPattern,
        rules [],
        raw [(
            "var",
            [
                Exp {data: ExpData::Id(name), ..}
            ],
            (_this, Ok(PropPattern::Var(name.clone()))),
            "Syntax is \"var <name>\"."
        ),

        (
            "top",
            [],
            (_this, Ok(PropPattern::Top())),
            "Syntax is \"top\"."
        ),
        (
            "bot",
            [],
            (_this, Ok(PropPattern::Bot())),
            "Syntax is \"bot\"."
        ),

        (
            "so_forall",
            [
                Exp {data: ExpData::Id(varname), ..},
                pat_in
            ],
            (this, Ok(
                PropPattern::SoForall(varname.clone(), Box::new(this.parse_prop(pat_in)?))
            )),
            "Syntax is \"so_forall <varname> <prop>\"."
        ),
        (
            "forall",
            [
                Exp {data: ExpData::Id(varname), ..},
                pat_in
            ],
            (this, Ok(
                PropPattern::Forall(varname.clone(), Box::new(this.parse_prop(pat_in)?))
            )),
            "Syntax is \"forall <varname> <prop>\"."
        ),
        (
            "exists",
            [
                Exp {data: ExpData::Id(varname), ..},
                pat_in
            ],
            (this, Ok(
                PropPattern::Exists(varname.clone(), Box::new(this.parse_prop(pat_in)?))
            )),
            "Syntax is \"exists <varname> <prop>\"."
        ),
        (
            "exists_unique",
            [
                Exp {data: ExpData::Id(varname), ..},
                pat_in
            ],
            (this, Ok(
                PropPattern::ExistsUnique(varname.clone(), Box::new(this.parse_prop(pat_in)?))
            )),
            "Syntax is \"exists_unique <varname> <prop>\"."
        ),

        (
            "and",
            [
                pat_l,
                pat_r,
            ],
            (this, Ok(PropPattern::And(
                Box::new(this.parse_prop(pat_l)?),
                Box::new(this.parse_prop(pat_r)?)
            ))),
            "Syntax is \"and <prop> <prop>\"."
        ),
        (
            "or",
            [
                pat_l,
                pat_r,
            ],
            (this, Ok(PropPattern::Or(
                Box::new(this.parse_prop(pat_l)?),
                Box::new(this.parse_prop(pat_r)?)
            ))),
            "Syntax is \"or <prop> <prop>\"."
        ),
        (
            "then",
            [
                pat_l,
                pat_r,
            ],
            (this, Ok(PropPattern::Then(
                Box::new(this.parse_prop(pat_l)?),
                Box::new(this.parse_prop(pat_r)?)
            ))),
            "Syntax is \"then <prop> <prop>\"."
        ),
        (
            "iff",
            [
                pat_l,
                pat_r,
            ],
            (this, Ok(PropPattern::Iff(
                Box::new(this.parse_prop(pat_l)?),
                Box::new(this.parse_prop(pat_r)?)
            ))),
            "Syntax is \"iff <prop> <prop>\"."
        ),
        (
            "not",
            [
                pat_in,
            ],
            (this, Ok(PropPattern::Not(
                Box::new(this.parse_prop(pat_in)?),
            ))),
            "Syntax is \"not <prop>\"."
        ),

        (
            "eq",
            [
                pat_l,
                pat_r,
            ],
            (this, Ok(PropPattern::Equ(
                Box::new(this.parse_term(pat_l)?),
                Box::new(this.parse_term(pat_r)?)
            ))),
            "Syntax is \"eq <prop> <prop>\"."
        ),
        (
            "in",
            [
                pat_l,
                pat_r,
            ],
            (this, Ok(PropPattern::In(
                Box::new(this.parse_term(pat_l)?),
                Box::new(this.parse_term(pat_r)?)
            ))),
            "Syntax is \"in <prop> <prop>\"."
        )]
    );

    pub fn parse_rule(&self, exp: &Exp) -> ParserResult<RulePattern> {
        Ok(RulePattern {
            data: self.parse_rule_internals(exp)?,
            start: exp.start,
            end: exp.end
        })
    }

    fun_parser!(
        parse_rule_internals, RuleData,
        rules [
            (
                "op_def",
                RuleData::OpDef,
                (a), (), (), ()
            ),
            (
                "intro_and",
                RuleData::IntroAnd,
                (), (), (), (a, b)
            ),
            (
                "elim_and_l",
                RuleData::ElimAndL,
                (), (), (), (a)
            ),
            (
                "elim_and_r",
                RuleData::ElimAndR,
                (), (), (), (a)
            ),
            (
                "intro_iff",
                RuleData::IntroIff,
                (), (), (), (a, b)
            ),
            (
                "elim_iff_l",
                RuleData::ElimIffL,
                (), (), (), (a)
            ),
            (
                "elim_iff_r",
                RuleData::ElimIffR,
                (), (), (), (a)
            ),
            (
                "intro_top",
                RuleData::IntroTop,
                (), (), (), ()
            ),
            (
                "intro_bot",
                RuleData::IntroBot,
                (), (), (), (a, b)
            ),
            (
                "elim_bot",
                RuleData::ElimBot,
                (), (a), (), (b)
            ),

            (
                "intro_exists",
                RuleData::IntroExists,
                (a), (b), (c), (d)
            ),
            (
                "intro_exists_unique",
                RuleData::IntroExistsUnique,
                (a), (b), (), (c)
            ),
            (
                "elim_exists_unique",
                RuleData::ElimExistsUnique,
                (a, b), (), (), (c)
            ),


            (
                "elim_then",
                RuleData::ElimThen,
                (), (), (), (l, r)
            ),
            (
                "elim_so_forall",
                RuleData::ElimSoForall,
                (), (a), (), (b)
            ),
            (
                "elim_forall",
                RuleData::ElimForall,
                (), (), (a), (b)
            )
        ],
        raw [(
            "get",
            [
                Exp {data: ExpData::Id(name), ..}
            ],
            (_this, Ok(RuleData::Get(name.clone()))),
            "Syntax is \"get <varname>\"."
        ),
        (
            "intro_or_l",
            [
                l_pat, r_pat
            ],
            (this, Ok(RuleData::IntroOrL(
                Box::new(this.parse_rule(l_pat)?),
                this.parse_prop(r_pat)?
            ))),
            "Syntax is \"intro_or_l <rule> <prop>\"."
        ),
        (
            "intro_or_r",
            [
                l_pat, r_pat
            ],
            (this, Ok(RuleData::IntroOrR(
                this.parse_prop(l_pat)?,
                Box::new(this.parse_rule(r_pat)?)
            ))),
            "Syntax is \"intro_or_l <rule> <prop>\"."
        )]
    );

    pub fn parse_cmd(&self, exp: &Exp) -> ParserResult<CmdPattern> {
        Ok(CmdPattern {
            data: self.parse_cmd_internals(exp)?,
            start: exp.start,
            end: exp.end
        })
    }

    fun_parser!(
        parse_cmd_internals, CmdData,
        rules [],
        raw [
        (
            "clear",
            [],
            (_this, Ok(CmdData::Clear)),
            "Syntax is \"clear\"."
        ),
        (
            "scope",
            [],
            (_this, Ok(CmdData::Scope)),
            "Syntax is \"scope\"."
        ),
        (
            "prop",
            [
                Exp {data: ExpData::Id(name), ..}
            ],
            (_this, Ok(CmdData::Prop(name.clone()))),
            "Syntax is \"prop <name>\"."
        ),
        (
            "hyp",
            [
                Exp {data: ExpData::Id(name), ..},
                pat
            ],
            (this, Ok(CmdData::Hyp(name.clone(), this.parse_prop(pat)?))),
            "Syntax is \"hyp <name> <prop>\"."
        ),
        (
            "term",
            [
                Exp {data: ExpData::Id(name), ..}
            ],
            (_this, Ok(CmdData::Term(name.clone()))),
            "Syntax is \"term <name>\"."
        ),
        (
            "instantiate",
            [
                Exp {data: ExpData::Id(propname), ..},
                Exp {data: ExpData::Id(varname), ..},
                pat,
                exists,
            ],
            (this, Ok(CmdData::Instantiate(
                propname.clone(),
                varname.clone(),
                this.parse_prop(pat)?,
                this.parse_rule(exists)?
            ))),
            "Syntax is \"instantiate <prop name> <term name> <prop> <exists>\"."
        ),
        (
            "reg",
            [
                Exp {data: ExpData::Id(name), ..},
                pat
            ],
            (this, Ok(CmdData::Reg(name.clone(), this.parse_rule(pat)?))),
            "Syntax is \"reg <name> <rule>\"."
        ),
        (
            "switch",
            [
                or_prop,
                Exp {data: ExpData::Id(l_name), ..},
                l_prop,
                Exp {data: ExpData::Id(r_name), ..},
                r_prop
            ],
            (this, Ok(CmdData::Switch(
                this.parse_rule(or_prop)?,
                l_name.clone(),
                this.parse_prop(l_prop)?,
                r_name.clone(),
                this.parse_prop(r_prop)?
            ))),
            "Syntax is \"switch <rule> <l_name> <l_prop> <r_name> <r_prop>\"."
        ),
        (
            "ccl_and_switch",
            [
                ccl
            ],
            (this, Ok(CmdData::CclAndSwitch(this.parse_rule(ccl)?))),
            "Syntax is \"ccl_and_switch <rule>\"."
        ),
        (
            "ccl_and_elim",
            [
                Exp {data: ExpData::Id(name), ..},
                ccl
            ],
            (this, Ok(CmdData::CclAndElim(name.clone(), this.parse_rule(ccl)?))),
            "Syntax is \"ccl_and_elim <name> <rule>\"."
        ),
        (
            "make_op",
            [
                Exp {data: ExpData::Id(name), ..},
                Exp {data: ExpData::Id(places), start, end, ..},
                pat
            ],
            (this, {
                if let Ok(places) = places.parse::<usize>() {
                    Ok(CmdData::MakeOp(
                        name.clone(),
                        places,
                        this.parse_rule(pat)?
                    ))
                } else {
                    Err(LocalizedError::new(*start, *end, "Expected a number."))
                }
            }),
            "Syntax is \"make_op <name> <places> <rule>\"."
        ),
        (
            "conclude",
            [
                Exp {data: ExpData::Id(name), ..},
                Exp {data: ExpData::Id(ccl_name), ..}
            ],
            (_this, Ok(CmdData::Conclude(name.clone(), ccl_name.clone()))),
            "Syntax is \"conclude <name> <ccl_name>\"."
        ),
        (
            "conclude_op",
            [
                Exp {data: ExpData::Id(name), ..},
                Exp {data: ExpData::Id(opname), ..}
            ],
            (_this, Ok(CmdData::ConcludeOp(name.clone(), opname.clone()))),
            "Syntax is \"conclude_op <name> <opname>\"."
        ),
        (
            "then",
            [
                Exp {data: ExpData::Id(name), ..},
                Exp {data: ExpData::Id(ccl_name), ..}
            ],
            (_this, Ok(CmdData::Then(name.clone(), ccl_name.clone()))),
            "Syntax is \"then <name> <ccl_name>\"."
        ),
        (
            "so_forall",
            [
                Exp {data: ExpData::Id(name), ..},
                Exp {data: ExpData::Id(varname), ..},
                Exp {data: ExpData::Id(ccl_name), ..}
            ],
            (_this, Ok(
                CmdData::SoForall(name.clone(), varname.clone(), ccl_name.clone())
            )),
            "Syntax is \"so_forall <name> <varname> <ccl_name>\"."
        ),
        (
            "forall",
            [
                Exp {data: ExpData::Id(name), ..},
                Exp {data: ExpData::Id(varname), ..},
                Exp {data: ExpData::Id(ccl_name), ..}
            ],
            (_this, Ok(CmdData::Forall(name.clone(), varname.clone(), ccl_name.clone()))),
            "Syntax is \"forall <name> <varname> <ccl_name>\"."
        ),
        (
            "elim_exists",
            [
                Exp {data: ExpData::Id(name), ..},
                Exp {data: ExpData::Id(ccl_name), ..},
                ccl_pat
            ],
            (this, Ok(CmdData::ElimExists(
                name.clone(), ccl_name.clone(), this.parse_prop(ccl_pat)?
            ))),
            "Syntax is \"elim_exists <name> <ccl_name> <ccl>\"."
        ),
        (
            "print",
            [
                Exp {data: ExpData::Id(name), ..}
            ],
            (_this, Ok(CmdData::Print(name.clone()))),
            "Syntax is \"print <name>\"."
        ),
        (
            "print_op",
            [
                Exp {data: ExpData::Id(name), ..}
            ],
            (_this, Ok(CmdData::PrintOp(name.clone()))),
            "Syntax is \"print_op <name>\"."
        ),
        (
            "print_all",
            [],
            (_this, Ok(CmdData::PrintAll)),
            "Syntax is \"print_all\"."
        )]
    );
}
