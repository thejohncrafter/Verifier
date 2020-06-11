
use std::rc::Rc;
use std::collections::{HashMap};

use parser::tokenizer::{Tokenizer, Parser};
use parser::rule_parser::RuleParser;

use core::base_types::*;
use core::templates::*;
use core::matcher::*;

pub struct TestTheory {
    pub t_exists: Rc<dyn TquTemplate>,
    pub t_so_forall: Rc<dyn TquTemplate>,

    quas: HashMap<String, Rc<dyn TquTemplate>>,
    funs: HashMap<String, Rc<dyn FunTemplate>>,

    var_is_term_rule: Rule,
    var_is_prop_rule: Rule,

    top_is_prop_rule: Rule,
    bot_is_prop_rule: Rule,

    eq_is_prop_rule: Rule,
    in_is_prop_rule: Rule,

    so_forall_is_prop_rule: Rule,
    forall_is_prop_rule: Rule,
    exists_is_prop_rule: Rule,
    exists_unique_is_prop_rule: Rule,

    and_is_prop_rule: Rule,
    or_is_prop_rule: Rule,
    then_is_prop_rule: Rule,
    iff_is_prop_rule: Rule,
    not_is_prop_rule: Rule,

    intro_hyp_rule: Rule,

    intro_so_forall_rule: Rule,
    elim_so_forall_rule: Rule,
    intro_forall_rule: Rule,
    elim_forall_rule: Rule,
    intro_exists_rule: Rule,
    elim_exists_rule: Rule,
    intro_exists_unique_rule: Rule,
    elim_exists_unique_rule: Rule,

    intro_top_rule: Rule,
    intro_bot_rule: Rule,
    elim_bot_rule: Rule,

    intro_and_rule: Rule,
    elim_and_l_rule: Rule,
    elim_and_r_rule: Rule,

    intro_or_l_rule: Rule,
    intro_or_r_rule: Rule,
    elim_or_rule: Rule,

    intro_then_rule: Rule,
    elim_then_rule: Rule,
    intro_iff_rule: Rule,
    elim_iff_l_rule: Rule,
    elim_iff_r_rule: Rule,
}

impl TestTheory {
    pub fn new() -> TestTheory {
        let t_so_forall = Rc::new(Template::new("∀(Prop)")) as Rc<dyn TquTemplate>;
        let t_forall = Rc::new(Template::new("∀")) as Rc<dyn TquTemplate>;
        let t_exists = Rc::new(Template::new("∃")) as Rc<dyn TquTemplate>;
        let t_exists_unique = Rc::new(Template::new("∃!")) as Rc<dyn TquTemplate>;

        let t_term = Rc::new(TypeTemplate::new("Term")) as Rc<dyn FunTemplate>;
        let t_prop = Rc::new(TypeTemplate::new("Prop")) as Rc<dyn FunTemplate>;
        let t_true = Rc::new(TypeTemplate::new("True")) as Rc<dyn FunTemplate>;
        let t_op = Rc::new(Template::new("Op")) as Rc<dyn FunTemplate>;

        let t_top = Rc::new(Template::new("⊤")) as Rc<dyn FunTemplate>;
        let t_bot = Rc::new(Template::new("⊥")) as Rc<dyn FunTemplate>;
        let t_opcall = Rc::new(CallTemplate::new()) as Rc<dyn FunTemplate>;

        let t_and = Rc::new(BinaryTemplate::new("∧", true)) as Rc<dyn FunTemplate>;
        let t_or = Rc::new(BinaryTemplate::new("∨", true)) as Rc<dyn FunTemplate>;
        let t_then = Rc::new(BinaryTemplate::new("=>", true)) as Rc<dyn FunTemplate>;
        let t_iff = Rc::new(BinaryTemplate::new("<=>", true)) as Rc<dyn FunTemplate>;
        let t_not = Rc::new(Template::new("¬")) as Rc<dyn FunTemplate>;

        let t_eq = Rc::new(BinaryTemplate::new("=", true)) as Rc<dyn FunTemplate>;
        let t_in = Rc::new(BinaryTemplate::new("∈", true)) as Rc<dyn FunTemplate>;

        let mut quas = HashMap::new();
        quas.insert("t_so_forall".to_string(), Rc::clone(&t_so_forall));
        quas.insert("t_forall".to_string(), Rc::clone(&t_forall));
        quas.insert("t_exists".to_string(), Rc::clone(&t_exists));
        quas.insert("t_exists_unique".to_string(), Rc::clone(&t_exists_unique));

        let mut funs = HashMap::new();
        funs.insert("t_term".to_string(), t_term);
        funs.insert("t_prop".to_string(), t_prop);
        funs.insert("t_true".to_string(), t_true);
        funs.insert("t_op".to_string(), t_op);

        funs.insert("t_top".to_string(), t_top);
        funs.insert("t_bot".to_string(), t_bot);
        funs.insert("t_opcall".to_string(), t_opcall);

        funs.insert("t_and".to_string(), t_and);
        funs.insert("t_or".to_string(), t_or);
        funs.insert("t_then".to_string(), t_then);
        funs.insert("t_iff".to_string(), t_iff);
        funs.insert("t_not".to_string(), t_not);

        funs.insert("t_eq".to_string(), t_eq);
        funs.insert("t_in".to_string(), t_in);

        let decl = include_str!("./test_theory.txt");

        let tz = Tokenizer::new(decl.chars());
        let data = Parser::new(tz).parse();
        let parser = RuleParser::new(&quas, &funs);
        let mut rules = match parser.parse_rules(&data) {
            Ok(r) => r,
            Err(e) => {
                e.pretty_display(&mut std::io::stdout(), &decl).unwrap();
                panic!()
            }
        };

        let var_is_term_rule = rules.remove("var_is_term").unwrap();
        let var_is_prop_rule = rules.remove("var_is_prop").unwrap();

        let top_is_prop_rule = rules.remove("top_is_prop").unwrap();
        let bot_is_prop_rule = rules.remove("bot_is_prop").unwrap();

        let eq_is_prop_rule = rules.remove("eq_is_prop").unwrap();
        let in_is_prop_rule = rules.remove("in_is_prop").unwrap();

        let so_forall_is_prop_rule = rules.remove("so_forall_is_prop").unwrap();
        let forall_is_prop_rule = rules.remove("forall_is_prop").unwrap();
        let exists_is_prop_rule = rules.remove("exists_is_prop").unwrap();
        let exists_unique_is_prop_rule = rules.remove("exists_unique_is_prop").unwrap();

        let and_is_prop_rule = rules.remove("and_is_prop").unwrap();
        let or_is_prop_rule = rules.remove("or_is_prop").unwrap();
        let then_is_prop_rule = rules.remove("then_is_prop").unwrap();
        let iff_is_prop_rule = rules.remove("iff_is_prop").unwrap();
        let not_is_prop_rule = rules.remove("not_is_prop").unwrap();

        let intro_hyp_rule = rules.remove("intro_hyp").unwrap();

        let intro_so_forall_rule = rules.remove("intro_so_forall").unwrap();
        let elim_so_forall_rule = rules.remove("elim_so_forall").unwrap();
        let intro_forall_rule = rules.remove("intro_forall").unwrap();
        let elim_forall_rule = rules.remove("elim_forall").unwrap();
        let intro_exists_rule = rules.remove("intro_exists").unwrap();
        let elim_exists_rule = rules.remove("elim_exists").unwrap();
        let intro_exists_unique_rule = rules.remove("intro_exists_unique").unwrap();
        let elim_exists_unique_rule = rules.remove("elim_exists_unique").unwrap();

        let intro_top_rule = rules.remove("intro_top").unwrap();
        let intro_bot_rule = rules.remove("intro_bot").unwrap();
        let elim_bot_rule = rules.remove("elim_bot").unwrap();

        let intro_and_rule = rules.remove("intro_and").unwrap();
        let elim_and_l_rule = rules.remove("elim_and_l").unwrap();
        let elim_and_r_rule = rules.remove("elim_and_r").unwrap();

        let intro_or_l_rule = rules.remove("intro_or_l").unwrap();
        let intro_or_r_rule = rules.remove("intro_or_r").unwrap();
        let elim_or_rule = rules.remove("elim_or").unwrap();

        let intro_then_rule = rules.remove("intro_then").unwrap();
        let elim_then_rule = rules.remove("elim_then").unwrap();
        let intro_iff_rule = rules.remove("intro_iff").unwrap();
        let elim_iff_l_rule = rules.remove("elim_iff_l").unwrap();
        let elim_iff_r_rule = rules.remove("elim_iff_r").unwrap();

        TestTheory {
            t_exists,
            t_so_forall,

            quas,
            funs,

            var_is_term_rule,
            var_is_prop_rule,

            top_is_prop_rule,
            bot_is_prop_rule,

            eq_is_prop_rule,
            in_is_prop_rule,

            so_forall_is_prop_rule,
            forall_is_prop_rule,
            exists_is_prop_rule,
            exists_unique_is_prop_rule,

            and_is_prop_rule,
            or_is_prop_rule,
            then_is_prop_rule,
            iff_is_prop_rule,
            not_is_prop_rule,

            intro_hyp_rule,

            intro_so_forall_rule,
            elim_so_forall_rule,
            intro_forall_rule,
            elim_forall_rule,
            intro_exists_rule,
            elim_exists_rule,
            intro_exists_unique_rule,
            elim_exists_unique_rule,

            intro_top_rule,
            intro_bot_rule,
            elim_bot_rule,

            intro_and_rule,
            elim_and_l_rule,
            elim_and_r_rule,

            intro_or_l_rule,
            intro_or_r_rule,
            elim_or_rule,

            intro_then_rule,
            elim_then_rule,
            intro_iff_rule,
            elim_iff_l_rule,
            elim_iff_r_rule,
        }
    }

    pub fn make_axioms(&self, decls: Vec<(String, Sequent)>) -> Vec<(String, Sequent)> {
        let decl = "
        rule make_axiom
            ()
            (
                (sequent (meta gamma) (fun t_prop ((meta P))))
            )
            (sequent (meta gamma) (fun t_true ((meta P))))
        ";
        let tz = Tokenizer::new(decl.chars());
        let data = Parser::new(tz).parse();
        let parser = RuleParser::new(&self.quas, &self.funs);
        let (_, rule) = match parser.parse_rule(&data) {
            Ok(r) => r,
            Err(e) => {
                e.pretty_display(&mut std::io::stdout(), &decl).unwrap();
                panic!()
            }
        };

        let mut axioms = Vec::with_capacity(decls.len());
        match decls.into_iter().try_for_each(|(name, decl)| -> Result {
            axioms.push((name, Matcher::match_rule(
                &rule,
                vec!(),
                vec!(),
                vec!(&decl)
            )?));
            Ok(())
        }) {
            Ok(_) => (),
            Err(e) => {
                println!("error : {}", e);
                panic!()
            }
        }

        axioms
    }

    pub fn push_hyp(&self, seq: &Sequent, index: usize) -> SequentResult {
        struct Visitor;

        impl Visitor {
            fn get_at(
                &self, hyp: &HypStack, index: usize
            ) -> std::result::Result<Term, Box<dyn std::error::Error>> {
                if index == 0 {
                    match hyp.value() {
                        Some(Jugement::Truth(t)) => Ok(*t.clone()),
                        Some(_) => Err(Error::create("Not a truth jugement.")),
                        None => Err(Error::create("Out of hypotheses."))
                    }
                } else {
                    match hyp.dest() {
                        Some((_, h2)) => self.get_at(&h2, index - 1),
                        None => Err(Error::create("Out of hypotheses."))
                    }
                }
            }
        }

        let visitor = Visitor {};
        Ok(Sequent {
            hyp: Rc::clone(&seq.hyp),
            ccl: Some(Box::new(visitor.get_at(&seq.hyp, index)?))
        })
    }

    pub fn share_ccl(&self, source: &Sequent, receiver: &Sequent) -> SequentResult {
        struct Visitor;

        impl Visitor {
            fn contains(
                &self, container: &HypStack, what: &HypStack
            ) -> bool {
                *what == HypStack::Void || what == container ||
                match container.dest() {
                    Some((_, next)) => self.contains(&next, what),
                    None => false
                }
            }
        }

        let visitor = Visitor {};
        if visitor.contains(&receiver.hyp, &source.hyp) {
            Ok(Sequent {
                hyp: Rc::clone(&receiver.hyp),
                ccl: source.ccl.clone(),
            })
        } else {
            Err(Error::create("Malformed sequent."))
        }
    }

    pub fn make_op(
        &self, places: usize, opname: &str, seq: &Sequent
    ) -> std::result::Result<(Sequent, Sequent), Box<dyn std::error::Error>> {
        fn wrap_qua(n: usize, around: &str) -> String {
            if n == 0 {
                format!("{}", around)
            } else {
                wrap_qua(n - 1, &format!("qua t_forall x{} ({})", n, around))
            }
        }
        fn arg_list(n: usize, argname: &str) -> String {
            if n == 0 {
                "".to_string()
            } else {
                format!("{} (var {}{})", arg_list(n - 1, argname), argname, n)
            }
        }

        let decl_seq = format!("
            sequent (meta gamma) (fun t_true ((
                {}
            )))
            ",
            wrap_qua(places, "fun t_then ((meta C) (qua t_exists_unique u (meta P)))")
        );
        let decl_out_1 = format!("
            sequent (meta gamma) (fun t_op ((var op) {})))
            ",
            arg_list(places, "a")
        );
        let decl_out_2 = format!("
            sequent (meta gamma) (fun t_true ((
                {}
            )))
            ",
            wrap_qua(
                places,
                &format!("
                    fun t_then ((meta C) (replace (meta P) u (fun t_opcall (
                        (var op) {}
                    ))))
                    ",
                    arg_list(places, "x")
                )
            )
        );

        fn parse_sequent_pat(parser: &RuleParser, decl: &str) -> SequentPattern {
            let tz = Tokenizer::new(decl.chars());
            let data = Parser::new(tz).parse();
            match parser.parse_sequent(&data) {
                Ok(p) => p,
                Err(e) => {
                    e.pretty_display(&mut std::io::stdout(), &decl).unwrap();
                    panic!()
                }
            }
        }

        let mut matcher = Matcher::new();
        (1..=places).for_each(|i| {matcher.register_var(
            format!("a{}", i),
            Rc::new(Template::new(&format!("arg{}", i))) as Rc<dyn VarTemplate>
        )});
        matcher.register_var("op".to_string(), Rc::new(Template::new(opname)));

        let parser = RuleParser::new(&self.quas, &self.funs);
        let seq_pat = parse_sequent_pat(&parser, &decl_seq);
        let out_1_pat = parse_sequent_pat(&parser, &decl_out_1);
        let out_2_pat = parse_sequent_pat(&parser, &decl_out_2);

        matcher.match_sequent(&seq_pat, &seq)?;

        match (matcher.build_sequent(&out_1_pat), matcher.build_sequent(&out_2_pat)) {
            (Ok(out1), Ok(out2)) => {
                Ok((out1, out2))
            },
            (Err(e), _) | (_, Err(e)) => Err(e),
        }
    }

    pub fn op_is_term(
        &self, places: usize, op_decl: &Sequent, args: Vec<Sequent>
    ) -> SequentResult {
        fn arg_list(n: usize, argname: &str) -> String {
            if n == 0 {
                "".to_string()
            } else {
                format!("{} (var {}{})", arg_list(n - 1, argname), argname, n)
            }
        }
        fn wrap_replace(n: usize, around: &str) -> String {
            if n == 0 {
                format!("{}", around)
            } else {
                wrap_replace(n - 1, &format!("replace ({}) arg{} (var x{})", around, n, n))
            }
        }

        let decl = format!("
            rule op_is_term
                ()
                (
                    {}
                    (sequent (meta gamma) (fun t_op ((var op)
                        {}
                    )))
                )
                (sequent (meta gamma) (fun t_term (
                    ({})
                )))
            ",
            (1..=places).map(|i| {
                format!("(sequent (meta gamma) (fun t_term ((var x{}))))", i)
            }).collect::<Vec<String>>().join(""),
            arg_list(places, "arg"),
            wrap_replace(places, &format!(
                "fun t_opcall ((var op) {})",
                arg_list(places, "x")
            ))
        );

        let parser = RuleParser::new(&self.quas, &self.funs);
        let tz = Tokenizer::new(decl.chars());
        let data = Parser::new(tz).parse();
        let r = match parser.parse_rule(&data) {
            Ok((_, r)) => r,
            Err(e) => {
                e.pretty_display(&mut std::io::stdout(), &decl).unwrap();
                panic!()
            }
        };

        let mut rule_args: Vec<&Sequent> = args.iter().collect();
        rule_args.push(op_decl);

        Matcher::match_rule(
            &r,
            vec!(),
            vec!(),
            rule_args
        )
    }

    pub fn var_is_term(&self, varname: &str, seq: &Sequent) -> SequentResult {
        Matcher::match_rule(
            &self.var_is_term_rule,
            vec!(Rc::new(Template::new(varname))),
            vec!(),
            vec!(seq)
        )
    }

    pub fn var_is_prop(&self, varname: &str, seq: &Sequent) -> SequentResult {
        Matcher::match_rule(
            &self.var_is_prop_rule,
            vec!(Rc::new(Template::new(varname))),
            vec!(),
            vec!(seq)
        )
    }

    pub fn top_is_prop(&self, seq: &Sequent) -> SequentResult {
        Matcher::match_rule(&self.top_is_prop_rule, vec!(), vec!(), vec!(seq))
    }

    pub fn bot_is_prop(&self, seq: &Sequent) -> SequentResult {
        Matcher::match_rule(&self.bot_is_prop_rule, vec!(), vec!(), vec!(seq))
    }

    pub fn eq_is_prop(&self, seq1: &Sequent, seq2: &Sequent) -> SequentResult {
        Matcher::match_rule(&self.eq_is_prop_rule, vec!(), vec!(), vec!(seq1, seq2))
    }

    pub fn in_is_prop(&self, seq1: &Sequent, seq2: &Sequent) -> SequentResult {
        Matcher::match_rule(&self.in_is_prop_rule, vec!(), vec!(), vec!(seq1, seq2))
    }

    pub fn so_forall_is_prop(&self, varname: &str, seq: &Sequent) -> SequentResult {
        Matcher::match_rule(
            &self.so_forall_is_prop_rule,
            vec!(Rc::new(Template::new(varname))),
            vec!(),
            vec!(&seq)
        )
    }

    pub fn forall_is_prop(&self, varname: &str, seq: &Sequent) -> SequentResult {
        Matcher::match_rule(
            &self.forall_is_prop_rule,
            vec!(Rc::new(Template::new(varname))),
            vec!(),
            vec!(&seq)
        )
    }

    pub fn exists_is_prop(&self, varname: &str, seq: &Sequent) -> SequentResult {
        Matcher::match_rule(
            &self.exists_is_prop_rule,
            vec!(Rc::new(Template::new(varname))),
            vec!(),
            vec!(&seq)
        )
    }

    pub fn exists_unique_is_prop(&self, varname: &str, seq: &Sequent) -> SequentResult {
        Matcher::match_rule(
            &self.exists_unique_is_prop_rule,
            vec!(Rc::new(Template::new(varname))),
            vec!(),
            vec!(&seq)
        )
    }

    pub fn and_is_prop(&self, seq1: &Sequent, seq2: &Sequent) -> SequentResult {
        Matcher::match_rule(&self.and_is_prop_rule, vec!(), vec!(), vec!(seq1, seq2))
    }

    pub fn or_is_prop(&self, seq1: &Sequent, seq2: &Sequent) -> SequentResult {
        Matcher::match_rule(&self.or_is_prop_rule, vec!(), vec!(), vec!(seq1, seq2))
    }

    pub fn then_is_prop(&self, seq1: &Sequent, seq2: &Sequent) -> SequentResult {
        Matcher::match_rule(&self.then_is_prop_rule, vec!(), vec!(), vec!(seq1, seq2))
    }

    pub fn iff_is_prop(&self, seq1: &Sequent, seq2: &Sequent) -> SequentResult {
        Matcher::match_rule(&self.iff_is_prop_rule, vec!(), vec!(), vec!(seq1, seq2))
    }

    pub fn not_is_prop(&self, seq: &Sequent) -> SequentResult {
        Matcher::match_rule(&self.not_is_prop_rule, vec!(), vec!(), vec!(seq))
    }

    pub fn intro_hyp(&self, seq: &Sequent) -> SequentResult {
        Matcher::match_rule(&self.intro_hyp_rule, vec!(), vec!(), vec!(seq))
    }

    pub fn intro_so_forall(&self, varname: &str, seq: &Sequent) -> SequentResult {
        Matcher::match_rule(
            &self.intro_so_forall_rule,
            vec!(Rc::new(Template::new(varname))),
            vec!(),
            vec!(&seq)
        )
    }

    pub fn elim_so_forall(&self, seq1: &Sequent, seq2: &Sequent) -> SequentResult {
        Matcher::match_rule(&self.elim_so_forall_rule, vec!(), vec!(), vec!(seq1, seq2))
    }

    pub fn intro_forall(&self, varname: &str, seq: &Sequent) -> SequentResult {
        Matcher::match_rule(
            &self.intro_forall_rule,
            vec!(Rc::new(Template::new(varname))),
            vec!(),
            vec!(&seq)
        )
    }

    pub fn elim_forall(&self, seq1: &Sequent, seq2: &Sequent) -> SequentResult {
        Matcher::match_rule(&self.elim_forall_rule, vec!(), vec!(), vec!(seq1, seq2))
    }

    pub fn intro_exists(
        &self, varname: &str, seq1: &Sequent, seq2: &Sequent, seq3: &Sequent
    ) -> SequentResult {
        Matcher::match_rule(
            &self.intro_exists_rule,
            vec!(Rc::new(Template::new(varname))),
            vec!(),
            vec!(seq1, seq2, seq3)
        )
    }

    pub fn intro_exists_unique(
        &self, varname: &str, seq1: &Sequent, seq2: &Sequent
    ) -> SequentResult {
        Matcher::match_rule(
            &self.intro_exists_unique_rule,
            vec!(Rc::new(Template::new(varname))),
            vec!(),
            vec!(seq1, seq2)
        )
    }

    pub fn elim_exists_unique(
        &self, varname1: &str, varname2: &str, seq: &Sequent
    ) -> SequentResult {
        Matcher::match_rule(
            &self.elim_exists_unique_rule,
            vec!(Rc::new(Template::new(varname1)), Rc::new(Template::new(varname2))),
            vec!(),
            vec!(seq)
        )
    }

    pub fn elim_exists(&self, seq1: &Sequent, seq2: &Sequent, seq3: &Sequent) -> SequentResult {
        Matcher::match_rule(&self.elim_exists_rule, vec!(), vec!(), vec!(seq1, seq2, seq3))
    }

    pub fn intro_top(&self, seq: &Sequent) -> SequentResult {
        Matcher::match_rule(&self.intro_top_rule, vec!(), vec!(), vec!(seq))
    }

    pub fn intro_bot(&self, seq1: &Sequent, seq2: &Sequent) -> SequentResult {
        Matcher::match_rule(&self.intro_bot_rule, vec!(), vec!(), vec!(seq1, seq2))
    }

    pub fn elim_bot(&self, seq1: &Sequent, seq2: &Sequent) -> SequentResult {
        Matcher::match_rule(&self.elim_bot_rule, vec!(), vec!(), vec!(seq1, seq2))
    }

    pub fn intro_and(&self, seq1: &Sequent, seq2: &Sequent) -> SequentResult {
        Matcher::match_rule(&self.intro_and_rule, vec!(), vec!(), vec!(seq1, seq2))
    }

    pub fn elim_and_l(&self, seq: &Sequent) -> SequentResult {
        Matcher::match_rule(&self.elim_and_l_rule, vec!(), vec!(), vec!(seq))
    }

    pub fn elim_and_r(&self, seq: &Sequent) -> SequentResult {
        Matcher::match_rule(&self.elim_and_r_rule, vec!(), vec!(), vec!(seq))
    }

    pub fn intro_or_l(&self, seq1: &Sequent, seq2: &Sequent) -> SequentResult {
        Matcher::match_rule(&self.intro_or_l_rule, vec!(), vec!(), vec!(seq1, seq2))
    }

    pub fn intro_or_r(&self, seq1: &Sequent, seq2: &Sequent) -> SequentResult {
        Matcher::match_rule(&self.intro_or_r_rule, vec!(), vec!(), vec!(seq1, seq2))
    }

    pub fn elim_or(&self, seq1: &Sequent, seq2: &Sequent, seq3: &Sequent) -> SequentResult {
        Matcher::match_rule(&self.elim_or_rule, vec!(), vec!(), vec!(seq1, seq2, seq3))
    }

    pub fn intro_then(&self, seq: &Sequent) -> SequentResult {
        Matcher::match_rule(&self.intro_then_rule, vec!(), vec!(), vec!(seq))
    }

    pub fn elim_then(&self, seq1: &Sequent, seq2: &Sequent) -> SequentResult {
        Matcher::match_rule(&self.elim_then_rule, vec!(), vec!(), vec!(seq1, seq2))
    }

    pub fn intro_iff(&self, seq1: &Sequent, seq2: &Sequent) -> SequentResult {
        Matcher::match_rule(&self.intro_iff_rule, vec!(), vec!(), vec!(seq1, seq2))
    }

    pub fn elim_iff_l(&self, seq: &Sequent) -> SequentResult {
        Matcher::match_rule(&self.elim_iff_l_rule, vec!(), vec!(), vec!(seq))
    }

    pub fn elim_iff_r(&self, seq: &Sequent) -> SequentResult {
        Matcher::match_rule(&self.elim_iff_r_rule, vec!(), vec!(), vec!(seq))
    }
}
