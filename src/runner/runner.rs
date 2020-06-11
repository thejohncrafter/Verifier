
use core::test_theory::*;
use core::base_types::*;

use parser::error::LocalizedError;
use parser::tokenizer::*;
use parser::proof_parser::*;

use runner::stack_helper::*;
use runner::cmd_caller::*;

pub struct Runner {
    pub th: TestTheory,
    pub parser: ProofParser,
    pub axioms: Vec<(String, Sequent)>,
}

impl Runner {
    pub fn new() -> Runner {
        Runner {
            th: TestTheory::new(),
            parser: ProofParser::new(),
            axioms: vec!()
        }
    }

    pub fn register_axioms(&mut self, axioms_def: &str) -> Result {
        let h = StackHelper::new(&self.th);

        let tz = Tokenizer::new(axioms_def.chars());
        let data = Parser::new(tz).parse();
        let axiom_pats = match self.parser.parse_axioms(&data) {
            Ok(pats) => pats,
            Err(e) => {
                e.pretty_display(&mut std::io::stdout(), &axioms_def).unwrap();
                Err(e)?
            }
        };

        let mut decls = Vec::with_capacity(axiom_pats.len());
        axiom_pats.into_iter().try_for_each(|(name, pat)| -> Result {
            decls.push((name, h.build_prop(&pat)?));
            Ok(())
        })?;

        self.axioms = self.th.make_axioms(decls);
        Ok(())
    }

    pub fn exec_proof(
        &mut self, decl: &str
    ) -> std::result::Result<String, LocalizedError> {
        let parser = ProofParser::new();

        let mut h = StackHelper::new(&self.th);

        match self.axioms.iter().try_for_each(|(name, seq)| -> Result {
            h.register_prop(&name, seq.clone())?;
            Ok(())
        }) {
            Ok(()) => (),
            Err(e) => {
                return Err(LocalizedError::new(
                    Pos {i: 0, line: 0, column: 0},
                    Pos {i: 0, line: 0, column: 0},
                    &format!("{}", e)
                ))
            }
        }

        let tz = Tokenizer::new(decl.chars());
        let data = Parser::new(tz).parse();

        let r = parser.parse_proof(&data)?;
        let mut caller = CmdCaller::new(&mut h, &self.th);

        let mut buf = vec!();

        r.iter().try_for_each(|r| -> LocRes {
            caller.call(&mut buf, r)
        })?;
        Ok(String::from_utf8(buf).unwrap())
    }
}
