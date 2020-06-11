/**
 * This file contains development tests.
 **/

extern crate wasm_bindgen;
extern crate crossbeam_channel;
extern crate notify;

mod core;
mod parser;
mod runner;

use crossbeam_channel::unbounded;
use notify::{RecommendedWatcher, RecursiveMode, Watcher};

use std::fs::File;
use std::io::Read;
use std::time::Duration;

use core::base_types::*;

use runner::runner::*;
use runner::stack_helper::*;
use runner::proof_types::*;

use core::printing::*;

use parser::tokenizer::*;

/*

and_comm = prop A => prop B => {
    a = prop A => prop B => hyp H: (A and B) => intro_and (elim_and_l H) (elim_and_r H);
    dir = elim_forall B (elim_forall A (a)); // a.so_spe(A).so_spe(B)
    rec = elim_forall A (elim_forall B (a)); // a.so_spe(B).so_spe(A)
    intro_iff dir rec
};

or_id = prop A => {
    dir = hyp H: (A or A) => {
        switch H (cA: A => cA) (cA: A => cA)
    };
    rec = hyp H: A => intro_or_r A H;
    intro_iff dir rec
};

term x => term y =>
    hyp H:
        (exists C (x in C) and (forall z (not (z in x)))) and
        (exists C (y in C) and (forall z (not (z in y)))
    => {
    elim_then (elim_forall y (elim_forall x axiom_ext)) (term z => {
        hyp z_in_x: (z in x) => {
            // H.left().right().spe(z)
            // H > left > right > spe z
            not_z_in_x = elim_forall z (elim_and_r (elim_and_l H));
            bot = intro_bot z_in_x not_z_in_x;
            elim_bot (z in y) bot
        };
        hyp z_in_y: (z in x) => {
            not_z_in_y = elim_forall z (elim_and_r (elim_and_r H));
            bot = intro_bot z_in_y not_z_in_y;
            elim_bot (z in x) bot
        };
        intro_iff z_in_x z_in_y
    })
}

term a => term b =>
    term x => term y =>
        hyp H:
            (exists C (x in C) and (forall z (iff (z in x) (or (z eq a) (z eq b))))) and
            (exists C (y in C) and (forall z (iff (z in y) (or (z eq a) (z eq b)))))
        => {
            term z => hyp z_in_a: (z in a) => {
                switch (H.left().right().spe(z).dir().ponens(z_in_a))
                    (z_is_a: (z eq a) => {
                        H.right().right().spe(z).rec().ponens(intro_or_l z_is_a (z eq b))
                    })
                    (z_is_b: (z eq b) => {
                        H.right().right().spe(z).rec().ponens(intro_or_r (z eq a) z_is_b)
                    })
            }
        }

axiom_ext.spe(x).spe(y).ponens(a)

*/

fn main() -> Result {

    // Manual proof example.
    if false {
        let axioms_def = include_str!("./axioms.txt");

        let mut runner = Runner::new();
        runner.register_axioms(axioms_def)?;

        let axioms = runner.axioms;
        let parser = runner.parser;
        let th = runner.th;

        let mut h = StackHelper::new(&th);

        match axioms.iter().try_for_each(|(name, seq)| -> Result {
            h.register_prop(&name, seq.clone())?;
            Ok(())
        }) {
            Ok(()) => (),
            Err(e) => {
                eprintln!("{:?}", e)
            }
        }

        let hyp = |
            h: &mut StackHelper, name: &str, decl: &str,
            f: &mut dyn Fn(&mut StackHelper) -> SequentResult
        | -> SequentResult {
            let tz = Tokenizer::new(decl.chars());
            let data = Parser::new(tz).parse();
            let pat = parser.parse_prop(&data)?;
            h.hyp(name, &pat)?;
            let a = f(h)?;
            h.then_func(&a)
        };

        let prop = |
            h: &mut StackHelper, name: &str,
            f: &mut dyn Fn(&mut StackHelper) -> SequentResult
        | -> SequentResult {
            h.prop(name)?;
            let a = f(h)?;
            h.so_forall_func(name, &a)
        };

        let mut out = std::io::stdout();

        let t = prop(&mut h, "A", &mut |h| {prop(h, "B", &mut |h| {
                let a = prop(h, "A", &mut |h| {prop(h, "B", &mut |h| {
                        hyp(h, "H", "and (var A) (var B)", &mut |h| {
                            th.intro_and(
                                &th.elim_and_r(&h.get_prop("H")?)?,
                                &th.elim_and_l(&h.get_prop("H")?)?
                            )
                        })
                    })
                })?;
                let dir = th.elim_so_forall(
                    &h.build_prop(&PropPattern::Var("B".to_string()))?,
                    &th.elim_so_forall(
                        &h.build_prop(&PropPattern::Var("A".to_string()))?,
                        &a
                    )?
                )?;
                let rec = th.elim_so_forall(
                    &h.build_prop(&PropPattern::Var("A".to_string()))?,
                    &th.elim_so_forall(
                        &h.build_prop(&PropPattern::Var("B".to_string()))?,
                        &a
                    )?
                )?;
                th.intro_iff(&dir, &rec)
            })
        })?;
        print_sequent(&mut out, &t)?;
    }

    // Proof from file (will check every time the file is modified).
    if true {
        let (tx, rx) = unbounded();
        let mut watcher: RecommendedWatcher = Watcher::new(tx, Duration::from_secs(2))?;
        watcher.watch("./test_proof.txt", RecursiveMode::Recursive)?;

        let axioms_def = include_str!("./axioms.txt");

        let mut runner = Runner::new();
        runner.register_axioms(axioms_def)?;

        let mut exec = || -> Result {
            let mut file = File::open("./test_proof.txt")?;
            let mut contents = String::new();
            file.read_to_string(&mut contents)?;

            print!("{}[2J", 27 as char);
            print!("{}[0;0H", 27 as char);
            println!();

            let decl = contents;

            match runner.exec_proof(&decl) {
                Ok(res) => print!("{}", res),
                Err(e) => {
                    e.pretty_display(&mut std::io::stdout(), &decl).unwrap();
                }
            };

            Ok(())
        };

        exec()?;

        loop {
            match rx.recv() {
                Ok(_) => exec()?,
                Err(err) => println!("watch error: {:?}", err),
            };
        }
    }

    Ok(())
}
