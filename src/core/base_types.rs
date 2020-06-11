
use std::rc::Rc;
use std::io::Write;
use std::fmt::Display;

pub struct Error {
    message: String
}

impl Error {
    pub fn create(message: &str) -> Box<Error> {
        Box::new(Error {message: message.to_string()})
    }
}

impl std::fmt::Debug for Error {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        Display::fmt(&self.message, f)
    }
}

impl Display for Error {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        Display::fmt(&self.message, f)
    }
}

impl std::error::Error for Error {

}

pub type Result = std::result::Result<(), Box<dyn std::error::Error>>;
pub type SequentResult = std::result::Result<Sequent, Box<dyn std::error::Error>>;

pub trait VarTemplate {
    fn format(&self, &mut dyn Write) -> Result;
}

pub trait TquTemplate {
    fn format(&self, &mut dyn Write, &dyn VarTemplate, &Term) -> Result;
}

pub trait FunTemplate {
    fn format(&self, &mut dyn Write, &Vec<Box<Term>>) -> Result;
}

pub trait RelTemplate {
    fn format(&self, &mut dyn Write, &Vec<Box<Term>>) -> Result;
}

#[derive(Clone)]
pub enum Term {
    Var(Rc<dyn VarTemplate>),                                       // Variables
    Qua(Rc<dyn TquTemplate>, Rc<dyn VarTemplate>, Box<Term>),       // Quantifiers
    Fun(Rc<dyn FunTemplate>, Vec<Box<Term>>),                       // Functions
}

impl PartialEq for Term {
    fn eq(&self, other: &Self) -> bool {
        struct Visitor;

        impl Visitor {
            fn templates_valid(
                &self,
                t1: &Rc<dyn VarTemplate>,
                t2: &Rc<dyn VarTemplate>,
                vars: &mut Vec<(&Rc<dyn VarTemplate>, &Rc<dyn VarTemplate>)>
            ) -> bool {
                match vars.iter().find(|(k, _)| {
                    Rc::ptr_eq(&k, t1)
                }) {
                    Some((_, v)) => {
                         Rc::ptr_eq(v, t2)
                    },
                    None => Rc::ptr_eq(t1, t2)
                }
            }

            fn is_equal<'a>(
                &self,
                term_a: &'a Term, term_b: &'a Term,
                vars: &mut Vec<(&'a Rc<dyn VarTemplate>, &'a Rc<dyn VarTemplate>)>
            ) -> bool {
                match (term_a, term_b) {
                    (Term::Var(t1), Term::Var(t2)) => {
                        self.templates_valid(t1, t2, vars)
                    },
                    (Term::Qua(te1, v1, t1), Term::Qua(te2, v2, t2)) => {
                        Rc::ptr_eq(te1, te2) && {
                            vars.push((v1, v2));
                            let res = self.is_equal(t1, t2, vars);
                            vars.pop();
                            res
                        }
                    },
                    (Term::Fun(t1, args1), Term::Fun(t2, args2)) => {
                        Rc::ptr_eq(t1, t2) &&
                        args1.len() == args2.len() &&
                        args1.iter().zip(args2).all(|(a1, a2)| {
                            self.is_equal(a1, a2, vars)
                        })
                    },
                    _ => false
                }
            }
        }

        let visitor = Visitor {};
        visitor.is_equal(self, other, &mut vec!())
    }
}

impl Eq for Term {}

#[derive(Clone)]
pub enum Jugement {
    Intro(Rc<dyn VarTemplate>),
    Truth(Box<Term>),
}

impl PartialEq for Jugement {
    fn eq(&self, other: &Self) -> bool {
        match (self, other) {
            (Jugement::Intro(ta), Jugement::Intro(tb)) => Rc::ptr_eq(ta, tb),
            (Jugement::Truth(fa), Jugement::Truth(fb)) => fa == fb,
            _ => false
        }
    }
}

pub enum HypStack {
    Void,
    Cons(Jugement, Rc<HypStack>),
}

impl HypStack {
    pub fn new() -> HypStack {
        HypStack::Void
    }

    pub fn value(&self) -> Option<&Jugement> {
        match self {
            HypStack::Void => None,
            HypStack::Cons(j, _) => Some(j)
        }
    }

    pub fn cons(self: Rc<HypStack>, j: Jugement) -> HypStack {
        HypStack::Cons(j, self)
    }

    pub fn dest(&self) -> Option<(&Jugement, Rc<HypStack>)> {
        match self {
            HypStack::Void => None,
            HypStack::Cons(j, stack) => {
                Some((j, Rc::clone(stack)))
            }
        }
    }

    pub fn is_empty(&self) -> bool {
        match self {
            HypStack::Void => true,
            HypStack::Cons(_, _) => false
        }
    }

    pub fn iter<'a>(self: Rc<Self>) -> StackIterator {
        StackIterator::new(self)
    }
}

impl PartialEq for HypStack {
    fn eq(&self, other: &Self) -> bool {
        fn descend(a: &HypStack, b: &HypStack) -> bool {
            match (a, b) {
                (HypStack::Void, HypStack::Void) => true,
                (HypStack::Cons(ja, stacka), HypStack::Cons(jb, stackb)) =>
                    *ja == *jb && descend(stacka, stackb),
                _ => false
            }
        }
        descend(self, other)
    }
}

impl Eq for HypStack {}

pub struct StackIterator {
    stack: Rc<HypStack>
}

impl StackIterator {
    fn new(stack: Rc<HypStack>) -> StackIterator {
        StackIterator {stack}
    }
}

impl<'a> Iterator for StackIterator {
    type Item = Jugement;

    fn next(&mut self) -> Option<Self::Item> {
        let (j, stack) = match self.stack.dest() {
            Some((j, stack)) => {
                Some((j.clone(), stack))
            }
            None => None
        }?;
        self.stack = stack;
        Some(j)
    }
}

#[derive(Clone)]
pub struct Sequent {
    pub hyp: Rc<HypStack>,
    pub ccl: Option<Box<Term>>,
}
