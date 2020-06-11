
use parser::tokenizer::Pos;

#[derive(Debug, Clone)]
pub enum TermPattern {
    Var(String),
    OpCall(String, Vec<Box<TermPattern>>),
}

#[derive(Debug, Clone)]
pub enum PropPattern {
    Var(String),

    Top(),
    Bot(),

    SoForall(String, Box<PropPattern>),
    Forall(String, Box<PropPattern>),
    Exists(String, Box<PropPattern>),
    ExistsUnique(String, Box<PropPattern>),

    And(Box<PropPattern>, Box<PropPattern>),
    Or(Box<PropPattern>, Box<PropPattern>),
    Then(Box<PropPattern>, Box<PropPattern>),
    Iff(Box<PropPattern>, Box<PropPattern>),
    Not(Box<PropPattern>),

    Equ(Box<TermPattern>, Box<TermPattern>),
    In(Box<TermPattern>, Box<TermPattern>)
}

#[derive(Debug)]
pub enum RuleData {
    Get(String),

    OpDef(String),
    IntroAnd(Box<RulePattern>, Box<RulePattern>),
    ElimAndL(Box<RulePattern>),
    ElimAndR(Box<RulePattern>),
    IntroOrL(Box<RulePattern>, PropPattern),
    IntroOrR(PropPattern, Box<RulePattern>),
    IntroIff(Box<RulePattern>, Box<RulePattern>),
    ElimIffL(Box<RulePattern>),
    ElimIffR(Box<RulePattern>),

    IntroTop(),
    IntroBot(Box<RulePattern>, Box<RulePattern>),
    ElimBot(PropPattern, Box<RulePattern>),

    IntroExists(String, PropPattern, TermPattern, Box<RulePattern>),
    IntroExistsUnique(String, PropPattern, Box<RulePattern>),
    ElimExistsUnique(String, String, Box<RulePattern>),

    ElimThen(Box<RulePattern>, Box<RulePattern>),
    ElimSoForall(PropPattern, Box<RulePattern>),
    ElimForall(TermPattern, Box<RulePattern>),
}

#[derive(Debug)]
pub struct RulePattern {
    pub data: RuleData,
    pub start: Pos,
    pub end: Pos
}

#[derive(Debug)]
pub enum CmdData {
    Clear,

    Scope,
    Prop(String),
    Hyp(String, PropPattern),
    Term(String),
    Instantiate(String, String, PropPattern, RulePattern),
    Reg(String, RulePattern),

    Switch(RulePattern, String, PropPattern, String, PropPattern),
    CclAndSwitch(RulePattern),
    CclAndElim(String, RulePattern),

    MakeOp(String, usize, RulePattern),

    Conclude(String, String),
    ConcludeOp(String, String),
    Then(String, String),
    SoForall(String, String, String),
    Forall(String, String, String),
    ElimExists(String, String, PropPattern),

    Print(String),
    PrintOp(String),
    PrintAll,
}

#[derive(Debug)]
pub struct CmdPattern {
    pub data: CmdData,
    pub start: Pos,
    pub end: Pos
}
