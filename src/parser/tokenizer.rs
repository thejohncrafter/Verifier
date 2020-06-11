
use std::iter::{Enumerate, Peekable};
use std::str::Chars;
use std::fmt::{Debug, Display};

#[derive(Clone, Copy, Debug)]
pub struct Pos {
    pub i: usize,
    pub line: usize,
    pub column: usize,
}

pub enum TokenData {
    Id(String),
    LPar,
    RPar,
}

impl Display for TokenData {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            TokenData::Id(id) => Debug::fmt(id, f),
            TokenData::LPar => write!(f, "("),
            TokenData::RPar => write!(f, ")"),
        }
    }
}

pub struct Token {
    start: Pos,
    end: Pos,
    data: TokenData,
}

impl Token {
    fn new(start: Pos, end: Pos, data: TokenData) -> Token {
        Token {start, end, data}
    }
}

impl Display for Token {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        Display::fmt(&self.data, f)
    }
}

struct Indexer<'a> {
    i: usize,
    line: usize,
    column: usize,
    iter: Peekable<Enumerate<Chars<'a>>>
}

impl<'a> Indexer<'a> {
    fn new(iter: Chars) -> Indexer {
        Indexer {
            i: 0,
            line: 1,
            column: 1,
            iter: iter.enumerate().peekable()
        }
    }

    fn current_pos(&self) -> Pos {
        Pos {i: self.i, line: self.line, column: self.column}
    }

    fn peek(&mut self) -> Option<(Pos, char)> {
        match self.iter.peek() {
            Some((i, c)) => {
                if *c == '\n' {
                    Some((Pos {i: *i, line: self.line + 1, column: 0}, *c))
                } else {
                    Some((Pos {i: *i, line: self.line, column: self.column + 1}, *c))
                }
            },
            None => None
        }
    }
}

impl<'a> Iterator for Indexer<'a> {
    type Item = (Pos, char);

    fn next(&mut self) -> Option<Self::Item> {
        match self.iter.next() {
            Some((i, c)) => {
                let p = Pos {i, line: self.line, column: self.column};
                self.i = i;
                if c == '\n' {
                    self.line = self.line + 1;
                    self.column = 0;
                } else {
                    self.column = self.column + 1;
                }
                Some((p, c))
            },
            None => None
        }
    }
}

pub struct Tokenizer<'a> {
    iter: Indexer<'a>,
}

impl<'a> Tokenizer<'a> {
    pub fn new(iter: Chars) -> Tokenizer {
        Tokenizer {iter: Indexer::new(iter)}
    }
}

impl<'a> Iterator for Tokenizer<'a> {
    type Item = Token;

    fn next(&mut self) -> Option<Token> {
        loop {
            match self.iter.peek() {
                Some((_, c)) if c == '#' => {
                    loop {
                        match self.iter.peek() {
                            Some((_, '\n')) | None => break,
                            _ => {self.iter.next();}
                        }
                    }
                },
                Some((p, c)) if c == '(' || c == ')' => {
                    self.iter.next();
                    return Some(Token::new(
                        p, Pos {i: p.i + 1, line: p.line, column: p.column + 1},
                        match c {
                            '(' => TokenData::LPar,
                            _ => TokenData::RPar
                        }
                    ))
                },
                Some((_, c)) if !c.is_whitespace() => {
                    break
                },
                Some(_) => {
                    self.iter.next();
                },
                None => {return None}
            }
        }

        let start = match self.iter.peek().unwrap() {(p, _) => p};

        let mut word = vec!();
        loop {
            word.push(match self.iter.next().unwrap() {(_, c) => c});
            match self.iter.peek() {
                Some((_, c)) if c == '#' || c == '(' || c == ')' => {
                    break
                },
                Some((_, c)) if c.is_whitespace() => {
                    break
                },
                None => {break},
                _ => {false}
            };
        }

        Some(Token::new(
            start, self.iter.current_pos(),
            TokenData::Id(word.iter().collect())
        ))
    }
}

#[derive(Debug)]
pub enum ExpData {
    Id(String),
    Tuple(Vec<Exp>),
}

#[derive(Debug)]
pub struct Exp {
    pub start: Pos,
    pub end: Pos,
    pub data: ExpData,
}

impl Exp {
    fn id(start: Pos, end: Pos, data: String) -> Exp {
        Exp {start, end, data: ExpData::Id(data)}
    }

    fn tuple(start: Pos, end: Pos, data: Vec<Exp>) -> Exp {
        Exp {start, end, data: ExpData::Tuple(data)}
    }
}

pub struct Parser<'a> {
    tz: Peekable<Tokenizer<'a>>,
}

impl<'a> Parser<'a> {
    pub fn new(tz: Tokenizer) -> Parser {
        Parser {tz: tz.peekable()}
    }

    fn parse_internal(&mut self) -> Vec<Exp> {
        let mut contents = vec!();

        while self.tz.peek().is_some() {
            let t = self.tz.peek().unwrap();
            match &t.data {
                TokenData::Id(_) => {
                    let token = self.tz.next().unwrap();
                    match token.data {
                        TokenData::Id(id) => contents.push(Exp::id(token.start, token.end, id)),
                        _ => panic!()
                    }
                },
                TokenData::LPar => {
                    let start = self.tz.next().unwrap().start;
                    let args = self.parse_internal();
                    let end = match self.tz.next() {
                        Some(t) => t.end,
                        None => {
                            match args.last() {
                                Some(t) => t.end,
                                None => start
                            }
                        }
                    };
                    contents.push(Exp::tuple(start, end, args));
                },
                TokenData::RPar => {
                    break
                }
            }
        }

        contents
    }

    pub fn parse(&mut self) -> Exp {
        let args = self.parse_internal();
        let start = Pos {i: 0, line: 1, column: 1};
        let end = match args.last() {
            Some(t) => t.end,
            None => start
        };
        Exp::tuple(start, end, args)
    }
}
