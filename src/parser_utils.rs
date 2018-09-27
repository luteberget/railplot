use std::iter::Peekable;
use std::fmt::Debug;

#[derive(Debug,Clone,Fail)]
pub enum LexerError {
    #[fail(display = "unexpected char at {}: {}", i, c)]
    UnexpectedChar { i: usize, c: String },
    #[fail(display = "unexpected EOF")]
    UnexpectedEOF,
}


#[derive(Debug,Clone, Fail)]
pub enum ParseError {
    #[fail(display = "unexpected token at {}: {}", _0,_1)]
    UnexpectedToken(usize, String),
    #[fail(display = "unexpected EOF")]
    UnexpectedEOF,
    #[fail(display = "parse errors: {:?}", _0)]
    Many(Vec<ParseError>),
    #[fail(display = "unknown name: {},  {}", _0, _1)]
    UnknownName(String, String),
}


pub fn alt<T, Token: PartialEq + Debug + Clone>(i: &mut usize,
                                                tokens: &[Token],
                                                alts: &[&Fn(&mut usize, &[Token])
                                                            -> Result<T, ParseError>])
                                                -> Result<T, ParseError> {
    let start = *i;
    let mut errs = Vec::new();
    for alt in alts {
        *i = start;
        match alt(i, tokens) {
            Ok(x) => return Ok(x),
            Err(y) => errs.push(y),
        }
    }
    Err(ParseError::Many(errs))
}

pub fn must_match<Token: PartialEq + Debug + Clone>(i: &mut usize,
                                                    tokens: &[Token],
                                                    tok: Token)
                                                    -> Result<(), ParseError> {
    if matches(i, tokens, tok) {
        Ok(())
    } else {
        Err(ParseError::UnexpectedToken(*i, format!("{:?}", tokens[*i].clone())))
    }
}

pub fn matches<Token: PartialEq + Debug + Clone>(i: &mut usize,
                                                 tokens: &[Token],
                                                 tok: Token)
                                                 -> bool {
    let r = tokens[*i] == tok;
    if r {
        *i += 1;
    }
    r
}

pub fn consume_while<F>(it: &mut Peekable<&mut Iterator<Item = char>>, x: F) -> Vec<char>
    where F: Fn(char) -> bool
{

    let mut v: Vec<char> = vec![];

    while let Some(&ch) = it.peek() {
        if x(ch) {
            it.next().unwrap();
            v.push(ch);
        } else {
            break;
        }
    }

    v
}
