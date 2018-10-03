use parser_utils::*;
use std::path::Path;
use failure;

// Parser input model

type Position = f64;
type Id = String;
//use input::{Side, Dir, Port};
#[derive(Debug, Copy, Clone)]
pub enum Side { Left, Right }
#[derive(Debug, Copy, Clone, PartialEq, Eq, Hash)]
pub enum Dir { Up, Down }
#[derive(Debug, Copy, Clone, Hash, PartialEq, Eq)]
pub enum Port { In, Out, Left, Right, Trunk, Top, Bottom, TopBottom /* Unknown top/bottom */ }
#[derive(Debug)]
pub enum Shape { Begin, End, Switch(Side, Dir), Vertical, }
type PortRef = (Id, Port); 

#[derive(Debug)]
pub enum Stmt {
    Node(String, Shape, Position),
    Edge(PortRef, PortRef),
}

#[derive(Clone, PartialEq, Debug)]
enum Token {
    Sep,
    Number(f64),
    Id(String),
    EOF,
}

pub fn file_contents(f :&Path) -> Result<String, failure::Error> {
    use std::fs::File;
    use std::io::prelude::*;
    use std::io::BufReader;

    let file = File::open(f)?;
    let mut file = BufReader::new(&file);
    let mut contents = String::new();
    file.read_to_string(&mut contents)?;
    Ok(contents)
}

pub fn read_file(s :&Path) -> Result<Vec<Stmt>, failure::Error> {
    read_string(&file_contents(s)?)
}

pub fn read_string(s:&str) -> Result<Vec<Stmt>, failure::Error> {
    let lex = lexer(&mut s.chars())?;
    let res = parse(&lex)?;
    Ok(res)
}

fn parse(t: &[Token]) -> Result<Vec<Stmt>, ParseError> {
    let mut i = 0;
    let mut res = Vec::new();
    while t[i] != Token::EOF {
        //if let Some(stmt) = parse_stmt(&mut i, t)? {
        //    res.push(stmt);
        //}
        res.push(parse_stmt(&mut i, t)?);
    }
    Ok(res)
}

fn parse_stmt(i: &mut usize, t: &[Token]) -> Result<Stmt, ParseError> {
    alt(i,t,&[
        &|i,t| { // NODE
            symbol(i,t,"node")?;
            let name = identifier(i,t)?;
            let shape = parse_shape(i,t)?;
            let pos = number(i,t)?;
            Ok(Stmt::Node(name,shape,pos))
        },
        &|i,t| { // EDGE
            symbol(i,t,"edge")?;
            let name1 = identifier(i,t)?;
            must_match(i,t,Token::Sep)?;
            let port1 = parse_port(i,t)?;
            let name2 = identifier(i,t)?;
            must_match(i,t,Token::Sep)?;
            let port2 = parse_port(i,t)?;
            Ok(Stmt::Edge((name1,port1),(name2,port2)))
        },
    ])
}

fn parse_shape(i :&mut usize, t: &[Token]) -> Result<Shape, ParseError> {
    alt(i,t,&[
        &|i,t| symbol(i,t,"begin").map(|_| Shape::Begin),
        &|i,t| symbol(i,t,"end").map(|_| Shape::End),
        &|i,t| symbol(i,t,"vertical").map(|_| Shape::Vertical),
        &|i,t| symbol(i,t,"outleftsw").map(|_|  Shape::Switch(Side::Left, Dir::Up    )),
        &|i,t| symbol(i,t,"outrightsw").map(|_| Shape::Switch(Side::Right, Dir::Up   )),
        &|i,t| symbol(i,t,"inleftsw").map(|_|   Shape::Switch(Side::Left, Dir::Down  )),
        &|i,t| symbol(i,t,"inrightsw").map(|_|  Shape::Switch(Side::Right, Dir::Down )),
    ])
}

fn parse_port(i :&mut usize, t :&[Token]) -> Result<Port, ParseError> {
    alt(i,t,&[
        &|i,t| symbol(i,t,"out").map(|_| Port::Out),
        &|i,t| symbol(i,t,"in").map(|_| Port::In),
        &|i,t| symbol(i,t,"trunk").map(|_| Port::Trunk),
        &|i,t| symbol(i,t,"left").map(|_| Port::Left),
        &|i,t| symbol(i,t,"right").map(|_| Port::Right),
        &|i,t| symbol(i,t,"top").map(|_| Port::Top),
        &|i,t| symbol(i,t,"bottom").map(|_| Port::Bottom),
        &|i,t| symbol(i,t,"topbottom").map(|_| Port::TopBottom),
    ])
}


fn lexer(x: &mut Iterator<Item = char>) -> Result<Vec<Token>, LexerError> {
    let mut tokens = Vec::new();
    let mut input = x.peekable();
    let mut line = 0;
    while let Some(&ch) = input.peek() {
        match ch {
            x if x.is_numeric() => {
                let num :String = consume_while(&mut input, |a| {
                    a.is_numeric() || a == '-' || a == 'e' || a == 'E' || a == '.'
                }).into_iter().collect();
                tokens.push(Token::Number(num.parse::<f64>().unwrap()));
            }
            x if x.is_alphabetic() => {
                let s : String = consume_while(&mut input, |a| a.is_alphanumeric())
                    .into_iter().collect();
                tokens.push(Token::Id(s));
            }
            '.' => {
                input.next().unwrap();
                tokens.push(Token::Sep);
            }
            '/' => {
                input.next().unwrap();
                if let Some('/') = input.next() {
                    consume_while(&mut input, |a| a != '\n');
                } else {
                    println!("Unexpected char after \"{}\"", '/');
                    return Err(LexerError::UnexpectedChar {
                        i: line,
                        c: '/'.to_string(),
                    });
                }
            }
            ' ' | '\r' | '\t' => {
                input.next().unwrap();
            }
            '\n' => {
                input.next().unwrap();
                line += 1;
            }
            c => {
                println!("Unexpected char \"{}\"", c.escape_debug());
                return Err(LexerError::UnexpectedChar {
                    i: line,
                    c: c.to_string(),
                });
            }
        }
    }

    tokens.push(Token::EOF);
    Ok(tokens)
}

fn symbol(i: &mut usize, t: &[Token], s: &str) -> Result<(), ParseError> {
    if identifier(i, t)? != s {
        Err(ParseError::UnexpectedToken(*i, format!("{:?}", s)))
    } else {
        Ok(())
    }
}

fn identifier(i: &mut usize, tokens: &[Token]) -> Result<String, ParseError> {
    let r = match tokens[*i] {
        Token::Id(ref s) => s.clone(),
        ref x => return Err(ParseError::UnexpectedToken(*i, format!("{:?}", x.clone()))),
    };
    *i += 1;
    Ok(r)
}

fn number(i: &mut usize, tokens: &[Token]) -> Result<f64, ParseError> {
    let r = match tokens[*i] {
        Token::Number(x) => x,
        ref x => return Err(ParseError::UnexpectedToken(*i, format!("{:?}", x))),
    };
    *i += 1;
    Ok(r)
}
