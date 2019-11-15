
/// Data types for representing schematic graph
/// as strongly-typed Rust, and conversion to/from Lua

#[derive(Debug, Copy, Clone)]
pub enum Dir { Up=0, Down=1 }

impl Dir {
    pub fn opposite(&self) -> Dir {
        match self {
            Dir::Up => Dir::Down,
            Dir::Down => Dir::Up,
        }
    }
}

#[derive(Debug, Copy, Clone)]
pub enum Side { Left=0, Right=1 }

#[derive(Debug, Copy, Clone, Hash, PartialEq, Eq)]
#[allow(dead_code)]
pub enum Port { In=0, Out=1, Left=2, Right=3, Trunk=4 }

#[derive(Debug, Copy, Clone)]
#[allow(dead_code)]
pub enum Shape { Begin, End, Switch(Side, Dir) }

#[derive(Debug, Clone)]
pub struct Node {
    pub name :String,
    pub pos :f64,
    pub shape :Shape,
}

#[derive(Debug, Clone )]
pub struct Edge<Obj> {
    pub a :(String,Port),
    pub b :(String,Port),
    pub objects :Vec<(Symbol,Obj)>,
}

#[derive(Debug, Copy, Clone)]
pub struct Symbol {
    pub pos :f64,
    pub width :f64,
    pub origin: f64,
    pub level: isize,
}

#[derive(Debug)]
pub struct SchematicGraph<Obj> {
    pub nodes :Vec<Node>,
    pub edges :Vec<Edge<Obj>>,
}

