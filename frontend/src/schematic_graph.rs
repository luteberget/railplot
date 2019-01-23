/// Data types for representing schematic graph
/// as strongly-typed Rust, and conversion to/from Lua

#[derive(Debug, Copy, Clone)]
pub enum Dir { Up, Down }

impl Dir {
    pub fn opposite(&self) -> Dir {
        match self {
            Dir::Up => Dir::Down,
            Dir::Down => Dir::Up,
        }
    }
}

#[derive(Debug, Copy, Clone)]
pub enum Side { Left, Right }

#[derive(Debug, Copy, Clone, Hash, PartialEq, Eq)]
pub enum Port { In, Out, Left, Right, Trunk, Top, Bottom, TopBottom /* Unknown top/bottom */ }

#[derive(Debug)]
pub enum Shape { Begin, End, Switch(Side, Dir), Vertical, }

pub type Level = isize;

pub struct Node {
    pub name :String,
    pub pos :f64,
    pub shape :Shape,
}

pub struct Edge<Obj> {
    pub a :(String,Port),
    pub b :(String,Port),
    pub objs :Vec<(Symbol,Obj)>,
}

#[derive(Debug, Copy, Clone)]
pub struct Symbol {
    pub pos :f64,
    pub width :f64,
    pub origin: f64,
    pub level: isize,
}

pub struct SchematicGraph<Obj> {
    pub nodes :Vec<Node>,
    pub edges :Vec<Edge<Obj>>,
}


fn port_to_string(p :Port) -> &'static str {
    match p {
        Port::Out => "out",
        Port::In => "in",
        Port::Left => "left",
        Port::Right => "right",
        Port::Trunk => "trunk",
        _ => panic!(),
    }
}

fn port_from_string(s :String) -> Port {
    match s.as_str() {
        "out" => Port::Out,
        "in" => Port::In,
        "left" => Port::Left,
        "right" => Port::Right,
        "trunk" => Port::Trunk,
        _ => panic!(),
    }
}

fn shape_to_string(s :Shape) -> &'static str {
    match s {
        Shape::Begin => "begin",
        Shape::End => "end",
        Shape::Switch(Side::Left,Dir::Up) => "outleftsw",
        Shape::Switch(Side::Right,Dir::Up) => "outrightsw",
        Shape::Switch(Side::Left,Dir::Down) => "inleftsw",
        Shape::Switch(Side::Right,Dir::Down) => "inrightsw",
        _ => panic!(),
    }
}

fn shape_from_string(s :String) -> Shape {
    match s.as_str() {
        "begin" => Shape::Begin,
        "end" => Shape::End,
        "outleftsw" => Shape::Switch(Side::Left,Dir::Up),
        "outrightsw" => Shape::Switch(Side::Right,Dir::Up),
        "inleftsw" => Shape::Switch(Side::Left,Dir::Down),
        "inrightsw" => Shape::Switch(Side::Right,Dir::Down),
        _ => panic!(),
    }
}

pub fn schematic_graph_to_lua<'l>(ctx :rlua::Context<'l>, schematic :SchematicGraph<rlua::Value<'l>>) -> Result<rlua::Value<'l>, rlua::Error> {
    let node_tbls :Vec<rlua::Table> = schematic.nodes.into_iter().map(|n| {
        let node_lua :rlua::Table = ctx.create_table()?;
        node_lua.set("name",n.name)?;
        node_lua.set("pos",n.pos)?;
        node_lua.set("shape",shape_to_string(n.shape))?;
        Ok(node_lua)
    }).collect::<Result<Vec<_>,_>>()?;

    let edge_tbls :Vec<rlua::Table> = schematic.edges.into_iter().map(|e| {
        let edge_lua :rlua::Table = ctx.create_table()?;
        edge_lua.set("node_a", e.a.0)?;
        edge_lua.set("node_b", e.b.0)?;
        edge_lua.set("port_a", port_to_string(e.a.1))?;
        edge_lua.set("port_b", port_to_string(e.b.1))?;
        let os = e.objs.into_iter().map(|(s,o)| {
            match o.clone() {
                rlua::Value::Table(t) => {
                    t.set::<_,rlua::Value>("_symbol_info", symbol_to_lua(&ctx, &s)?)?;
                },
                _ => panic!(),
            }
            Ok(o)
        }).collect::<Result<Vec<_>,_>>()?;
        edge_lua.set("objects", ctx.create_sequence_from(os)?)?;
        Ok(edge_lua)
    }).collect::<Result<Vec<_>,_>>()?;

    let model = ctx.create_table()?;
    model.set("nodes",node_tbls)?;
    model.set("edges",edge_tbls)?;
    Ok(rlua::Value::Table(model))
}

pub fn schematic_graph_from_lua<'l>(schematic :&rlua::Table<'l>) -> Result<SchematicGraph<rlua::Value<'l>>, rlua::Error> {
    let mut model = SchematicGraph {
        nodes: Vec::new(),
        edges: Vec::new(),
    };

    let ns = schematic.get::<_,rlua::Table>("nodes")?;
    for node in ns.sequence_values::<rlua::Table>() {
        let node = node?;
        let name = node.get::<_,String>("name")?;
        let pos = node.get::<_,f64>("pos")?;
        let shape = shape_from_string(node.get::<_,String>("shape")?);
        model.nodes.push(Node { name, pos, shape });
    }

    let es = schematic.get::<_,rlua::Table>("edges")?;
    for edge in es.sequence_values::<rlua::Table>() {
        let edge = edge?;
        let node_a = edge.get::<_,String>("node_a")?;
        let node_b = edge.get::<_,String>("node_b")?;
        let port_a = port_from_string(edge.get::<_,String>("port_a")?);
        let port_b = port_from_string(edge.get::<_,String>("port_b")?);
        let symbols_tbl = edge.get::<_,rlua::Table>("objects")?;
        let objs = symbols_tbl.sequence_values::<rlua::Table>().map(|s| {
            let o = s?;
            use rlua::ExternalResult;
            let symbol_info = symbol_from_lua(&o.get::<_,rlua::Table>("_symbol_info")?).to_lua_err()?;
            Ok((symbol_info, rlua::Value::Table(o)))
        }).collect::<Result<Vec<_>,_>>()?;
        model.edges.push(Edge { a: (node_a, port_a), b: (node_b, port_b), objs: objs });
    }

    println!("converted model");
    Ok(model)
}

pub fn symbol_to_lua<'a>(ctx :&rlua::Context<'a>, s :&Symbol) -> Result<rlua::Value<'a>,rlua::Error> {
    let t = ctx.create_table()?;
    t.set("pos",s.pos)?;
    t.set("width",s.width)?;
    t.set("origin",s.origin)?;
    t.set("level",s.level)?;
    Ok(rlua::Value::Table(t))
}

pub fn symbol_from_lua(s :&rlua::Table) -> Result<Symbol,String> {
    let pos = s.get::<_,f64>("pos").map_err(|e| format!("Symbol.pos: {}",e))?;
    let width = s.get::<_,f64>("width").map_err(|e| format!("Symbol.width: {}",e))?;
    let origin = s.get::<_,f64>("origin").map_err(|e| format!("Symbol.origin: {}",e))?;
    let level = s.get::<_,isize>("level").map_err(|e| format!("Symbol.level: {}",e))?;
    Ok(Symbol { pos, width, origin, level })
}
