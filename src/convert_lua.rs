use railplotlib::model::*;
use railplotlib::solvers::*;
use log::{info};

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

pub fn shape_to_string(s :Shape) -> &'static str {
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

pub fn shape_from_string(s :String) -> Shape {
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

pub fn edge_to_lua<'l>(ctx :rlua::Context<'l>, e :Edge<rlua::Value<'l>>) -> Result<rlua::Table<'l>,rlua::Error> {
    let edge_lua :rlua::Table = ctx.create_table()?;
    edge_lua.set("node_a", e.a.0)?;
    edge_lua.set("node_b", e.b.0)?;
    edge_lua.set("port_a", port_to_string(e.a.1))?;
    edge_lua.set("port_b", port_to_string(e.b.1))?;
    let os = e.objects.into_iter().map(|(s,o)| {
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
}

pub fn node_to_lua<'l>(ctx :rlua::Context<'l>, n :Node) -> Result<rlua::Table<'l>,rlua::Error> {
    let node_lua :rlua::Table = ctx.create_table()?;
    node_lua.set("name",n.name)?;
    node_lua.set("pos",n.pos)?;
    node_lua.set("shape",shape_to_string(n.shape))?;
    Ok(node_lua)
}

pub fn schematic_graph_to_lua<'l>(ctx :rlua::Context<'l>, schematic :SchematicGraph<rlua::Value<'l>>) -> Result<rlua::Value<'l>, rlua::Error> {
    let node_tbls :Vec<rlua::Table> = schematic.nodes.into_iter().map(|n| {
        node_to_lua(ctx,n)
    }).collect::<Result<Vec<_>,_>>()?;

    let edge_tbls :Vec<rlua::Table> = schematic.edges.into_iter().map(|e| {
        edge_to_lua(ctx,e)
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
        let objects = symbols_tbl.sequence_values::<rlua::Table>().map(|s| {
            let o = s?;
            use rlua::ExternalResult;
            let symbol_info = symbol_from_lua(&o.get::<_,rlua::Table>("_symbol_info")?).to_lua_err()?;
            Ok((symbol_info, rlua::Value::Table(o)))
        }).collect::<Result<Vec<_>,_>>()?;
        model.edges.push(Edge { a: (node_a, port_a), b: (node_b, port_b), objects });
    }

    info!("Converted schematic graph to Lua.");
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


pub fn output_to_lua<'l>(ctx :rlua::Context<'l>, model :SchematicOutput<rlua::Value<'l>>) -> Result<rlua::Value<'l>,rlua::Error> {

    // convert nodes

    let node_tbls :Vec<rlua::Table> = model.nodes.into_iter().map(|(n,(x,y))| {
        let t = node_to_lua(ctx, n)?;
        t.set("x",x)?;
        t.set("y",y)?;
        Ok(t)
    }).collect::<Result<Vec<_>,_>>()?;


    // convert edges
    let edge_tbls :Vec<rlua::Table> = model.lines.into_iter().map(|(e,ls)| {
        let t = edge_to_lua(ctx,e)?;
        t.set("line", polyline_to_lua(ctx, ls)?)?;
        Ok(t)
    }).collect::<Result<Vec<_>,_>>()?;

    // convert symbols
    let symbol_tbls :Vec<rlua::Table> = model.symbols.into_iter().map(|(obj,(pt,tan))| {
        match obj {
            rlua::Value::Table(t) => {
                t.set("point", ctx.create_sequence_from(vec![pt.0,pt.1])?)?;
                t.set("tangent", ctx.create_sequence_from(vec![tan.0,tan.1])?)?;
                Ok(t)
            },
            _ => panic!("Symbol must be Lua table type."),
        }
    }).collect::<Result<Vec<_>,_>>()?;


    let lua_model = ctx.create_table()?;
    lua_model.set("nodes",node_tbls)?;
    lua_model.set("edges",edge_tbls)?;
    lua_model.set("symbols",symbol_tbls)?;
    Ok(rlua::Value::Table(lua_model))
}

pub fn point_to_lua<'l>(ctx :rlua::Context<'l>, (x,y) :(f64,f64)) -> Result<rlua::Table<'l>, rlua::Error> {
    ctx.create_sequence_from(vec![x,y])
}

pub fn point_from_lua(t :rlua::Table) -> Result<(f64,f64), rlua::Error> {
    let x = t.sequence_values().collect::<Result<Vec<f64>,_>>()?;
    Ok((x[0],x[1]))
}

pub fn polyline_to_lua<'l>(ctx :rlua::Context<'l>, ls :Vec<(f64,f64)>) -> Result<rlua::Table<'l>,rlua::Error> {
    ctx.create_sequence_from(ls.into_iter().map(|pt| point_to_lua(ctx,pt))
            .collect::<Result<Vec<_>,_>>()?)
}

pub fn polyline_from_lua<'l>(ls :rlua::Table<'l>) -> Result<Vec<(f64,f64)>,rlua::Error> {
    ls.sequence_values().map(|pt| point_from_lua(pt?)).collect::<Result<Vec<_>,_>>()
}
