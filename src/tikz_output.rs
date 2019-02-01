use ordered_float::OrderedFloat;
use railplotlib::model::{Shape};
use crate::convert_lua::{polyline_from_lua, point_from_lua};
use crate::convert_lua::{shape_from_string};
use rlua;
use log::trace;

type Pt = (f64,f64);

/// TikZ coordinate string "(x,y)"
fn coords(x:f64,y:f64) -> String { format!("({},{})",x,y) }
fn angle((x,y):Pt) -> f64 { y.atan2(x) }
fn rad2deg(x :f64) -> f64 { x*180.0/::std::f64::consts::PI }

/// Return a string containing TikZ draw commands
/// for tracks.
pub fn tikz_tracks<'l>(ctx :rlua::Context<'l>, args:rlua::Table<'l>) -> Result<rlua::Value<'l>,rlua::Error> {
    use itertools::Itertools;

    let mut out = String::new();
    let model = args.get::<_,rlua::Table>("data")?;
    let style :String = args.get("style").unwrap_or_else(|_| format!(""));
    let title :Option<String> = args.get("title")?;
    let edges = model.get::<_,rlua::Table>("edges")?;
    let (mut xmax, mut ymax) = (OrderedFloat(0.0f64),OrderedFloat(0.0f64));
    for e in edges.sequence_values() {
        let t :rlua::Table = e?;
        let line :rlua::Table = t.get("line")?;
        let line = polyline_from_lua(line)?;
        out.push_str(&format!("\\draw[{}] {};\n", style,
                  line.iter().map(|(x,y)| coords(*x,*y)).join(" -- ")));

        xmax = xmax.max(line.iter().map(|(x,_)| OrderedFloat(*x)).max().unwrap_or(0.0.into()));
        ymax = ymax.max(line.iter().map(|(_,y)| OrderedFloat(*y)).max().unwrap_or(0.0.into()));

    }

    if let Some(t) = title {
        out.push_str(&format!("\\node[align=center,anchor=south] at ({},{}) {{{}}};",
        xmax.into_inner()/2.0,ymax.into_inner()+0.5,t));
    }

    Ok(rlua::Value::String(ctx.create_string(&out)?))
}

pub fn tikz_switches<'l>(ctx :rlua::Context<'l>, args:rlua::Table<'l>) -> Result<rlua::Value<'l>,rlua::Error> {
    let mut out = String::new();
    let model = args.get::<_,rlua::Table>("data")?;
    let nodes = model.get::<_,rlua::Table>("nodes")?;
    let edges = model.get::<_,rlua::Table>("edges")?;
    let style :String = args.get("style").unwrap_or_else(|_| format!(""));
    let length :f64 = args.get("length").unwrap_or_else(|_| 0.25);

    for n in nodes.sequence_values() {
        let n :rlua::Table = n?;
        let name :String = n.get("name")?;
        let shape :String = n.get("shape")?;
        if let Shape::Switch(_,_) = shape_from_string(shape.clone()) {
            let left  = get_edge(edges.clone(), &name, "left")?;
            let right = get_edge(edges.clone(), &name, "right")?;

            let (x,y) :(f64,f64) = (n.get("x")?,n.get("y")?);

            let line1 :rlua::Table = left.get("line")?;
            let line1 = polyline_from_lua(line1)?;
            let line2 :rlua::Table = right.get("line")?;
            let line2 = polyline_from_lua(line2)?;

            let (v1,v2) = if shape.starts_with("out") {
                (line1[1],line2[1])
            } else {
                (*line1.iter().rev().nth(1).unwrap(),
                 *line2.iter().rev().nth(1).unwrap())
            };


            let t1 = (v1.0 - x, v1.1 - y);
            let t2 = (v2.0 - x, v2.1 - y);
            let ut1 = (t1.0 / t1.0.abs(), t1.1 / t1.0.abs());
            let ut2 = (t2.0 / t2.0.abs(), t2.1 / t2.0.abs());
            out.push_str(&format!("\\fill[{}] {} -- {} -- {};\n", style,
                                 coords(x,y),
                                 coords(x+ut1.0*length, y+ut1.1*length),
                                 coords(x+ut2.0*length, y+ut2.1*length)));
        }
    }
    Ok(rlua::Value::String(ctx.create_string(&out)?))
}

fn get_edge<'l>(t :rlua::Table<'l>, n :&str, p :&str) -> Result<rlua::Table<'l>, rlua::Error> {

    for v in t.sequence_values() {
        let edge :rlua::Table = v?;
        let node_a = edge.get::<_,String>("node_a")?;
        let node_b = edge.get::<_,String>("node_b")?;
        let port_a = edge.get::<_,String>("port_a")?;
        let port_b = edge.get::<_,String>("port_b")?;
        if (node_a == n && port_a == p) || (node_b == n && port_b == p) {
            return Ok(edge);
        }
    }
    use rlua::ExternalError;
    Err(format!("Edge not found").to_lua_err())
}

pub fn tikz_symbols<'l>(ctx :rlua::Context<'l>, args:rlua::Table<'l>) -> Result<rlua::Value<'l>,rlua::Error> {
    let mut out = String::new();
    let model = args.get::<_,rlua::Table>("data")?;
    let symbols = model.get::<_,rlua::Table>("symbols")?;
    let draw_func :rlua::Function = args.get("draw")?;

    for s in symbols.sequence_values() {
        let s :rlua::Table = s?;
        let pt  = point_from_lua(s.get("point")?)?;
        let tan  = point_from_lua(s.get("tangent")?)?;
        let deg = rad2deg(angle(tan));
        trace!("TikZ symbols point/tangent {:?} {:?}", pt, tan);
        out.push_str(&format!("\\begin{{scope}}[shift={{{}}},rotate={}]\n", 
             coords(pt.0,pt.1), deg));
        let draw :String = draw_func.call(s)?;
        out.push_str(&draw);
        out.push_str("\n");
        out.push_str(&format!("\\end{{scope}}\n"));
    }

    Ok(rlua::Value::String(ctx.create_string(&out)?))
}
