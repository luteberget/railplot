use railplotlib::model::{Shape};
use crate::convert_lua::{polyline_from_lua, point_from_lua};
use crate::convert_lua::{shape_from_string};
use rlua;
use log::trace;

type Pt = (f64,f64);

fn angle((x,y):Pt) -> f64 { y.atan2(x) }
fn rad2deg(x :f64) -> f64 { x*180.0/::std::f64::consts::PI }

pub fn drawing_size<'l>(ctx :rlua::Context<'l>, args:rlua::Table<'l>) -> Result<rlua::Value<'l>,rlua::Error> {
    let mut w :f64 = -1.0;
    let mut h :f64 = -1.0;
    let model = args.get::<_,rlua::Table>("data")?;

    let nodes = model.get::<_,rlua::Table>("nodes")?;
    for n in nodes.sequence_values() {
        let n :rlua::Table = n?;
        let x :f64 = n.get("x")?;
        let y :f64 = n.get("y")?;
        w = w.max(x);
        h = h.max(y);
    }

    let edges = model.get::<_,rlua::Table>("edges")?;
    for e in edges.sequence_values() {
        let t :rlua::Table = e?;
        let line :rlua::Table = t.get("line")?;
        let line = polyline_from_lua(line)?;
        for (x,y) in line {
            w = w.max(x);
            h = h.max(y);
        }
    }

    Ok(rlua::Value::Table(ctx.create_sequence_from(vec![w,h])?))
}

pub fn svg_document<'l>(ctx :rlua::Context<'l>, args:rlua::Table<'l>) -> Result<rlua::Value<'l>,rlua::Error> {
    let size :rlua::Table= args.get("size")?;
    let w :f64 = size.get(1)?;
    let h :f64 = size.get(2)?;
    let scale :f64 = args.get("scale")?;
    let contents :String = args.get("contents")?;
    let mut out = String::new();

    out.push_str(&format!("<svg width=\"{}\" height=\"{}\" 
              viewBox=\"{} {} {} {}\" 
              xmlns=\"http://www.w3.org/2000/svg\">\n", 
          (scale*(w+2.0)) as i64,(scale*(h+2.0)) as i64, 
          -(scale as i64),-(scale as i64),
          ((w+2.0)*scale) as i64, ((h+2.0)*scale) as i64 ));

    out.push_str(&contents);
    out.push_str(&format!("</svg>\n"));

    Ok(rlua::Value::String(ctx.create_string(&out)?))
}

/// Return a string containing svg draw commands
/// for tracks.
pub fn svg_tracks<'l>(ctx :rlua::Context<'l>, args:rlua::Table<'l>) -> Result<rlua::Value<'l>,rlua::Error> {
    let size :rlua::Table= args.get("size")?;
    let _w :f64 = size.get(1)?;
    let h :f64 = size.get(2)?;
    let scale :f64 = args.get("scale")?;

    let cx = |x| scale*x;
    let cy = |y| scale*(h-y);

    let mut out = String::new();
    let model = args.get::<_,rlua::Table>("data")?;
    let style :String = args.get("style").unwrap_or_else(|_| format!(""));
    let edges = model.get::<_,rlua::Table>("edges")?;
    for e in edges.sequence_values() {
        let t :rlua::Table = e?;
        let line :rlua::Table = t.get("line")?;
        let mut line = polyline_from_lua(line)?;

        let mut path = String::new();
        let (x,y) = line.remove(0);
        path.push_str(&format!("M {} {}", cx(x),cy(y)));
        for (x,y) in line {
            path.push_str(&format!("L {} {}", cx(x),cy(y)));
        }
        out.push_str(&format!("<path {} d=\"{}\" />\n", style, path));

    }
    Ok(rlua::Value::String(ctx.create_string(&out)?))
}

pub fn svg_switches<'l>(ctx :rlua::Context<'l>, args:rlua::Table<'l>) -> Result<rlua::Value<'l>,rlua::Error> {
    let size :rlua::Table= args.get("size")?;
    let _w :f64 = size.get(1)?;
    let h :f64 = size.get(2)?;
    let scale :f64 = args.get("scale")?;

    let cx = |x| scale*x;
    let cy = |y| scale*(h-y);

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

            let mut path = String::new();
            path.push_str(&format!("M {} {}", cx(x),cy(y)));
            path.push_str(&format!("L {} {}", 
                                   cx(x+ut1.0*length),
                                   cy(y+ut1.1*length)));

            path.push_str(&format!("L {} {}", 
                                   cx(x+ut2.0*length),
                                   cy(y+ut2.1*length)));
            out.push_str(&format!("<path {} d=\"{}\" />\n", style, path));
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

pub fn svg_symbols<'l>(ctx :rlua::Context<'l>, args:rlua::Table<'l>) -> Result<rlua::Value<'l>,rlua::Error> {
    let mut out = String::new();
    let model = args.get::<_,rlua::Table>("data")?;
    let symbols = model.get::<_,rlua::Table>("symbols")?;
    let draw_func :rlua::Function = args.get("draw")?;

    for s in symbols.sequence_values() {
        let s :rlua::Table = s?;
        let pt  = point_from_lua(s.get("point")?)?;
        let tan  = point_from_lua(s.get("tangent")?)?;
        let _deg = rad2deg(angle(tan));
        trace!("svg symbols point/tangent {:?} {:?}", pt, tan);
        let draw :String = draw_func.call(s)?;
        // TODO translate coordinate system
        out.push_str(&draw);
    }

    Ok(rlua::Value::String(ctx.create_string(&out)?))
}
