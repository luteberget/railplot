use super::schematic_graph::*;
use std::collections::HashMap;
use ordered_float::OrderedFloat;
use crate::levelssat;
use crate::edgeorder;

#[allow(unused)]
pub enum Goal {
    Width,Height,Bends,Diagonals,Nodeshapes,Shortedges,
}

type Pt = (f64,f64);

pub trait SchematicSolver {
    fn solve<Obj:Clone>(self, model :SchematicGraph<Obj>) -> Result<SchematicOutput<Obj>, String>;
}

pub struct SchematicOutput<Obj> {
    pub nodes :Vec<(Node, Pt)>, // node coords
    pub lines :Vec<(Edge<Obj>,Vec<Pt>)>, // edge lines
    pub symbols :Vec<(Obj,(Pt,Pt))>, // origin pt and rotation
}


pub struct LevelsSatSolver{
    pub criteria: Vec<Goal>,
}

impl SchematicSolver for LevelsSatSolver {
    fn solve<Obj:Clone>(self, mut model:SchematicGraph<Obj>) -> Result<SchematicOutput<Obj>, String> {
        // Convert 
        model.nodes.sort_by_key(|n| OrderedFloat(n.pos));
        let node_names = model.nodes.iter().enumerate()
            .map(|(i,n)| (n.name.clone(), i)).collect::<HashMap<_,_>>();
        model.edges.sort_by_key(|e| (node_names[&e.a.0], node_names[&e.b.0]));

        let symbols_ref = model.edges.iter().enumerate().flat_map(|(i,e)| {
            e.objects.iter().map(move |(s,objref)| (i,s,objref))
        }).collect::<Vec<_>>();

        let symbols = symbols_ref.iter().map(|(i,s,_)| (*i,*s))
            .collect::<Vec<_>>();

        let edges = model.edges.iter().map(|e| {
            levelssat::Edge {
                a: levelssat::NodePort { node: node_names[&e.a.0], port: e.a.1 },
                b: levelssat::NodePort { node: node_names[&e.b.0], port: e.b.1 },
            }
        }).collect::<Vec<_>>();

        let edges2 = model.edges.iter().map(|e| {
            ((node_names[&e.a.0],e.a.1),
            (node_names[&e.b.0],e.b.1))
        }).collect::<Vec<_>>();

        let mut edges_lt = edgeorder::edgeorder(&model.nodes, &edges2);
        edgeorder::transitive_reduction(&mut edges_lt);
        let edges_lt : Vec<(usize,usize)> = edges_lt.into_iter().collect();

        let nodes = model.nodes.iter().map(|n| levelssat::Node { shape: n.shape.clone(),
         pos: n.pos }).collect::<Vec<_>>();
        let levelssat::Output { node_coords, edge_levels, symbol_xs } = 
            levelssat::solve(&nodes, &edges, &symbols, &edges_lt, &self.criteria)?;

        println!("c {:?}", node_coords);
        println!("l {:?}", edge_levels);
        println!("s {:?}", symbol_xs);

        let edge_lines = convert_edge_levels(
            &edges2.iter().map(|((a,_),(b,_))| (*a,*b)).collect::<Vec<_>>(),
            &node_coords, &edge_levels);

        let symbol_coords = convert_symbol_coords(&symbols, &symbol_xs, &edge_lines);
        let symbol_out = model.edges.iter()
            .flat_map(|e| e.objects.iter()).map(|(_,objref)| objref.clone())
            .zip(symbol_coords.into_iter()).collect();

        // the given output should be 
        // (1) line segments for edges
        // (2) coords for nodes?
        // (3) coords for symbols (with y coord) and rotation
        // (4) switches with tangents? TODO
        // (5) something about end-nodes? TODO
        Ok(SchematicOutput {
            nodes: model.nodes.into_iter().zip(node_coords.into_iter()).collect(),
            lines: model.edges.into_iter().zip(edge_lines.into_iter()).collect(),
            symbols: symbol_out,
        })

    }
}

fn convert_symbol_coords(symbols :&[(usize,&Symbol)], symbol_xs :&[f64], edge_lines :&[Vec<(f64,f64)>]) -> Vec<(Pt,Pt)> {
    fn lerp2((x0,y0) :Pt, (x1,y1) :Pt, s :f64) -> Pt {
        (x0 + s * (x1 - x0),
         y0 + s * (y1 - y0))
    }

    fn line_pt_at_x(l :&[Pt], x: f64) -> Result<Pt,()> {
        l.binary_search_by_key(&OrderedFloat(x), |(x0,_y0)| OrderedFloat(*x0))
         .map(|i| l[i]).or_else(|i| {
             if i == 0 || i == l.len() { Err(()) }
             else { Ok(lerp2(l[i-1],l[i], (x - l[i-1].0) / (l[i].0 - l[i-1].0))) }
         })
    }

    fn rot90((x,y) :Pt) -> Pt { (-y,x) }
    fn addpt((x0,y0):Pt,(x1,y1):Pt) -> Pt { (x0+x1,y0+y1) }
    fn scale(s:f64, (x,y):Pt) -> Pt { (s*x,s*y) }

    fn line_tangent_at_x(l :&[Pt], x :f64) -> Result<Pt,()> {
        l.binary_search_by_key(&OrderedFloat(x), |(x0,_y0)| OrderedFloat(*x0))
            .map(|i| l[i]).or_else(|i| {
                if i == 0 || i == l.len() { Err(()) }
                else {
                    let (dx,dy) = (l[i].0-l[i-1].0, l[i].1-l[i-1].1);
                    let len = (dx*dx+dy*dy).sqrt();
                    Ok((dx/len,dy/len))
                }})
    }

    let mut output = Vec::new();
    for (i,(ei,s)) in symbols.iter().enumerate() {
        let line_pt = line_pt_at_x(&edge_lines[*ei], symbol_xs[i] as f64).unwrap();
        let line_tangent  = line_tangent_at_x(&edge_lines[*ei], symbol_xs[i] as f64).unwrap();
        let pt = addpt(line_pt, scale(0.25*(s.level as f64),rot90(line_tangent)));

        output.push((pt,line_tangent));
    }

    output

}

fn convert_edge_levels(edges :&[(usize,usize)], node_coords :&[(f64,f64)], edge_levels :&[f64]) -> Vec<Vec<(f64,f64)>> {
    edges.iter().enumerate().map(|(i,(a, b))| {
        let (x1,y1) = node_coords[*a];
        let (x2,y2) = node_coords[*b];
        let l = edge_levels[i];
        conv_line((x1,y1),l,(x2,y2))
    }).collect()
}

pub fn conv_line((x1,y1) :(f64,f64), l :f64, (x2,y2) :(f64,f64)) -> Vec<(f64,f64)> {
    let dx1 = (y1-l).abs();
    let dx2 = (y2-l).abs();

    let mut line = Vec::new();

    let p1 = (x1,y1);
    let p2 = (x1+dx1, l);
    let p3 = (x2-dx2, l);
    let p4 = (x2,y2);

    line.push(p1);
    if (p2.0-p1.0)*(p2.0-p1.0) +
       (p2.1-p1.1)*(p2.1-p1.1) > 1e-5 { line.push(p2); }
    if (p3.0-p2.0)*(p3.0-p2.0) +
       (p3.1-p2.1)*(p3.1-p2.1) > 1e-5 { line.push(p3); }
    if (p4.0-p3.0)*(p4.0-p3.0) +
       (p4.1-p3.1)*(p4.1-p3.1) > 1e-5 { line.push(p4); }

    line
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
