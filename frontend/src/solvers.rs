use super::schematic_graph::*;
use std::collections::HashMap;
use ordered_float::OrderedFloat;
use crate::levelssat;
use crate::edgeorder;

#[allow(unused)]
pub enum Goal {
    Width,Height,Bends,Diagonals,Nodeshapes,Shortedges,
}

pub trait SchematicSolver {
    fn solve<Obj>(self, model :SchematicGraph<Obj>) -> Result<SchematicOutput<Obj>, String>;
}

pub struct SchematicOutput<Obj> {
    pub nodes :Vec<(Node, (f64,f64))>, // node coords
    pub lines :Vec<(Edge<Obj>,Vec<(f64,f64)>)>, // edge lines
    pub symbols :Vec<(Obj,(f64,f64),f64)>, // origin pt and rotation
}


pub struct LevelsSatSolver{
    pub criteria: Vec<Goal>,
}

impl SchematicSolver for LevelsSatSolver {
    fn solve<Obj>(self, mut model:SchematicGraph<Obj>) -> Result<SchematicOutput<Obj>, String> {
        // Convert 
        model.nodes.sort_by_key(|n| OrderedFloat(n.pos));
        let node_names = model.nodes.iter().enumerate()
            .map(|(i,n)| (n.name.clone(), i)).collect::<HashMap<_,_>>();
        model.edges.sort_by_key(|e| (node_names[&e.a.0], node_names[&e.b.0]));

        let symbols = model.edges.iter().enumerate().flat_map(|(i,e)| {
            e.objects.iter().map(move |(s,_objref)| (i,s))
        }).collect::<Vec<_>>();

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
        let edge_lines = convert_edge_levels(&edges, &node_coords, &edge_levels)?;
        let symbol_coords = unimplemented!();
        Ok(SchematicOutput {
            nodes: model.nodes.into_iter().zip(node_coords.into_iter()).collect(),
            lines: edge_lines,
            symbols: symbol_coords,
        })
            // the given output should be 
            // (1) line segments for edges
            // (2) coords for nodes?
            // (3) coords for symbols (with y coord) and rotation
            // (4) switches with tangents? TODO
            // (5) something about end-nodes? TODO
    }
}

fn convert_edge_levels(edges :&[Edge], node_coords :&[(f64,f64)], edge_levels :&[f64]) -> Result<Vec<Vec<(f64,f64)>>, String> {
}


pub fn output_to_lua(_model :SchematicOutput<rlua::Value>) -> Result<rlua::Value,rlua::Error> {
    unimplemented!()
}
