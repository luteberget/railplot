use super::schematic_graph::*;
use std::collections::HashMap;
use ordered_float::OrderedFloat;
use crate::levelssat;
use crate::edgeorder;

pub enum Goal {
    Width,Height,Bends
}

pub trait SchematicSolver {
    fn solve<Obj>(self, model :SchematicGraph<Obj>) -> Result<SchematicOutput<Obj>, String>;
}

pub struct SchematicOutput<Obj> {
    pub lines :Vec<(Edge<Obj>,Vec<(f64,f64)>)>,
    pub symbosl :Vec<f64>,
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
            ((node_names[&e.a.0],e.a.1),
            (node_names[&e.b.0],e.b.1))
        }).collect::<Vec<_>>();

        let mut edges_lt = edgeorder::edgeorder(&model.nodes, &edges);
        edgeorder::trans_red(&mut edges_lt);
        let edges_lt : Vec<(usize,usize)> = edges_lt.into_iter().collect();

        let nodes = model.nodes.iter().map(|n| n.shape.clone()).collect::<Vec<_>>();
        let levelssat::Output { node_coords, edge_levels, symbol_xs } = 
            levelssat::solve(&nodes, &edges, &symbols, &edges_lt)?;

        println!("c {:?}", node_coords);
        println!("l {:?}", edge_levels);
        println!("s {:?}", symbol_xs);
        unimplemented!()
    }
}


pub fn output_to_lua(model :SchematicOutput<rlua::Value>) -> Result<rlua::Value,rlua::Error> {
    unimplemented!()
}
