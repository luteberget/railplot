use super::schematic_graph::*;

pub mod levelssat {
    pub struct Solver{}
}


pub trait SchematicSolver {
    fn solve<Obj>(model :SchematicGraph<Obj>) -> Result<SchematicOutput<Obj>, String>;
}

pub struct SchematicOutput<Obj> {
    pub lines :Vec<(Edge<Obj>,Vec<(f64,f64)>)>,
    pub symbosl :Vec<f64>,
}

