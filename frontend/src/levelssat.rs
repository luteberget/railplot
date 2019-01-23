use crate::schematic_graph::*;
use diffsolver::*;

type EdgeRef = usize;
type NodeRef = usize;
type PortRef = (NodeRef,Port);
type Edge = (PortRef, PortRef);
type EdgePair = (EdgeRef, EdgeRef);

pub type Pt = (f64,f64);
pub struct Output {
    pub node_coords: Vec<Pt>,
    pub edge_levels: Vec<f64>,
    pub symbol_xs: Vec<f64>,
}

pub fn solve(nodes :&[Shape], edges :&[Edge], symbols:&[(EdgeRef,&Symbol)], edges_lt :&[EdgePair]) -> Result<Output, String> {

    unimplemented!()
}
