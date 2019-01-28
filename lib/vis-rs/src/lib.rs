#[macro_use]
extern crate serde_json;
extern crate failure;
#[macro_use] extern crate failure_derive;
extern crate z3;
extern crate rolling;
extern crate svg;
extern crate itertools;
extern crate diffsolver;
extern crate lp_modeler;
extern crate ordered_float;
extern crate disjoint_sets;

pub mod parser_utils;
pub mod parser;
pub mod solver;
pub mod convert;
pub mod json;
pub mod trans_red;
pub mod svg_output;
pub mod tikz_output;
pub mod convert_pos;
pub mod symbols;

use std::collections::HashMap;

#[derive(Debug)]
pub struct Schematic {
    pub result :solver::SolverOutput,
    pub original_edges :convert::OrigEdges,
    pub pos_range: convert::PosRange,
    pub node_names :HashMap<String,usize>,
    pub portref_changes :Vec<(solver::Edge,solver::Edge)>,
}

use rolling::input::staticinfrastructure::*;
pub fn convert_dgraph(inf :&StaticInfrastructure) -> Result<Schematic, String> {
    let (visgraph_str, original_edges, pos_range) = convert::convert(inf).map_err(|e| format!("{:?}", e))?;
    let stmts = parser::read_string(&visgraph_str).map_err(|e| format!("{:?}", e))?;
    let (solver_input, node_names) = solver::convert(stmts)?;
    let (result, portref_changes) = solver::solve_difftheory(solver_input)?;

    Ok(Schematic { result, original_edges, node_names, portref_changes, pos_range })
}


pub fn convert_javascript(s :Schematic) -> Result<(serde_json::Value,serde_json::Value),String> {
        use std::collections::HashMap;
        //let oe = orig_edges.ok_or(format!("Could not find original d-graph edge map."));
        //let nnames = node_names.ok_or(format!(("Could not find original node names.");

        let oe = s.original_edges;
        let nnames = s.node_names;
        let portref_changes = s.portref_changes;
        let output = s.result;

        let mut node_idxs = HashMap::new();
        for (k,v) in nnames.iter() {
                node_idxs.insert(*v,k.clone());
        }

        use parser::Port;
        type E = ((String,Port),(String,Port));
        let portref_tr : HashMap<E,E> =
            portref_changes.into_iter().map(|(old,new)| {
                let source = ((node_idxs[&old.a.node].clone(), old.a.port),
                              (node_idxs[&old.b.node].clone(), old.b.port));
                let target = ((node_idxs[&new.a.node].clone(), new.a.port),
                              (node_idxs[&new.b.node].clone(), new.b.port));
                (source,target)
            }).collect();


        use convert::OrigEdges;
        let oe :OrigEdges = oe.into_iter().map(|(k,v)| (portref_tr.get(&k).cloned().unwrap_or(k), v))
            .collect();

        use std::fs::File;
        use std::io::BufWriter;
        use std::io::Write;
        let (edge_lines,node_tangents) = json::lines(&output, &oe, &s.pos_range, &nnames).expect("Could not convert output to javascript format");
        //let json2 = json::pos(&oe, &s.pos_range).expect("Pos ranges did not match.");

        Ok((edge_lines, node_tangents))
}
