#[macro_use]
extern crate clap;
extern crate failure;
#[macro_use] extern crate failure_derive;
extern crate z3;
mod parser_utils;
mod parser;
mod solver;

fn main() {
    use clap::{Arg};
    let matches = clap::App::new("vis-rs")
        .about("Convert railway graph representation (vis-rs format) to screen coordinates")
        .version(crate_version!())
        .author(crate_authors!())
        .arg(Arg::with_name("INPUT")
             .help("File to use (vis-rs format)")
             .required(true).index(1))
        .arg(Arg::with_name("v")
             .short("v")
             .multiple(true)
             .help("Verbose output"))
        .get_matches();

    let filename = matches.value_of("INPUT").expect("No input file specified");
    let verbose = matches.occurrences_of("v") > 0;
    let debug = matches.occurrences_of("v") > 1;

    if verbose { println!("Parsing \"{}\".",filename); }
    let stmts = parser::read_file(std::path::Path::new(filename)).expect("Parser failed");
    if debug { println!("Parsed: {:?}", stmts); }

    if verbose { println!("Converting {} statements.", stmts.len()); }
    let solver_input = solver::convert(stmts).expect("Conversion failed");
    if debug { println!("Converted: {:?}", solver_input); }
    if verbose { println!("Converted {} nodes and {} edges.", 
                          solver_input.nodes.len(), solver_input.edges.len()); }

    if verbose { println!("Solving."); }
    let output = solver::solve(solver_input).expect("Solver failed");
    if verbose { 
        use std::f64;
        let width = output.node_coords.iter().map(|(_,x,_)| *x).fold(-1./0., f64::max);
        let height = output.node_coords.iter().map(|(_,_,y)| *y)
            .chain(  output.edge_levels.iter().map(|(_,_,y)| *y))
            .max().unwrap() as f64;
        println!("Finished solving, size is {} x {}.", width, height);
    }
    if debug {
        println!("solution {:?}", output);
    }

    if verbose { println!("Finished."); }
}

