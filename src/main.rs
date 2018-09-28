#[macro_use]
extern crate clap;
extern crate failure;
#[macro_use] extern crate failure_derive;
extern crate z3;
extern crate rolling;
mod parser_utils;
mod parser;
mod solver;
mod convert;

fn main() {
    use clap::{Arg};
    let matches = clap::App::new("vis-rs")
        .about("Convert railway graph representation (vis-rs format) to screen coordinates")
        .version(crate_version!())
        .author(crate_authors!())
        .arg(Arg::with_name("INPUT")
             .help("File to use (vis-rs format)")
             //.required(true)
             .index(1))
        .arg(Arg::with_name("infrastructure")
             .short("i")
             .long("infrastructure")
             .help("Convert rolling D-graph format to vis-rs format")
             .value_name("FILE")
             .takes_value(true))
        .arg(Arg::with_name("v")
             .short("v")
             .multiple(true)
             .help("Verbose output"))
        .get_matches();

    let filename = matches.value_of("INPUT");
    let verbose = matches.occurrences_of("v") > 0;
    let debug = matches.occurrences_of("v") > 1;
    let infrastructure_filename = matches.value_of("infrastructure");
    println!("Convert from infrastructure: {:?}", infrastructure_filename);

    let mut input = None;
    if let Some(filename) = filename {
        if verbose { println!("Parsing \"{}\".",filename); }
        let stmts = parser::read_file(std::path::Path::new(filename)).expect("Parser failed");
        if debug { println!("Parsed: {:?}", stmts); }

        if verbose { println!("Converting {} statements.", stmts.len()); }
        let solver_input = solver::convert(stmts).expect("Conversion failed");
        if debug { println!("Converted: {:?}", solver_input); }
        if verbose { println!("Converted {} nodes and {} edges.", 
                              solver_input.nodes.len(), solver_input.edges.len()); }
        input = Some(solver_input);
    } else {
        if let Some(filename) = infrastructure_filename {
            if verbose { println!("Converting d-graph \"{}\".",filename); }
            let c = convert::convert(std::path::Path::new(filename)).expect("D-graph conversion failed");
            if debug { println!("Converted: {}", c);}

            if verbose { println!("Parsing."); }
            let stmts = parser::read_string(&c).expect("Parser failed");
            if debug { println!("Parsed: {:?}", stmts); }

            if verbose { println!("Converting {} statements.", stmts.len()); }
            let solver_input = solver::convert(stmts).expect("Conversion failed");
            if debug { println!("Converted: {:?}", solver_input); }
            if verbose { println!("Converted {} nodes and {} edges.", 
                                  solver_input.nodes.len(), solver_input.edges.len()); }
            input = Some(solver_input);

        }
    }

    if input.is_none() {
        println!("No input.");
        return;
    }

    if verbose { println!("Solving."); }
    let output = solver::solve(input.unwrap()).expect("Solver failed");
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

