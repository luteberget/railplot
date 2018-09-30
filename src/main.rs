#[macro_use] 
extern crate serde_json;
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
mod json;

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
        .arg(Arg::with_name("js")
             .short("j")
             .long("js")
             .help("Write javascript output data for visualization.")
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
    let js_filename = matches.value_of("js");
    println!("Convert from infrastructure: {:?}", infrastructure_filename);

    let mut input = None;
    let mut orig_edges = None;
    let mut node_names = None;
    if let Some(filename) = filename {
        if verbose { println!("Parsing \"{}\".",filename); }
        let stmts = parser::read_file(std::path::Path::new(filename)).expect("Parser failed");
        if debug { println!("Parsed: {:?}", stmts); }

        if verbose { println!("Converting {} statements.", stmts.len()); }
        let (solver_input,nnames) = solver::convert(stmts).expect("Conversion failed");
        if debug { println!("Converted: {:?}", solver_input); }
        if verbose { println!("Converted {} nodes and {} edges.", 
                              solver_input.nodes.len(), solver_input.edges.len()); }
        input = Some(solver_input);
        node_names = Some(nnames);
        
    } else {
        if let Some(filename) = infrastructure_filename {
            if verbose { println!("Converting d-graph \"{}\".",filename); }
            let (c,oe) = convert::convert(std::path::Path::new(filename)).expect("D-graph conversion failed");
            orig_edges = Some(oe);
            if debug { println!("Converted: {}", c);}

            if verbose { println!("Parsing."); }
            let stmts = parser::read_string(&c).expect("Parser failed");
            if debug { println!("Parsed: {:?}", stmts); }

            if verbose { println!("Converting {} statements.", stmts.len()); }
            let (solver_input,nnames) = solver::convert(stmts).expect("Conversion failed");
            
            if debug { println!("Converted: {:?}", solver_input); }
            if verbose { println!("Converted {} nodes and {} edges.", 
                                  solver_input.nodes.len(), solver_input.edges.len()); }
            input = Some(solver_input);

            node_names = Some(nnames);
        }
    }

    if input.is_none() {
        println!("No input.");
        return;
    }

    if debug {
        for (i,n) in input.as_ref().unwrap().nodes.iter().enumerate() {
            println!("n{}: {:?}", i, n);
        }
        for (i,e) in input.as_ref().unwrap().edges.iter().enumerate() {
            println!("e{}: {:?}", i, e);
        }
    }

    if verbose { println!("Solving."); }
    let output = solver::solve(input.unwrap()).expect("Solver failed");
    if verbose { 
        use std::f64;
        let width = output.node_coords.iter().map(|(_,x,_)| *x).fold(-1./0., f64::max);
        let height = output.node_coords.iter().map(|(_,_,y)| *y)
            .chain(  output.edge_levels.iter().map(|(_,_,y)| *y))
            .fold(-1./0.,f64::max);
        println!("Finished solving, size is {} x {}.", width, height);
    }
    if debug {
        println!("solution {:?}", output);
    }


    if let Some(file) = js_filename {
        let oe = orig_edges.expect("Could not find original d-graph edge map.");
        let nnames = node_names.expect("Could not find original node names.");
        use std::fs::File;
        use std::io::BufWriter;
        use std::io::Write;
        if verbose { println!("Converting to javascript output."); }
        let string = json::javascript_output(&output, &oe, &nnames).expect("Could not convert output to javascript format");
        let mut file = File::create(file).expect("could not create file");
        let mut writer = BufWriter::new(&file);
        write!(writer, "{}", string); 
        if verbose { println!("Wrote javascript output to file."); }
    }

    if verbose { println!("Finished."); }
}
