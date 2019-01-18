#[macro_use]
extern crate clap;
extern crate failure;
extern crate vis_rs;
extern crate rolling;
extern crate serde_json;
use vis_rs::*;

fn main() {
    use clap::{Arg};
    let matches = clap::App::new("vis_rs")
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
        .arg(Arg::with_name("outgraph")
             .short("g")
             .long("outgraph")
             .help("Write vis-rs  graph format to file (when reading infrastructure with the -i flag")
             .value_name("FILE")
             .takes_value(true))
        .arg(Arg::with_name("js")
             .short("j")
             .long("js")
             .help("Write javascript output data for visualization.")
             .value_name("FILE")
             .takes_value(true))
        .arg(Arg::with_name("svg")
             .short("s")
             .long("svg")
             .help("Write SVG output data for visualization.")
             .value_name("FILE")
             .takes_value(true))
        .arg(Arg::with_name("tikz")
             .short("z")
             .long("tikz")
             .help("Write TikZ output data for visualization.")
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
    let svg_filename = matches.value_of("svg");
    let tikz_filename = matches.value_of("tikz");
    let outgraph_filename = matches.value_of("outgraph");
    println!("Convert from infrastructure: {:?}", infrastructure_filename);

    let mut input = None;
    let mut orig_edges = None;
    let mut node_names = None;
    let mut pos_range = None;
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
            let inf = rolling::get_infrastructure(&std::path::Path::new(filename)).expect("Infrastructure parser failed");
            let (c,oe,pos) = convert::convert(&inf).expect("D-graph conversion failed");

            if let Some(file) = outgraph_filename {
                use std::fs::File;
                use std::io::BufWriter;
                use std::io::Write;
                let mut file = File::create(file).expect("could not create file");
                let mut writer = BufWriter::new(&file);
                write!(writer, "{}", c);
                if verbose { println!("Wrote vis-rs graph format output to file."); }
            }

            orig_edges = Some(oe);
            pos_range = Some(pos);
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

    let input2 = input.clone();
    if verbose { println!("Solving."); }
    let (output,portref_changes) = solver::solve_difftheory(input.unwrap()).expect("Solver failed");
    if verbose { 
        use std::f64;
        let width = output.tracks.node_coords.iter().map(|(_,x,_)| *x).fold(-1./0., f64::max);
        let height = output.tracks.node_coords.iter().map(|(_,_,y)| *y)
            .chain(  output.tracks.edge_levels.iter().map(|(_,_,y)| *y))
            .fold(-1./0.,f64::max);
        println!("Finished solving, size is {} x {}.", width, height);
        println!("Resolved port ref changes {:?}", portref_changes);
    }
    if debug {
        println!("solution {:?}", output);
    }


    if let Some(file) = js_filename {
        use std::fs::File;
        use std::io::BufWriter;
        use std::io::Write;
        let mut file = File::create(file).expect("could not create file");
        let (edges,_) = convert_javascript(Schematic {
            result: output.clone(),
            original_edges: orig_edges.unwrap(),
            pos_range: pos_range.unwrap(),
            node_names: node_names.unwrap(),
            portref_changes: portref_changes,
        }).unwrap();
        let string = format!("var edges = {};",serde_json::to_string_pretty(&edges).unwrap());
        let mut writer = BufWriter::new(&file);
        write!(writer, "{}", string); 
        if verbose { println!("Wrote javascript output to file."); }
    }

    if let Some(file) = svg_filename {
        use std::fs::File;
        use std::io::BufWriter;
        use std::io::Write;
        if verbose { println!("Converting to SVG."); }
        let svg = svg_output::convert(&output).expect("Could not convert to SVG");;
        let mut file = File::create(file).expect("could not create file");
        let mut writer = BufWriter::new(&file);
        write!(writer, "{}", svg); 
        if verbose { println!("Wrote SVG output to file."); }
    }

    if let Some(file) = tikz_filename {
        use std::fs::File;
        use std::io::BufWriter;
        use std::io::Write;
        if verbose { println!("Converting to TikZ."); }
        let tikz = tikz_output::convert(&input2.unwrap(), &output).expect("Could not convert to SVG");;
        let mut file = File::create(file).expect("could not create file");
        let mut writer = BufWriter::new(&file);
        write!(writer, "{}", tikz); 
        if verbose { println!("Wrote TikZ output to file."); }
    }

    if verbose { println!("Finished."); }
}
