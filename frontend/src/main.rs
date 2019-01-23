//! Railplot is a library and command line tool for converting
//! railway infrastructure data into a visual representation.
//! It can read tracks and trackside equipment from railML or
//! the railplotinf format and write output to JSON, SVG or TikZ.
//! (TODO It can also produce PDF and JPG on systems that have
//! converters installed)
//!
//! # Using railplot
//!
//! ```shell
//! railplot -f railml -t svg my_infrastructure.xml
//! ```
//!
//! # Implementation
//!
//! This command line tool uses three (four?) libraries:
//!
//!  * railml input conversion to graph / label-loc-size
//!  * schematic drawing constraint solver (levellp/levelsat/gridsat)
//!  * schematic symbol placement
//!  * output format template stuff
//!

use failure::ResultExt;
use exitfailure::ExitFailure;

use std::path::PathBuf;
use structopt::StructOpt;

mod schematic_graph;
mod solver;
mod xml;
mod railml;

//#[derive(Debug, Copy, Clone)]
//enum InputFormat { RailML, Custom, Script }
//
//#[derive(Debug, Copy, Clone)]
//enum OutputFormat { JSON, SVG, TikZ }
//
//fn parse_output_format(src: &str) -> Result<OutputFormat, String> {
//    if src == "json" { return Ok(OutputFormat::JSON); }
//    if src == "svg" { return Ok(OutputFormat::SVG); }
//    if src == "tikz" { return Ok(OutputFormat::TikZ); }
//    Err(format!("Unrecognized format: {}", src))
//}
//
//fn parse_input_format(src: &str) -> Result<InputFormat, String> {
//    if src == "railml" { return Ok(InputFormat::RailML); }
//    if src == "custom" { return Ok(InputFormat::Custom); }
//    if src == "script" { return Ok(InputFormat::Script); }
//    Err(format!("Unrecognized format: {}", src))
//}


#[derive(Debug, StructOpt)]
#[structopt(about = "Linear schematic railway drawings")]
struct Opt {

    // /// Activate debug mode
    // #[structopt(short = "d", long = "debug")]
    // debug: bool,

    ///// Input format
    //#[structopt(short = "f", long = "format", parse(try_from_str="parse_input_format"))]
    //input_format :Option<InputFormat>,

    ///// Output format
    //#[structopt(short = "t", long = "to", parse(try_from_str="parse_output_format"))]
    //output_format :Option<OutputFormat>,

    ///// Input file
    //#[structopt(parse(from_os_str))]
    //input: Option<PathBuf>,

    /// Script file
    #[structopt(parse(from_os_str))]
    script: PathBuf,

    ///// Output file, stdout if not present
    //#[structopt(parse(from_os_str))]
    //output: Option<PathBuf>,

}

//struct Object {
//    // json object here?
//}

fn main() -> Result<(),ExitFailure> {
    let opt = Opt::from_args();
    //println!("{:?}", opt);

    //use std::env;
    //use std::path::Path;
    use std::fs;
    use std::ffi::OsStr;

    let script_file_contents = fs::read_to_string(&opt.script).expect("Could not read file.");
    //let input_format = (opt.input_format).map(|x| Ok(x)).unwrap_or_else(|| {
    //    opt.input.extension().ok_or(()).map(|x| match OsStr::to_str(x).unwrap() {
    //        "railml" => Ok(InputFormat::RailML),
    //        "lua" | "script" | "railplot" => Ok(InputFormat::Script),
    //        "in" => Ok(InputFormat::Custom),
    //        _ => Err(()),
    //    }).unwrap()
    //}).expect("Unknown file format (use the --format argument).");

    //println!("Opening {:?} file: {:?}", input_format, file_contents);

    use rlua::prelude::*;
    let lua = Lua::new();
    lua.context::<_,Result<(),failure::Error>>(|l| {
        // Load library stored in lua
        l.load(include_str!("std.lua")).set_name("railplotlib")?.exec()
            .with_context(|_| format!("Error loading railplot Lua library!"))?;


        // Load library from rust.
        
        let load_xml = l.create_function(|ctx, (filename,a):(String,Option<rlua::Table>)| {
            let arrays = if let Some(t) = a {
                t.sequence_values::<String>().collect::<Result<Vec<_>,_>>()?
            } else { Vec::new() };

            Ok(xml::json_to_lua(ctx, 
                  xml::load_xml_to_json(&filename, &arrays).to_lua_err()?
              ).to_lua_err()?)
        })?;
        l.globals().set("load_xml",load_xml)?;

        let plot_network = l.create_function(|ctx, args:rlua::Table| {
            let m = args.get::<_,rlua::Table>("model")
                .map_err(|e| format!("Requires model argument. {}", e)).to_lua_err()?;
            let m = schematic_graph::schematic_graph_from_lua(&m)?;

            // choose solver
            let method = args.get::<_,String>("method").unwrap_or("levelssat".to_string());
            let solver = match method.as_str() {
                "levelssat" => solver::levelssat::Solver {}, 
                _ => panic!(),
            };

            let output = solver.solve(m);

            let lua_output=  output_to_lua(output);

            Ok(lua_output)
        })?;
        l.globals().set("plot_network",plot_network)?;

        let load_railml = l.create_function(|ctx, args:rlua::Table| {
            use crate::railml::*;
            use crate::schematic_graph::*;
            let a = vec!["signals".to_string(),"trainDetectionElements".to_string()];

            let filename = args.get::<_,String>("filename")
                .map_err(|e| format!("Requires filename argument. {}",e)).to_lua_err()?;

            let track_objects = args.get::<_,LuaFunction>("track_objects")
                .map_err(|e| format!("Requires track_objects function argument. {}",e)).to_lua_err()?;

            let get_pos = args.get::<_,LuaFunction>("get_pos")
                .map_err(|e| format!("Requires get_pos function argument. {}",e)).to_lua_err()?;

            let get_symbol_info = args.get::<_,LuaFunction>("symbol_info")
                .map_err(|e| format!("Requires symbol_info function argument. {}",e)).to_lua_err()?;

            let get_xml_pos = |e :&minidom::Element| {
                let v = xml::json_to_lua(ctx, 
                             xml::xml_to_json(e, &a).map_err(|e| format!("{}",e))?
                         ).map_err(|e| format!("{}",e))?;
                let result = get_pos.call::<_,f64>(v).map_err(|e| format!("{}",e))?;
                Ok(result)
            };

            let get_xml_objects = |e :&minidom::Element| {
                // Convert element to json
                let v = xml::json_to_lua(ctx, 
                             xml::xml_to_json(e, &a).map_err(|e| format!("{}",e))?
                         ).map_err(|e| format!("{}",e))?;
                // Run track_objects function on it
                let result = track_objects.call::<_,rlua::Table>(v)
                    .map_err(|e| format!("{}",e))?;
                let mut vec = Vec::new();
                for v in result.sequence_values::<rlua::Table>() {
                    // Get "pos" value back and store the reference to the object.
                    let v = v.map_err(|e| format!("{}",e))?;
                    let pos = get_pos.call::<_,f64>(v.clone()).map_err(|e| format!("{}",e))?;

                    // Get "symbol" value back and store it in the object
                    let symbol = get_symbol_info.call::<_,LuaTable>(v.clone())
                        .map_err(|e| format!("{}",e))?;
                    let symbol = symbol_from_lua(&symbol)?;
                    vec.push((pos, BrObject::Other(symbol, rlua::Value::Table(v))));
                }
                Ok(vec)
            };

            let root = xml::open_xml(&filename).to_lua_err()?;
            let ns = root.ns().ok_or("Missing XML namespace.").to_lua_err()?.to_string();
            println!("BRANCHING");
            let branching :BranchingModel<rlua::Value> = railml::railml_to_branching(&root,&ns,
                                 get_xml_objects,get_xml_pos)
                .to_lua_err()?;
            let schematic :SchematicGraph<rlua::Value> = railml::branching_to_schematic_graph(branching)
                .to_lua_err()?;
            println!("SCHEMATIC OK");
            let lua = schematic_graph::schematic_graph_to_lua(ctx, schematic)
                .to_lua_err()?;
            Ok(lua)
        })?;
        l.globals().set("load_railml",load_railml)?;


        //let program = match input_format {
        //    InputFormat::Script => file_contents,
        //    InputFormat::RailML => r#" print("loading railml")
        //    "#.to_string(),
        //    InputFormat::Custom => panic!(),
        //};

        let x = l.load(&script_file_contents)
            .set_name(&opt.script.to_string_lossy().as_bytes())?.exec()
            .map_err(|e| match e { 
                rlua::Error::CallbackError { cause , .. } => rlua::Error::CallbackError {
                    traceback: format!("{}",cause), cause: cause,
                },
                _ => e,
            })
            .with_context(|_| format!("Error executing input script."));
        //println!("X WAS {:?}", x);
        x?;
        Ok(())
     })?;
    Ok(())
}
