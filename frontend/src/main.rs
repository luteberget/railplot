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
use rlua::ExternalResult;

use std::path::PathBuf;
use structopt::StructOpt;

mod schematic_graph;
mod solvers;
mod xml;
mod railml;

mod levelssat;
mod edgeorder;

mod tikz_output;

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
    use std::fs;
    let script_file_contents = fs::read_to_string(&opt.script).expect("Could not read file.");

    use rlua::prelude::*;
    let lua = Lua::new();
    lua.context::<_,Result<(),failure::Error>>(|l| {
        // Load library stored in lua
        l.load(include_str!("std.lua")).set_name("railplotlib")?.exec()
            .with_context(|_| format!("Error loading railplot Lua library!"))?;


        let g = l.globals();
        // Rust functions library
        g.set("load_xml", l.create_function(load_xml)?)?;
        g.set("load_railml", l.create_function(load_railml)?)?;
        g.set("plot_network", l.create_function(plot_network)?)?;

        g.set("tikz_tracks", l.create_function(tikz_output::tikz_tracks)?)?;
        g.set("tikz_switches", l.create_function(tikz_output::tikz_switches)?)?;
        g.set("tikz_symbols", l.create_function(tikz_output::tikz_symbols)?)?;

        g.set("tikzpdf", l.create_function(tikzpdf)?)?;

        use crate::xml::lua_to_json;
        let to_json = l.create_function(|ctx, obj :rlua::Value| {
            let json = lua_to_json(ctx,obj).to_lua_err()?;
            Ok(serde_json::to_string(&json).to_lua_err()?)
        })?;
        l.globals().set("to_json",to_json)?;
        let to_json_pretty = l.create_function(|ctx, obj :rlua::Value| {
            let json = lua_to_json(ctx,obj).to_lua_err()?;
            Ok(serde_json::to_string_pretty(&json).to_lua_err()?)
        })?;
        l.globals().set("to_json_pretty",to_json_pretty)?;


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


/// Read an XML from given filename, converting to a Lua table.
/// Optionally, an array of element names can be given which are interpreted
/// as array containers instead of objects. An example:
///
/// ```xml
/// <objects>
///   <object> ... </object>
///   <object> ... </object>
/// </objects>
/// ```
/// 
/// Here, putting the string "objects" into the arrays table will avoid
/// creating a associative table where the "object" key points to an array 
/// of objects, and instead return the array of <object>s directly.
fn load_xml<'l>(ctx :rlua::Context<'l>, (filename,a):(String,Option<rlua::Table<'l>>)) -> Result<rlua::Value<'l>,rlua::Error>
{
    let arrays = if let Some(t) = a {
        t.sequence_values::<String>().collect::<Result<Vec<_>,_>>()?
    } else { Vec::new() };

    Ok(xml::json_to_lua(ctx, 
            xml::load_xml_to_json(&filename, &arrays).to_lua_err()?
        ).to_lua_err()?)
}

/// RailML 2.x import function. Takes an associative table of arguments and
/// produces a schematic graph object.
///
/// Arguments:
///  * filename -- string, path to file.
///  * track_objects -- function, for each track in the railml infrastructure 
///      element, return an array of objects to be drawn as symbols.
///  * get_pos -- function, for each element, return its absolute position
///      on the drawing. The default choices is the absPos attribute.
///  * symbol_info -- function, for each element returned by track_objects, 
///      return an associative table containing symbol pos, width, origin, level.
///
fn load_railml<'l>(ctx :rlua::Context<'l>, args:rlua::Table<'l>) -> Result<rlua::Value<'l>,rlua::Error> {
    use crate::railml::*;
    use crate::schematic_graph::*;
    let a = vec!["signals".to_string(),"trainDetectionElements".to_string()];

    let filename = args.get::<_,String>("filename")
        .map_err(|e| format!("Requires filename argument. {}",e)).to_lua_err()?;

    let track_objects = args.get::<_,rlua::Function>("track_objects")
        .map_err(|e| format!("Requires track_objects function argument. {}",e)).to_lua_err()?;

    let get_pos = args.get::<_,rlua::Function>("get_pos")
        .map_err(|e| format!("Requires get_pos function argument. {}",e)).to_lua_err()?;

    let get_symbol_info = args.get::<_,rlua::Function>("symbol_info")
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
            let symbol = get_symbol_info.call::<_,rlua::Table>(v.clone())
                .map_err(|e| format!("{}",e))?;
            let symbol = symbol_from_lua(&symbol)?;
            vec.push((pos, BrObject::Other(symbol, rlua::Value::Table(v))));
        }
        Ok(vec)
    };

    let root = xml::open_xml(&filename).to_lua_err()?;
    let ns = root.ns().ok_or("Missing XML namespace.").to_lua_err()?.to_string();
    let branching :BranchingModel<rlua::Value> = railml::railml_to_branching(&root,&ns,
                         get_xml_objects,get_xml_pos)
        .to_lua_err()?;
    let schematic :SchematicGraph<rlua::Value> = railml::branching_to_schematic_graph(branching)
        .to_lua_err()?;
    let lua = schematic_graph::schematic_graph_to_lua(ctx, schematic)
        .to_lua_err()?;
    Ok(lua)
}

fn plot_network<'l>(ctx :rlua::Context<'l>, args:rlua::Table<'l>) -> Result<rlua::Value<'l>,rlua::Error> {
    let m = args.get::<_,rlua::Table>("model")
        .map_err(|e| format!("Requires model argument. {}", e)).to_lua_err()?;
    let m = schematic_graph::schematic_graph_from_lua(&m)?;

    // choose solver
    let method = args.get::<_,String>("method").unwrap_or("levelssat".to_string());
    use self::solvers::SchematicSolver;
    use self::solvers::Goal;
    let solver = match method.as_str() {
        "levelssat" => Box::new(solvers::LevelsSatSolver { criteria:
            vec![Goal::Bends, Goal::Height, Goal::Width],
        }), 
        _ => panic!(),
    };

    let output = solver.solve(m).to_lua_err()?;
    let lua_output = solvers::output_to_lua(ctx, output)?;
    Ok(lua_output)
}

fn tikzpdf<'l>(_ctx :rlua::Context<'l>, (filename,text,preamble):(String,String,Option<String>)) -> Result<(),rlua::Error> {
    let mut input = String::new();
    input.push_str(r#"\documentclass[tikz,margin=5mm]{standalone} \usepackage{pgfplots,amsmath}"#);
    if let Some(pre) = preamble { input.push_str(&pre); }
    input.push_str(r#"\begin{document} \begin{tikzpicture}"#);
    input.push_str(&text);
    input.push_str(r#"\end{tikzpicture} \end{document}"#);

    use std::process;

    let mut proc = process::Command::new("pdflatex")
        .args(&["-jobname",&filename])
        .stdin(process::Stdio::piped())
        .spawn().to_lua_err()?;

    {
        let stdin = proc.stdin.as_mut().ok_or(format!("Failed to open pdflatex input pipe.")).to_lua_err()?;
        use std::io::Write;
        stdin.write_all(input.as_bytes()).to_lua_err()?;
    }
    proc.wait().to_lua_err()?;
    Ok(())
}
