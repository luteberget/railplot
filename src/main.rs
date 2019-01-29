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

use log::*;

mod convert_lua;
mod xml;
mod railml;
mod tikz_output;
mod svg_output;

#[derive(Debug, StructOpt)]
#[structopt(about = "Linear schematic railway drawings.\nSee manual at https://github.com/luteberget/railplot/.")]
struct Opt {
    /// Verbose mode (-v, -vv, -vvv, etc.)
    #[structopt(short = "v", long = "verbose", parse(from_occurrences))]
    verbose: u8,

    /// Input file
    #[structopt(parse(from_os_str))]
    input :Option<PathBuf>,

    /// Output file
    #[structopt(parse(from_os_str))]
    output :Option<PathBuf>,

    /// Input format: railml or sgraph
    #[structopt(short="f", long="from")]
    input_format :Option<String>,

    /// Output format: json, svg, tikz, or pdf
    #[structopt(short="t", long="to")]
    output_format :Option<String>,

    /// Use a custom script file instead of the default
    #[structopt(short="s", long="script")]
    script :Option<String>,

    /// Dump the default script file (see the --script flag)
    #[structopt(short="d", long="dump-script")]
    dump_script :bool,

    /// Title to be written on the output graphic.
    #[structopt(short="h",long="title")]
    title :Option<String>,

    /// Symbol style: simple or ertms
    #[structopt(short="c",long="style")]
    style :Option<String>,
}

fn main() -> Result<(),ExitFailure> {
    use std::fs;
    use rlua::prelude::*;

    //let opt = Opt::from_args();
    let clap = Opt::clap();
    let clap_matches = clap.get_matches();
    let opt = Opt::from_clap(&clap_matches);

    // Setup logging, use environment variable if available, otherwise command line switches.
    if let Ok(ref envvar) = std::env::var("RAILPLOT_LOG") {
        env_logger::Builder::new()
            .parse(envvar)
            .init();
    } else {
        env_logger::Builder::new()
            .filter(None, match opt.verbose {
                //0 => LevelFilter::Error,
                0 => LevelFilter::Warn,
                1 => LevelFilter::Info,
                2 => LevelFilter::Debug,
                _ => LevelFilter::Trace,
            }).init();
    }


    let default_script = include_str!("default.lua");

    if opt.dump_script {
        println!("{}", default_script);
        return Ok(());
    }

    let (script,script_name) = if let Some(ref filename) = &opt.script {
        (fs::read_to_string(filename)
            .context("Could not read script file")?,
         filename.as_str())
    } else {
        (default_script.to_string(),"railplot-default.lua")
    };

    let lua = Lua::new();
    lua.context::<_,Result<(),failure::Error>>(|l| {
        let g = l.globals();

        // Set variables from command line info
        if let Some(ref t) = &opt.title {
            g.set("title",t.as_str())?;
        }
        if let Some(ref t) = &opt.style {
            g.set("style",t.as_str())?;
        }
        if let Some(ref f) = &opt.input_format {
            g.set("input_format",f.as_str())?;
        } else if let Some(ref filename) = &opt.input {
            let s = filename.to_string_lossy();
            if s.ends_with("railml") || s.ends_with("xml") {
                g.set("input_format", "railml")?;
            } else if s.ends_with("sgraph") {
                g.set("input_format", "sgraph")?;
            }
        }
        if let Some(ref f) = &opt.output_format {
            g.set("output_format",f.as_str())?;
        } else if let Some(ref filename) = &opt.output {
            let s = filename.to_string_lossy();
            if s.ends_with("json") { g.set("output_format", "json")?; }
            if s.ends_with("svg")  { g.set("output_format", "svg")?; }
            if s.ends_with("tikz") { g.set("output_format", "tikz")?; }
            if s.ends_with("pdf")  { g.set("output_format", "pdf")?; }
            if s.ends_with("png")  { g.set("output_format", "png")?; }
        }
        if let Some(ref f) = &opt.input {
            g.set("input_file",&*f.to_string_lossy())?;
        }
        if let Some(ref f) = &opt.output {
            g.set("output_file",&*f.to_string_lossy())?;
        }


        // Load library stored in lua
        l.load(include_str!("lib.lua")).set_name("railplotlib")?.exec()
            .context("Error loading railplot Lua library!")?;


        // Rust functions library
        g.set("load_xml", l.create_function(load_xml)?)?;
        g.set("load_railml", l.create_function(load_railml)?)?;
        g.set("plot_network", l.create_function(plot_network)?)?;

        g.set("tikz_tracks", l.create_function(tikz_output::tikz_tracks)?)?;
        g.set("tikz_switches", l.create_function(tikz_output::tikz_switches)?)?;
        g.set("tikz_symbols", l.create_function(tikz_output::tikz_symbols)?)?;

        g.set("tikzpdf", l.create_function(tikzpdf)?)?;

        g.set("svg_tracks", l.create_function(svg_output::svg_tracks)?)?;
        g.set("svg_switches", l.create_function(svg_output::svg_switches)?)?;
        g.set("svg_symbols", l.create_function(svg_output::svg_symbols)?)?;
        g.set("drawing_size", l.create_function(svg_output::drawing_size)?)?;
        g.set("svg_document", l.create_function(svg_output::svg_document)?)?;

        g.set("rsvgpng", l.create_function(rsvgpng)?)?;

        g.set("log_error", l.create_function(|_ctx, s:String| { 
            error!("{}", s);  Ok(())} )?)?;
        g.set("log_warn", l.create_function(|_ctx, s:String| { 
            warn!("{}", s);  Ok(())} )?)?;
        g.set("log_info", l.create_function(|_ctx, s:String| { 
            info!("{}", s);  Ok(())} )?)?;
        g.set("log_debug", l.create_function(|_ctx, s:String| { 
            debug!("{}", s);  Ok(())} )?)?;
        g.set("log_trace", l.create_function(|_ctx, s:String| { 
            trace!("{}", s);  Ok(())} )?)?;
        g.set("print_help", l.create_function(|_ctx, msg:String| { 
            // Print clap help.
            println!("Error: {}", msg);
            // This exits the program showing the help screen.
            Opt::clap().get_matches_from(vec!["railplot", "--help"]);
        Ok(())} )?)?;

        g.set("write", l.create_function(lua_write_file)?)?;

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



        let x = l.load(&script)
            .set_name(script_name.as_bytes())?.exec()
            .map_err(|e| match e { 
                rlua::Error::CallbackError { cause , .. } => rlua::Error::CallbackError {
                    traceback: format!("{}",cause), cause: cause,
                },
                _ => e,
            })
            .with_context(|_| format!("Error executing input script."));
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
fn load_xml<'l>(ctx :rlua::Context<'l>, 
                (filename,a):(String,Option<rlua::Table<'l>>)) 
                              -> Result<rlua::Value<'l>,rlua::Error>
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
    use crate::convert_lua::*;
    use railplotlib::model::*;
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
    let ns = root.ns().ok_or("Missing XML namespace.")
        .to_lua_err()?.to_string();
    let branching :BranchingModel<rlua::Value> = 
        railml::railml_to_branching(&root,&ns,
                                    get_xml_objects,get_xml_pos)
        .to_lua_err()?;
    let schematic :SchematicGraph<rlua::Value> = 
        railml::branching_to_schematic_graph(branching)
        .to_lua_err()?;
    let lua = convert_lua::schematic_graph_to_lua(ctx, schematic)
        .to_lua_err()?;
    Ok(lua)
}

fn lua_write_file<'l>(_c :rlua::Context<'l>, (filename,text):(Option<String>,String))
-> Result<(),rlua::Error> {
    if let Some(f) = filename {
        if f != "-" {
            std::fs::write(f, text).to_lua_err()?;
            return Ok(())
        }
    }
    println!("{}", text);
    Ok(())
}

fn plot_network<'l>(ctx :rlua::Context<'l>, args:rlua::Table<'l>) 
-> Result<rlua::Value<'l>,rlua::Error> {
    let m = args.get::<_,rlua::Table>("model")
        .map_err(|e| format!("Requires model argument. {}", e)).to_lua_err()?;
    let m = convert_lua::schematic_graph_from_lua(&m)?;

    // choose solver
    let method = args.get::<_,String>("method").unwrap_or("levelssat".to_string());
    use railplotlib::solvers::SchematicSolver;
    use railplotlib::solvers::Goal;
    let solver = match method.as_str() {
        "levelssat" => Box::new(railplotlib::solvers::LevelsSatSolver { 
            criteria: vec![Goal::Bends, Goal::Height, Goal::Width],
        }), 
        _ => panic!(),
    };

    let output = solver.solve(m).to_lua_err()?;
    let lua_output = convert_lua::output_to_lua(ctx, output)?;
    Ok(lua_output)
}

fn tikzpdf<'l>(_ctx :rlua::Context<'l>, 
               (mut filename,text,preamble):(String,String,Option<String>)) 
                                         -> Result<(),rlua::Error> {
    if filename.ends_with(".pdf") {
        filename = filename.chars().take(filename.len()-4).collect();
    }
    let mut input = String::new();
    input.push_str(r#"\documentclass[tikz,margin=5mm]{standalone}
                   \usepackage{pgfplots,amsmath}"#);
    if let Some(pre) = preamble { input.push_str(&pre); }
    input.push_str(r#"\begin{document} \begin{tikzpicture}"#);
    input.push_str(&text);
    input.push_str(r#"\end{tikzpicture} \end{document}"#);

    use std::process;

    info!("Starting pdflatex");
    let mut proc = process::Command::new("pdflatex")
        .args(&["-jobname",&filename])
        .stdin(process::Stdio::piped())
        .stdout(process::Stdio::piped())
        .spawn().to_lua_err()?;

    {
        let stdin = proc.stdin.as_mut()
            .ok_or(format!("Failed to open pdflatex input pipe."))
            .to_lua_err()?;
        use std::io::Write;
        stdin.write_all(input.as_bytes()).to_lua_err()?;
    }
    proc.wait_with_output().to_lua_err()?;
    info!("Pdflatex finished");
    Ok(())
}

fn rsvgpng<'l>(_ctx :rlua::Context<'l>, 
               (filename,text,args):(String,String,Option<rlua::Table<'l>>)) 
                                         -> Result<(),rlua::Error> {
    let mut input = String::new();
    input.push_str(&text);

    use std::process;
    let mut cliargs = vec!["-f", "png", "-o", &filename];
    let bg_color = args.and_then(|x| x.get::<_,String>("background_color").ok());
    if let Some(ref c) = bg_color {
        cliargs.push("-b");
        cliargs.push(c);
    }

    info!("Starting rsvg-convert");
    let mut proc = process::Command::new("rsvg-convert")
        .args(&cliargs)
        .stdin(process::Stdio::piped())
        .stdout(process::Stdio::piped())
        .spawn().to_lua_err()?;

    {
        let stdin = proc.stdin.as_mut()
            .ok_or(format!("Failed to open pdflatex input pipe."))
            .to_lua_err()?;
        use std::io::Write;
        stdin.write_all(input.as_bytes()).to_lua_err()?;
    }
    proc.wait_with_output().to_lua_err()?;
    info!("rsvg-convert finished");
    Ok(())
}
