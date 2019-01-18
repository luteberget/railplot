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

use std::num::ParseIntError;
use std::path::PathBuf;
use structopt::StructOpt;

#[derive(Debug)]
enum InputFormat { RailML, Custom }

#[derive(Debug)]
enum OutputFormat { JSON, SVG, TikZ }

fn parse_hex(src: &str) -> Result<u32, ParseIntError> {
    u32::from_str_radix(src, 16)
}

fn parse_output_format(src: &str) -> Result<OutputFormat, String> {
    if src == "json" { return Ok(OutputFormat::JSON); }
    if src == "svg" { return Ok(OutputFormat::SVG); }
    if src == "tikz" { return Ok(OutputFormat::TikZ); }
    Err(format!("Unrecognized format: {}", src))
}

fn parse_input_format(src: &str) -> Result<InputFormat, String> {
    if src == "railml" { return Ok(InputFormat::RailML); }
    if src == "custom" { return Ok(InputFormat::Custom); }
    Err(format!("Unrecognized format: {}", src))
}


#[derive(Debug, StructOpt)]
#[structopt(about = "Linear schematic railway drawings")]
struct Opt {

    /// Activate debug mode
    #[structopt(short = "d", long = "debug")]
    debug: bool,

    /// Set speed
    #[structopt(short = "s", long = "speed", default_value = "42")]
    speed: f64,

    /// Input format
    #[structopt(short = "f", long = "format", default_value = "custom", parse(try_from_str="parse_input_format"))]
    input_format :InputFormat,

    /// Output format
    #[structopt(short = "t", long = "to", default_value = "json", parse(try_from_str="parse_output_format"))]
    output_format :OutputFormat,

    /// Input file
    #[structopt(parse(from_os_str))]
    input: PathBuf,

    /// Output file, stdout if not present
    #[structopt(parse(from_os_str))]
    output: Option<PathBuf>,

    #[structopt(short = "n", parse(try_from_str = "parse_hex"))]
    number: u32,

}

fn main() {
    let opt = Opt::from_args();
    println!("{:?}", opt);
}
