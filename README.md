# railplot

Railplot is a library and command line tool for converting
railway infrastructure data into a visual representation.
It can read tracks and trackside equipment from railML 2.x
and write results as a graphics file or as JSON data.

## Using railplot

Binary packages for Windows, Linux, and OS X can be downloaded
from the [releases page](https://github.com/luteberget/railplot/releases/latest).
After unpacking the archive, you should be able to plot the example input files, for example:

```shell
> railplot example-twotrack.railml.xml twotrack-output.svg --title "railplot example"
```

Which outputs into `twotrack-output.svg`:

![railplot output example](./.imgs/twotrack-output.svg)

Further command line usage is presented in the help output:

```shell
> railplot --help
railplot 0.3.1
Bjørnar Luteberget <luteberget@gmail.com>
Linear schematic railway drawings.
See manual at https://github.com/luteberget/railplot/.

USAGE:
    railplot [FLAGS] [OPTIONS] [ARGS]

FLAGS:
    -d, --dump-script    Dump the default script file (see the --script flag)
        --help           Prints help information
    -V, --version        Prints version information
    -v, --verbose        Verbose mode (-v, -vv, -vvv, etc.)

OPTIONS:
    -f, --from <input_format>    Input format: railml or sgraph
    -t, --to <output_format>     Output format: json, svg, tikz, or pdf
    -s, --script <script>        Use a custom script file instead of the default
    -c, --style <style>          Symbol style: simple or ertms
    -h, --title <title>          Title to be written on the output graphic.

ARGS:
    <input>     Input file
    <output>    Output file
```

## Default script

The railplot program has a default presentation style which can be configured to
show simple or ERTMS-like symbols. 

For more advanced applications, the built in styles and output formats can be 
insufficient, and you may choose to instead use the JSON output mode and 
produce drawings from that data in some other way, or the railplot scripting interface
can be used to extend the symbol appearances, supported object types, and output formats.
See the custom script section below.


## Custom script

The railplot program is driven by a [Lua](https://www.lua.org/) script, 
which may be replaced by using the `--script` command line option.
The script will be executed as a Lua program with some built-in functions exposed
for finding and optimizing a railway track layout. 
To get the default script, run railplot with the `--dump-script` option,
and modify it to suit your use case.


