# railplot

Railplot is a library and command line tool for converting
railway infrastructure data into a visual representation.
It can read tracks and trackside equipment from railML or
the railplotinf format and write output to JSON, SVG or TikZ.
(TODO It can also produce PDF and JPG on systems that have
converters installed)

## Using railplot

```shell
railplot -f railml -t svg my_infrastructure.xml
```

## Implementation
This command line tool uses three (four?) libraries:
 * railml input conversion to graph / label-loc-size
 * schematic drawing constraint solver (levellp/levelsat/gridsat)
 * schematic symbol placement
 * output format template stuff

