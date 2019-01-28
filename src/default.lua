check(input_file ~= nil, "No input file given")
check(from_format == "railml", "Unrecognized input file format.")

model = load_railml { 
    filename = input_file,

    track_objects = function(track)
        objs = {}
        if track.ocsElements ~= nil then
            extend(objs,track.ocsElements.signals)
            extend(objs,track.ocsElements.trainDetectionElements)
        end
        return objs
    end,

    get_pos = function(o) return o.absPos or o.pos end,

    symbol_info = function(o) 
        level = o.dir == "up" and -1 or 1
        return { pos = o.absPos or o.pos, width=0.4, origin = 0.0,level=level }
    end,
}

output = plot_network {
    model=model,
}

if output_format == "json" then
    print(to_json_pretty(output))

elseif output_format == "tikz" or output_format == "pdf" then
    tikz = ""
    tikz = tikz .. (tikz_tracks   { data = output, style = "ultra thick, black" })
    tikz = tikz .. (tikz_switches { data = output, style = "" })
    tikz = tikz .. (tikz_symbols  { data = output, draw = function(o) 
        x0,y0 = -o._symbol_info.origin, 0
        x1,y1 = o._symbol_info.width, -0.25*o._symbol_info.level
        return "\\draw ("..x0..","..y0..") rectangle ("..x1..","..y1..");"
    end })

    if output_format == "tikz" then 
        print(tikz)
    else 
        tikzpdf(output_file,tikz)
    end


elseif output_format == "svg" then
    svg = ""
    print(svg)

else
    error "Unknown output format."
end


