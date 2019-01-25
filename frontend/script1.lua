
input_file = "../examples/twotrack.railml"
input_file_format = "railml"

check(input_file ~= nil, "No input file given.")
check(input_file_format == "railml", "Need RailML file.")

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

    get_pos = function(o) return o.pos end,

    symbol_info = function(o) 
        return { pos = o.absPos or o.pos, width=0.4, origin = 0.0,level=1 }
    end,
}

output = plot_network {
    model=model,
}

print(tikz_tracks   { data = output, style = "ultra thick, black" })
print(tikz_switches { data = output, style = "yellow" })
print(tikz_symbols  { data = output, draw = function(o) 
    x0,y0 = -o._symbol_info.origin, 0
    x1,y1 = o._symbol_info.width, 0.25
    return "\\draw ("..x0..","..y0..") rectangle ("..x1..","..y1..");"
end })


