
input_file = "../examples/oneitem.railml"
input_file_format = "railml"

check(input_file ~= nil, "No input file given.")
check(input_file_format == "railml", "Need RailML file.")

model = load_railml { 

    -- Load input file as railML file
    filename = input_file,

    track_objects = function(track)
        objs = {}
        if track.ocsElements ~= nil then
            extend(objs,track.ocsElements.signals)
            extend(objs,track.ocsElements.trainDetectionElements)
        end
        print("Track "..track.name.." has ".. #objs.. " objects")
        return objs
    end,

    get_pos = function(o) return o.pos end,

    symbol_info = function(o) 
        return { pos = o.absPos or o.pos, width=0.4, origin = 0.0,level=1 }
    end,

    normalize_dirs   = false,
    normalize_abspos = false,
}

print "parsed railml"
print ((#model.nodes) .." nodes and " .. (#model.edges) .. " edges.")
for k,v in pairs(model.nodes) do
    print("node " .. dump(v))
end
for k,v in pairs(model.edges) do
    print("edge " .. v.node_a .. "." .. v.port_a .. " " .. v.node_b ..".".. v.port_b)
end

output = plot_network {
    model=model,
}

print("Got output")
print(dump(output))

--print(to_json(output, { pretty = true }))

