use serde_json::{json};
use std::collections::{HashMap, HashSet};
use super::schematic_graph::*;
use ordered_float::OrderedFloat;

pub fn branching_to_schematic_graph<Obj>(model :BranchingModel<Obj>) -> Result<SchematicGraph<Obj>, String> {

    // on each track, 
    //  1. sort objects, 
    //  2. create nodes at ends and switches 
    //  3. resolve connections, checking dir consistency

    let BranchingModel { tracks, connections, switch_conns } = model;

    let mut model = SchematicGraph {
        nodes: Vec::new(),
        edges: Vec::new(),
    };

    let mut named_connections = HashMap::new();
    type NodeRef = Result<(String,Port),(String,String)>;
    let mut ref_edges :Vec<((NodeRef,NodeRef),Vec<(Symbol,Obj)>)> = Vec::new();

    for mut t in tracks {
        let mut last_port : NodeRef = match t.begin.1 {
            BrTrackEnd::Stop | BrTrackEnd::Boundary(_) => {
                // generate begin node
                let name = format!("{}_begin", t.name);
                model.nodes.push(Node {
                    name: name.clone(),
                    pos: t.begin.0,
                    shape: Shape::Begin,
                });
                Ok((name,Port::Out))
            },
            BrTrackEnd::Connection((a,b)) => { Err((a,b)) },
        };

        t.objs.sort_by_key(|(p,_)| OrderedFloat(*p));
        let mut edge_objects = Vec::new();
        for o in t.objs {
            match o {
                (_,BrObject::Other(symbol,data)) => { edge_objects.push((symbol,data)); },
                (pos,BrObject::Switch { name, dir, side, conn }) => {
                    // create new node

                    model.nodes.push(Node {
                        name: name.clone(),
                        pos: pos,
                        shape: Shape::Switch(side,dir),
                    });

                    // create new edge
                    let (below_port,above_port,unconnected) = match (dir,side) {
                        (Dir::Up,Side::Left) => (Port::Trunk, Port::Right, Port::Left),
                        (Dir::Up,Side::Right) => (Port::Trunk, Port::Left, Port::Right),
                        (Dir::Down,Side::Left) => (Port::Right, Port::Trunk, Port::Left),
                        (Dir::Down,Side::Right) => (Port::Left, Port::Trunk, Port::Right),
                    };

                    ref_edges.push(((last_port, Ok((name.clone(), below_port)))
                                    ,edge_objects));
                    edge_objects = Vec::new();
                    named_connections.insert(conn,(name.clone(),unconnected));
                    last_port = Ok((name.clone(), above_port));
                },
            }
        }

        match t.end.1 {
            BrTrackEnd::Stop | BrTrackEnd::Boundary(_) => {
                // generate end node
                let name = format!("{}_end", t.name);
                model.nodes.push(Node {
                    name: name.clone(),
                    pos: t.end.0,
                    shape: Shape::Begin,
                });
                ref_edges.push(((last_port, Ok((name.clone(), Port::In))), edge_objects));
            },
            BrTrackEnd::Connection((a,b)) => { 
                ref_edges.push(((last_port, Err((a,b))), edge_objects));
            },
        };
    }

        println!("e {:?}", named_connections);
    for ((a,b),c) in &ref_edges {
        println!("e {:?}", (a,b,c.len()));
    }

    model.edges = ref_edges.into_iter().map(|((a,b),objs)| {
        let begin = a.unwrap_or_else(|(x,y)| named_connections.remove(&(y,x)).unwrap());
        let end =   b.unwrap_or_else(|(x,y)| named_connections.remove(&(y,x)).unwrap());
        Edge { a: begin, b: end, objs: objs }
    }).collect();

    Ok(model)
}


// TODO load_railml lua function
//      1. loads xml document from file
//      2. get_branching_model
//      3. to_schematic_graph
//      4. schematic_to_lua


#[derive(Debug)]
pub struct BranchingModel<Obj> {
    pub tracks:       Vec<BrTrack<Obj>>,
    pub connections:  HashMap<(String,String), BrCursor>,
    pub switch_conns: HashSet<(String,String)>,
}

#[derive(Clone, Debug)]
pub struct BrTrack<Obj> {
    pub name :String,
    pub begin :(f64, BrTrackEnd),
    pub objs :Vec<(f64, BrObject<Obj>)>,
    pub end: (f64, BrTrackEnd),
}

#[derive(Copy, Clone, Debug)]
pub struct BrCursor {
    pub track: usize,
    pub offset: f64,
    pub dir: Dir,
}

#[derive(Clone, Debug)]
pub enum BrTrackEnd {
    Stop,
    Boundary(String),
    Connection((String,String)),
}

#[derive(Clone, Debug)]
pub enum BrObject<Obj> {
    // TODO what about when continuing track is the branching track?
    Switch { name :String, dir :Dir, side: Side, conn: (String,String) }, 
    Other(Symbol, Obj),
}

pub fn railml_to_branching<'a,O,P,Obj>(doc :&'a minidom::Element, ns: &str,
                                   get_objects :O, get_pos: P)
    -> Result<BranchingModel<Obj>, String> 
    where O:Fn(&minidom::Element) -> Result<Vec<(f64, BrObject<Obj>)>, String>,
          P:Fn(&minidom::Element) -> Result<f64, String>,
{

    // Convert railML from XML into a similar structure that we
    // call "branching model", meaning that tracks can have branches (switches
    // or crossings) in the middle. Contrast this to more graph-like models
    // such as railtopomodel (linear/non-branching track sections connected by relations)
    // or the doppelpunktgraph (double/twin nodes giving local directionality,
    // and no objects between nodes)

    let mut model = BranchingModel {
        tracks: Vec::new(),
        connections: HashMap::new(),
        switch_conns: HashSet::new(),
    };

    let infrastructure = {
        if doc.name().to_lowercase() == "infrastructure" {
            doc
        } else if doc.name().to_lowercase() == "railml" {
            doc.children()
                .filter(|x| x.name().to_lowercase() == "infrastructure")
                .nth(0)
                .ok_or(format!("No infrastructure element found"))?
        } else {
            return Err(format!("No infrastructure element found"));
        }
    };

    let tracks = infrastructure.children()
        .filter(|x| x.name().to_lowercase() == "tracks")
        .nth(0)
        .ok_or(format!("No tracks found in the infrastructure"))?;

    for track in tracks.children().filter(|x| x.name().to_lowercase() == "track") {

        let name = track.attr("name").or_else(|| track.attr("id"))
            .ok_or(format!("Track is missing id and name."))?;

        //println!("track {}", name);
        let track_idx = model.tracks.len();
        let topology = track.get_child("trackTopology", ns)
            .ok_or(format!("No trackTopology element in track {:?}", name))?;

        let track_begin = topology.get_child("trackBegin", ns)
            .ok_or(format!("No trackBegin found on track {:?}.", name))?;
        let track_end = topology.get_child("trackEnd", ns)
            .ok_or(format!("No trackEnd found on track {:?}.", name))?;

        // try to get a position on track_begin, track_end or their children
        let track_begin_pos = get_pos(track_begin)?;
        let track_end_pos = get_pos(track_end)?;

        let (begin,end) = {
            let mut conv_end = |n:&minidom::Element,offset,dir| {
                if let Some(conn) = n.get_child("connection", ns) {
                    let id = conn.attr("id").ok_or(format!("Connection has no id.")).unwrap();
                    let ref_ = conn.attr("ref").ok_or(format!("Connection has no ref.")).unwrap();
                    model.connections.insert((ref_.to_string(),id.to_string()),
                                             BrCursor { track: track_idx, offset, dir });
                    BrTrackEnd::Connection((id.to_string(),ref_.to_string()))
                } else if let Some(e) = n.get_child("openEnd", ns) {
                    let name = e.attr("id").ok_or(format!("openEnd has no id.")).unwrap();
                    BrTrackEnd::Boundary(name.to_string())
                } else {
                    BrTrackEnd::Stop
                }
            };
            let begin = conv_end(track_begin, track_begin_pos, Dir::Up);
            let end   = conv_end(track_end,   track_end_pos,   Dir::Down);
            (begin,end)
        };

        let mut objs = get_objects(&track)?;

        add_switches(&mut objs, &mut model.connections, &mut model.switch_conns,
                     &track, track_idx, ns, &get_pos)?;

        model.tracks.push(BrTrack { 
            name: name.to_string(),
            begin: (track_begin_pos, begin), 
            objs: objs,
            end: (track_end_pos, end),
        });
    }

    Ok(model)
}

fn add_switches<Obj,P>(vec :&mut Vec<(f64, BrObject<Obj>)>, connections :&mut HashMap<(String,String), BrCursor>, switch_conns :&mut HashSet<(String,String)>, track :&minidom::Element, track_idx: usize, ns :&str, get_pos:&P) -> Result<(), String>
    where P:Fn(&minidom::Element) -> Result<f64, String>,
{
    let switches = track.get_child("trackTopology",ns)
        .and_then(|o| o.get_child("connections", ns))
        .map(|c| {
            c.children()
                .filter(|x| x.name() == "switch" /*|| TODO x.name() == "crossing"*/)
                .collect::<Vec<_>>()
        })
        .unwrap_or_else(|| Vec::new());


    for sw in &switches {
        let id = sw.attr("id").expect("switch id missing");
        let name = id.to_string();
        //let pos = sw.attr("pos").expect("switch pos missing")
        //.parse::<f64>().expect("switch pos is not a valid number");
        let pos = get_pos(sw)?;

        let conns = sw.children().filter(|x| x.name() == "connection").collect::<Vec<_>>();
        if conns.len() == 0 {
            return Err(format!("No connection in switch {:?}", name));
        }

        if conns.len() > 1 {
            return Err(format!("Multiple connections in switch {:?}", name));
        }

        let conn = conns[0];
        let dir = match conn.attr("orientation") {
            Some("outgoing") => Dir::Up,
            Some("incoming") => Dir::Down,
            _ => return Err(format!("Switch orientation")),
        };

        let side = match conn.attr("course") {
            Some("left") => Side::Left,
            Some("right") => Side::Right,
            _ => return Err(format!("Switch course")),
        };

                let conn_name = match (conn.attr("id"), conn.attr("ref")) {
            (Some(id),Some(ref_)) => (id.to_string(), ref_.to_string()),
            _ => return Err(format!("Switch connection ids missing")),
        };

        connections.insert((conn_name.1.clone(), conn_name.0.clone()),
                           BrCursor { track: track_idx, offset: pos, dir: dir.opposite() });
        switch_conns.insert((conn_name.0.clone(), conn_name.1.clone()));
        switch_conns.insert((conn_name.1.clone(), conn_name.0.clone()));

        //vec.push(BrObject { name, pos, data: BrObjectData::Switch { dir, side, conn: conn_name }});
        vec.push((pos, BrObject::Switch { name, dir, side, conn: conn_name }));
    }
    Ok(())
}

