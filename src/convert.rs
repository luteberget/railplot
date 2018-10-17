// Convert from rolling D-graph to solver input
//

use rolling::input::staticinfrastructure::*;
use solver::SolverInput;
use std::path::Path;
use failure::Error;
use std::collections::VecDeque;
use std::collections::HashSet;
use std::collections::HashMap;
use convert_pos;

type PortRef = (usize,Port);
fn mk_pos(nodes :&[usize], edges: &[(PortRef,PortRef,Vec<(usize,usize,f64)>)], gnode :&GNodeData) -> Result<HashMap<usize, f64>, Error>{
    let mut pos = HashMap::new();
    pos.insert(nodes[0], 0.0); // Anchor first node in list at zero.

    let (up_edges, down_edges) = {
        let mut up_edges :HashMap<usize,Vec<(usize,f64)>> = HashMap::new();
        let mut down_edges :HashMap<usize,Vec<(usize,f64)>> = HashMap::new();
        for ((x,_),(y,_),dists) in edges {
            let d :f64 = dists.iter().map(|(_a,_b,d)| d).sum();
            up_edges  .entry(*x).or_insert_with(Vec::new).push((*y,d));
            down_edges.entry(*y).or_insert_with(Vec::new).push((*x,d));
        }
        (up_edges,down_edges)
    };

    let mut queue = vec![nodes[0]];
    while queue.len() > 0 {
        //println!("pos {:?}",pos);
        let n = queue.pop().unwrap();
        for (up,d) in up_edges.get(&n).unwrap_or(&Vec::new()).iter() {
            if !pos.contains_key(up) {
                let p = pos[&n]+d;
                pos.insert(*up, p);
                queue.push(*up);
            }
        }
        for (down,d) in down_edges.get(&n).unwrap_or(&Vec::new()).iter() {
            if !pos.contains_key(down) {
                let p = pos[&n]-d;
                pos.insert(*down, p);
                queue.push(*down);
            }
        }
    }
    //println!("finished pos");

    // Reset to zero
    use std;
    let mut min = std::f64::INFINITY;
    for (_,&d) in pos.iter() {
        min = f64::min(min,d);
    }
    for (_, d) in pos.iter_mut() {
        *d -= min;
    }

    Ok(pos)
}

pub type OrigEdges = HashMap<((String, Port),(String,Port)),Vec<(String,String,f64)>>;

pub fn convert(x :&StaticInfrastructure) -> Result<(String,OrigEdges), Error> {
    use std::fmt::Write;

    let mut output = String::new();


    println!("CONVERT-ALTERNATIVE D-GRAPH");
    let turns = convert_pos::turn_nodes(&x)?;
    println!("{:?}",turns);
    println!("OK\n\n");
    let mut turnsset :HashSet<usize> = HashSet::new();
    for (a,b) in turns { turnsset.insert(a); turnsset.insert(b); }

    let gnode = gnode(&x,&turnsset)?;
    println!("gnode nodes");
    for (i,n) in gnode.nodes.iter().enumerate() { println!("n{}: {:?}", i, n); }
    let (nodes,edges) = major(&gnode)?;
    let pos = mk_pos(&nodes, &edges, &gnode)?;

    let lookup_names = x.node_names.iter().map(|(k,v)| (*v,k.clone())).collect::<HashMap<usize,String>>();

    for i in nodes  {
        let name = &lookup_names[&i];
        let typ_ = match gnode.nodes[&i] {
            GNode::Begin(_) => "begin",
            GNode::End(_) => "end",
            GNode::Switch(Side::Left, Dir::Up, _, _)    => "outleftsw",
            GNode::Switch(Side::Right, Dir::Up, _, _)   => "outrightsw",
            GNode::Switch(Side::Left, Dir::Down, _, _)  => "inleftsw",
            GNode::Switch(Side::Right, Dir::Down, _, _) => "inrightsw",
            GNode::Turn(_, _,_) => "vertical",
            GNode::Linear(_,_) => panic!("Unexpected node type"),
        };
        writeln!(&mut output, "node {} {} {}", name, typ_, pos[&i]);
    }

    for ((e1,p1),(e2,p2),d) in &edges {
        let p = |p:Port| match p {
            Port::Out => "out",
            Port::In => "in",
            Port::Trunk => "trunk",
            Port::Left => "left",
            Port::Right => "right",
            Port::Top => "top",
            Port::Bottom => "bottom",
            Port::TopBottom => "topbottom",
        };
        writeln!(&mut output, "edge {}.{} {}.{}", 
                 &lookup_names[&e1], p(*p1),
                 &lookup_names[&e2], p(*p2));
    }

    let mut original_edges = HashMap::new();
    for ((e1,p1),(e2,p2),d) in &edges {
        let edge_id = ((lookup_names[&e1].clone(), *p1),
                       (lookup_names[&e2].clone(),*p2));
        let d = d.iter().map(|(a,b,d)| 
                             (lookup_names[&a].clone(), 
                              lookup_names[&b].clone(), *d)).collect::<Vec<_>>();
        original_edges.insert(edge_id,d);
    }

    println!("ORIGINAL EDGES {:?}", original_edges);

    Ok((output, original_edges))
}


use parser::{Dir,Side,Port};
#[derive(Debug)]
enum GNode {
    Begin(usize),
    End(usize),
    Linear(usize,usize),
    Turn(Dir, usize,usize),
    Switch(Side, Dir, usize, (usize,usize)),
}

#[derive(Debug)]
struct GNodeData {
    nodes: HashMap<usize,GNode>,
    dists: HashMap<(usize,usize),f64>,
}

#[derive(Debug, Fail)]
enum InfConvError {
    #[fail(display = "no model boundary found")]
    NoModelBoundary,
}

fn sw_side(x :&SwitchPosition) -> Side {
    match x {
        SwitchPosition::Left => Side::Left,
        SwitchPosition::Right => Side::Right,
    }
}

fn major(g: &GNodeData) -> Result<(Vec<usize>,Vec<((usize,Port),(usize,Port), Vec<(usize,usize,f64)>)>), Error> {

    let mut majornodes = g.nodes.iter().filter_map(|(i,ref n)| {
        if let GNode::Linear(_,_) = *n {
            None
        } else {
            Some(i.clone())
        }
    }).collect::<Vec<_>>();
    println!("Major nodes {:?}", majornodes);


    // Create up edges from each major
    //
    let mut edges = Vec::new();
    let find_in_port = |mut last: usize, mut x:usize| {
        //println!("Find in port: {:?} {:?}", last, x);
        let mut dists = vec![(last,x,g.dists[&(last,x)])];
        while let GNode::Linear(from,to) = g.nodes[&x] {
            last = x;
            x = to;
            dists.push((last,x,g.dists[&(last,x)]));
            //println!("Find in port: {:?} ", dists);
        }
        match g.nodes[&x] {
            GNode::End(_) => (x, Port::In, dists),
            GNode::Switch(side, Dir::Up, _, _) => (x, Port::Trunk, dists),
            GNode::Switch(side, Dir::Down, _, (left, right)) => {
                if last == left { (x, Port::Left, dists) }
                else if last == right { (x, Port::Right, dists) }
                else { panic!("Inconsistent node network.") }
            }
            GNode::Turn(_, a,b) => (x, Port::TopBottom, dists),
            _ => panic!("Inconsistent node network."),
        }
    };
    for &node_i in &majornodes {
        match g.nodes[&node_i] {
            GNode::Begin(x) => {
                let outport = (node_i, Port::Out);
                let inport = find_in_port(node_i, x);
                edges.push(((node_i, Port::Out),find_in_port(node_i, x)));
            },
            GNode::Switch(pos, Dir::Up, trunk, (left,right)) => {
                edges.push(((node_i, Port::Left), find_in_port(node_i, left)));
                edges.push(((node_i, Port::Right), find_in_port(node_i, right)));
            },
            GNode::Switch(pos, Dir::Down, trunk, (left,right)) => {
                edges.push(((node_i, Port::Trunk), find_in_port(node_i, trunk)));
            },
            GNode::Turn(Dir::Down, a, b) => {
                edges.push(((node_i, Port::TopBottom), find_in_port(node_i, a)));
                edges.push(((node_i, Port::TopBottom), find_in_port(node_i, b)));
            }
            _ => {} // These are all the possible up-dir edges from nodes
        }
    }

    majornodes.sort();
    let mut edges = edges.into_iter().map(|(x,(y,z,d))| (x,(y,z),d)).collect::<Vec<_>>();
    edges.sort_by_key(|((x,_),_,_)| *x);
    Ok((majornodes,edges))
}

fn gnode(inf :&StaticInfrastructure, turns :&HashSet<usize>) -> Result<GNodeData, Error> {
    println!("INF {:?}", inf);

    let boundaries = inf.nodes.iter().enumerate().filter_map(|(i, ref n)| {
        if let Edges::ModelBoundary = n.edges { 
            Some(i)
        } else {
            None
        }}).collect::<Vec<_>>();
    let boundary = *boundaries.iter().nth(0).ok_or(InfConvError::NoModelBoundary)?;

    println!("Selected boundary {:?}", boundary);

    let mut nodes = HashMap::new();
    let mut visited = HashSet::new();

    // TODO use an actual queue to do breadth-first and get better vertical node placements?
    // TODO trying this now:
    //let mut queue = vec![boundary];
    let mut queue = VecDeque::new();
    queue.push_back(boundary);

    let mut dists = HashMap::new();

    while queue.len() > 0 {
        let n = queue.pop_front().unwrap();
        visited.insert(n);

        // Add nodes in down direction
        match inf.nodes[n].edges {
            Edges::ModelBoundary | Edges::Nothing => {
                nodes.insert(n, GNode::Begin(inf.nodes[n].other_node));
                dists.insert((n, inf.nodes[n].other_node), 0.0);
            },
            Edges::Single(a,d) => {
                // Down direction a -> n | o
                let opposite_down = inf.nodes[a].other_node;
                dists.insert((n, inf.nodes[n].other_node), 0.0);
                dists.insert((a,n), d);
                if turns.contains(&a) {
                    nodes.insert(n, GNode::Turn(Dir::Down, a, inf.nodes[n].other_node));
                    if !visited.contains(&a) { queue.push_back(a); }
                } else {
                    nodes.insert(n, GNode::Linear(a, inf.nodes[n].other_node));
                    if !visited.contains(&opposite_down) { queue.push_back(opposite_down); }
                }
            },

            Edges::Switchable(obj) => {
                if let StaticObject::Switch { ref left_link, ref right_link, ref branch_side } = inf.objects[obj] {
                    // Switch in down direction == incoming switch == down
                    nodes.insert(n, GNode::Switch(sw_side(branch_side), Dir::Down, inf.nodes[n].other_node,
                                             (left_link.0, right_link.0)));
                    dists.insert((left_link.0, n), left_link.1);
                    dists.insert((right_link.0, n), right_link.1);

                    for ni in &[left_link.0, right_link.0] {
                        let other_ni = inf.nodes[*ni].other_node;
                        if !visited.contains(&other_ni) { queue.push_back(other_ni); }
                    }
                } else {
                    panic!("Not a switch");
                }
            }
        }

        // inspect in up direction
        let upnode = inf.nodes[n].other_node;
        match inf.nodes[upnode].edges {
            Edges::ModelBoundary | Edges::Nothing => {
                nodes.insert(upnode, GNode::End(n));
                dists.insert((n,upnode),0.0);
            },
            Edges::Single(a,d) => {
                nodes.insert(upnode, GNode::Linear(n,a));
                dists.insert((n,upnode), 0.0);
                dists.insert((upnode, a), d);
                if !visited.contains(&a) { queue.push_back(a);}
            },
            Edges::Switchable(obj) => {
                if let StaticObject::Switch { ref left_link, ref right_link, ref branch_side } = inf.objects[obj] {
                    // Switch in up direction == outgoing switch
                    nodes.insert(upnode, GNode::Switch(sw_side(branch_side), Dir::Up, n, 
                                                      (left_link.0, right_link.0)));
                    dists.insert((upnode, left_link.0), left_link.1);
                    dists.insert((upnode, right_link.0), right_link.1);

                    for ni in &[left_link.0, right_link.0] {
                        if !visited.contains(ni) { queue.push_back(*ni); }
                    }
                } else {
                    panic!("Not a switch");
                }
            }
        }
    }

    Ok(GNodeData { nodes, dists })
}
