use std::collections::{HashMap, HashSet, BinaryHeap};
use crate::schematic_graph::*;
use std::hash::Hash;

type NodeRef = usize;
type EdgeRef = usize;
type EdgePair = (EdgeRef,EdgeRef);
type NodePort = (NodeRef, Port);
type Edge = (NodePort,NodePort);

pub fn edgeorder(nodes :&[Node], edges :&[Edge]) -> HashSet<EdgePair> {
    let mut lt :HashSet<EdgePair> = HashSet::new();

    // (NodeRef,Port) -> EdgeRef
    let port_edge = edges.iter().enumerate().flat_map(|(i,e)| vec![(i,e.0),(i,e.1)])
        .map(|(i,(n,p))| ((n,p),i)).collect::<HashMap<_,_>>();

    let lr = |n:NodeRef, dir:Dir| {
        // BinaryHeap is max-heap, so up is negative for higher priority
        let dirfactor = match dir { Dir::Up => -1, Dir::Down => 1 };

        let top_edge = |n:NodeRef| match (dir, &nodes[n].shape) {
            (Dir::Up, Shape::Switch(_sd,Dir::Up)) => port_edge[&(n,Port::Left)],
            (Dir::Down, Shape::Switch(_sd,Dir::Down)) => port_edge[&(n,Port::Right)],
            _ => panic!(),
        };

        let bottom_edge = |n:NodeRef| match (dir, &nodes[n].shape) {
            (Dir::Up, Shape::Switch(_,Dir::Up)) => port_edge[&(n,Port::Right)],
            (Dir::Down, Shape::Switch(_sd,Dir::Down) ) => port_edge[&(n,Port::Left)],
            _ => panic!(),
        };

        let edge_node = |e:EdgeRef| match dir {
            Dir::Up => (edges[e].1).0,
            Dir::Down => (edges[e].0).0,
        };

        let next_edges = |n:NodeRef| match (dir, &nodes[n].shape) {
            (Dir::Up, Shape::End) => vec![],
            (Dir::Up, Shape::Switch(_sd,Dir::Up)) => 
                vec![port_edge[&(n,Port::Left)], port_edge[&(n,Port::Right)]],
            (Dir::Up, Shape::Switch(_sd,Dir::Down)) => vec![port_edge[&(n,Port::Trunk)]],
            (Dir::Down, Shape::Begin) => vec![],
            (Dir::Down, Shape::Switch(_sd,Dir::Down)) => 
                vec![port_edge[&(n,Port::Left)], port_edge[&(n,Port::Right)]],
            (Dir::Down, Shape::Switch(_sd,Dir::Up)) => vec![port_edge[&(n,Port::Trunk)]],
             _ => panic!(),
        };
                let mut over_edges = HashSet::new();
        let mut over_nodes = vec![edge_node(top_edge(n))].into_iter().collect::<HashSet<usize>>();
        let mut over_queue :BinaryHeap<(isize,usize)> = BinaryHeap::new();
        over_queue.push((dirfactor*edge_node(top_edge(n)) as isize, top_edge(n)));

        let mut under_edges = HashSet::new();
        let mut under_nodes = vec![edge_node(bottom_edge(n))].into_iter().collect::<HashSet<usize>>();
        let mut under_queue :BinaryHeap<(isize,usize)>  = BinaryHeap::new();
        under_queue.push((dirfactor*edge_node(bottom_edge(n)) as isize, bottom_edge(n)));

        loop {

            // If the over and under paths have joined, we are done
            if over_nodes.intersection(&under_nodes).next().is_some() {
                break;
            }

            match (over_queue.peek().cloned(), under_queue.peek().cloned()) {
                (Some((over_priority,_)), Some((under_priority,_))) => {
                    if over_priority > under_priority {
                        let (_,edge) = over_queue.pop().unwrap();
                        over_edges.insert(edge);
                        for edge_i in next_edges(edge_node(edge)) {
                            over_nodes.insert(edge_node(edge_i));
                            over_queue.push((dirfactor*edge_node(edge_i) as isize, edge_i));
                        }
                    } else {
                        let (_,edge) = under_queue.pop().unwrap();
                        under_edges.insert(edge);
                        for edge_i in next_edges(edge_node(edge)) {
                            under_nodes.insert(edge_node(edge_i));
                            under_queue.push((dirfactor*edge_node(edge_i) as isize, edge_i));
                        }
                    }
                }
                _ => break
            }
        }

                // left-over edges in the queue are also included
        over_edges.extend(over_queue.into_iter().map(|(_,e)| e));
        under_edges.extend(under_queue.into_iter().map(|(_,e)| e));

        (over_edges.into_iter().collect::<Vec<_>>(),
         under_edges.into_iter().collect::<Vec<_>>())
    };

    for (i,n) in nodes.iter().enumerate() {
        match &n.shape {
            Shape::Switch(_side,dir) => {
                let (l,r) = lr(i,*dir);
                //println!("adding from {:?} {:?} {:?} {:?}", dir, n,l,r);
                for e_l in &l {
                    for e_r in &r {
                        lt.insert((*e_r,*e_l));
                    }
                }
            },
            _ => {},
        }
    }

    lt
}

pub fn transitive_reduction<T: Eq+Hash+Copy+Clone>(set :&mut HashSet<(T,T)>) {
    let map = {
        let mut map = HashMap::new();
        for (a,b) in set.iter() {
            map.entry(*a).or_insert_with(Vec::new).push(*b);
        }
        map
    };

    let orig = set.iter().cloned().collect::<Vec<_>>();
    for (a,b) in orig {
        if !set.contains(&(a,b)) { continue; }
        let mut stack = vec![b];
        while stack.len() > 0 {
            let b = stack.pop().unwrap();
            for c in map.get(&b).unwrap_or(&Vec::new()) {
                stack.push(*c);
                set.remove(&(a,*c));
            }
        }
    }
}

