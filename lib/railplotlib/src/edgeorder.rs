use std::collections::{HashMap, HashSet, BinaryHeap};
use crate::model::*;
use std::hash::Hash;
use log::{trace,debug,warn};

type NodeRef = usize;
type EdgeRef = usize;
type EdgePair = (EdgeRef,EdgeRef);
type NodePort = (NodeRef, Port);
type Edge = (NodePort,NodePort);

pub fn fix_edgeorder_ambiguities(nodes :&[Node], edges :&[Edge], partial_order :&mut HashSet<EdgePair>) {
    while let Err((a,b)) = find_edgeorder_ambiguities(nodes, edges, partial_order) {
        warn!("  *inserting ({},{})", b,a);
        partial_order.insert((b,a));
    }
}

pub fn find_edgeorder_ambiguities(nodes :&[Node], edges :&[Edge], partial_order :&HashSet<EdgePair>) -> Result<(), EdgePair> {
    let partial_order = transitive_closure(partial_order);
    let mut edges = edges.iter().enumerate().collect::<Vec<_>>();
    edges.sort_by_key(|(_i,(start,_end))| -(start.0 as isize));
    let mut active_edges = Vec::new();
    for (n_i,_) in nodes.iter().enumerate() {
        while edges.last().map(|(_i,(start,_end))| start.0) == Some(n_i) {
            active_edges.push(edges.pop().unwrap());
        }
        active_edges.retain(|(_i,(_start,end))| end.0 > n_i);

        debug!("Active edges between {} and {}: {:?}", n_i, n_i+1, active_edges);

        active_edges.sort_by(|(a,_),(b,_)| {
            use std::cmp::Ordering;
            if partial_order.contains(&(*a,*b)) { return Ordering::Less; }
            if partial_order.contains(&(*b,*a)) { return Ordering::Greater; }
            return Ordering::Equal;
        });

        for ((a,_),(b,_)) in active_edges.iter().zip(active_edges.iter().skip(1)) {
            if !partial_order.contains(&(*a,*b)) {
                warn!("Edge order ambiguity {}-{} between nodes {}-{}", a,b,n_i,n_i+1);
                return Err((*a,*b));
            }
        }
    }
    
    Ok(())
}

pub fn edgeorder(nodes :&[Node], edges :&[Edge]) -> HashSet<EdgePair> {
    trace!("computing edge order");
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

pub fn transitive_closure<T: Eq+Hash+Copy+Clone>(set :&HashSet<(T,T)>) -> HashSet<(T,T)> {
    let mut result :HashMap<T, HashSet<T>> = HashMap::new();
    for (a,b) in set { result.entry(*a).or_insert(HashSet::new()).insert(*b); }
    let mut prev_delta = result.clone();
    let mut delta = HashMap::new();
    loop {

        for (a,s) in prev_delta.iter() { 
            for x in s.iter() {
                for b in result.get(x).iter().flat_map(|x| x.iter()) {
                    let exists = result.get(a).map(|a| a.contains(b)).unwrap_or(false);
                    if exists { continue; }

                    delta.entry(*a).or_insert(HashSet::new()).insert(*b);
                }
            }
        }

        if delta.len() == 0 {
            break;
        } else {
            for (a,s) in delta.iter() { 
                for b in s.iter() {
                    result.entry(*a).or_insert(HashSet::new()).insert(*b);
                }
            }
            std::mem::swap(&mut delta, &mut prev_delta);
            delta.clear();
        }
    }

    result.into_iter().flat_map(|(a,s)| s.into_iter().map(move |b| (a,b))).collect()
}

pub fn transitive_reduction<T: Eq+Hash+Copy+Clone>(set :&mut HashSet<(T,T)>) {
    debug!("Computing transitive reduction");
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


