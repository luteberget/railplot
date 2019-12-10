use std::collections::{HashMap, HashSet, BinaryHeap};
use crate::model::*;
use std::hash::Hash;
use log::{trace,debug,warn};

type NodeRef = usize;
type EdgeRef = usize;
type EdgePair = (EdgeRef,EdgeRef);
type NodePort = (NodeRef, Port);
type Edge = (NodePort,NodePort);

/// Find pairs of edges which overlap in mileage but are not ordered by the given order relation
/// and add tuples to the relation until no more such pairs can be found.
pub fn fix_edgeorder_ambiguities(nodes :&[Node], edges :&[Edge], partial_order :&mut HashSet<EdgePair>) {
    while let Err((a,b)) = find_edgeorder_ambiguities(nodes, edges, partial_order) {
        warn!("  *inserting ({},{})", b,a);
        partial_order.insert((b,a));
    }
}

/// Find pairs of edges which overlap in mileage but are not ordered by the given order relation.
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

/// Compute the vertical edge order relation.
/// See Sec. 6.2.1 (p. 148).
pub fn edgeorder(nodes :&[Node], edges :&[Edge]) -> Result<HashSet<EdgePair>,String> {
    trace!("computing edge order");
    let mut lt :HashSet<EdgePair> = HashSet::new();

    // (NodeRef,Port) -> EdgeRef
    let port_edge = edges.iter().enumerate().flat_map(|(i,e)| vec![(i,e.0),(i,e.1)])
        .map(|(i,(n,p))| ((n,p),i)).collect::<HashMap<_,_>>();

    let lr = |n:NodeRef, dir:Dir| -> Result<(Vec<EdgeRef>,Vec<EdgeRef>), String> {
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

            (Dir::Up, Shape::Continuation) => vec![port_edge[&(n,Port::Out)]],
            (Dir::Down, Shape::Continuation) => vec![port_edge[&(n,Port::In)]],
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
                        if !over_edges.insert(edge) { continue; }
                        for edge_i in next_edges(edge_node(edge)) {
                            over_nodes.insert(edge_node(edge_i));
                            over_queue.push((dirfactor*edge_node(edge_i) as isize, edge_i));
                        }
                    } else {
                        let (_,edge) = under_queue.pop().unwrap();
                        if !under_edges.insert(edge) { continue; }
                        for edge_i in next_edges(edge_node(edge)) {
                            under_nodes.insert(edge_node(edge_i));
                            under_queue.push((dirfactor*edge_node(edge_i) as isize, edge_i));
                        }
                    }
                }
                _asdf => break
            }
        }

                // left-over edges in the queue are also included
        over_edges.extend(over_queue.into_iter().map(|(_,e)| e));
        under_edges.extend(under_queue.into_iter().map(|(_,e)| e));

        Ok((over_edges.into_iter().collect::<Vec<_>>(),
         under_edges.into_iter().collect::<Vec<_>>()))
    };

    for (i,n) in nodes.iter().enumerate() {
        match &n.shape {
            Shape::Switch(_side,dir) => {
                let (l,r) = lr(i,*dir)?;
                for e_l in &l {
                    for e_r in &r {
                        lt.insert((*e_r,*e_l));
                    }
                }
            },
            _ => {},
        }
    }

    Ok(lt)
}

/// Another try on this function. [WIP]
/// Compute the vertical edge order relation.
/// See Sec. 6.2.1 (p. 148).
pub fn edgeorder2(nodes :&[Node], edges :&[Edge]) -> Result<HashSet<EdgePair>,String> {

    trace!("computing edge order");
    let mut lt :HashSet<EdgePair> = HashSet::new();
    for (i,n) in nodes.iter().enumerate() {
        match &n.shape {
            Shape::Switch(_side,dir) => {
                let (under,over) = edgeorder_switch(nodes, edges, i,*dir)?;
                for e_under in &under {
                    for e_over in &over {
                        // Less-than relation:   e_under < e_over.
                        lt.insert((*e_under,*e_over));
                    }
                }
            },
            _ => {},
        }
    }

    Ok(lt)
}

fn edgeorder_switch(nodes :&[Node], edges :&[Edge], node_idx :NodeRef, dir :Dir) -> Result<(HashSet<EdgeRef>,HashSet<EdgeRef>), String> {
    let over_edge  = edges.iter().position(|e| (e.0) == (node_idx, Port::Left )).unwrap();
    let under_edge = edges.iter().position(|e| (e.0) == (node_idx, Port::Right)).unwrap();

    let mut interval_size = 1;
    let mut over_edges  : HashSet<EdgeRef> = vec![over_edge ].into_iter().collect();
    let mut under_edges : HashSet<EdgeRef> = vec![under_edge].into_iter().collect();

    loop {
        let interval = match dir {
            Dir::Up => (node_idx, node_idx+interval_size),
            Dir::Down => (node_idx-interval_size, node_idx),
        };

        let over_candidates  = find_candidates(nodes, edges, &over_edges,  interval.0, interval.1);
        let under_candidates = find_candidates(nodes, edges, &under_edges, interval.0, interval.1);

        if over_candidates.intersection(&under_candidates).next().is_some() {
            return Ok((under_edges, over_edges));
        } else {
            over_edges = over_candidates;
            under_edges = under_candidates;
            interval_size += 1;
        }
    }
}

fn find_candidates(nodes :&[Node], edges :&[Edge], edge_set :&HashSet<EdgeRef>, left_node :NodeRef, right_node :NodeRef) -> HashSet<EdgeRef> {
    let mut edge_set = edge_set.clone();

    loop {
        let mut found_new = HashSet::new();

        for edge in &edge_set {
            for other_edge in connected_edges(nodes, edges, left_node, right_node, *edge) {
                if !edge_set.contains(&other_edge) {
                    found_new.insert(other_edge);
                }
            }
        }

        if found_new.is_empty() {
            return edge_set;
        } else {
            edge_set.extend(found_new);
        }
    }
}

fn connected_edges(nodes :&[Node], edges :&[Edge], left_node :NodeRef, right_node :NodeRef, edge :EdgeRef) -> Vec<EdgeRef> {
    let n_a = (edges[edge].0).0;
    let n_b = (edges[edge].1).0;

    let mut out = Vec::new();
    out.extend(edges.iter().enumerate().filter(|(_,e)| (e.0).0 == n_a || (e.1).0 == n_a).map(|(i,_)| i));
    out.extend(edges.iter().enumerate().filter(|(_,e)| (e.0).0 == n_b || (e.1).0 == n_b).map(|(i,_)| i)); 
    out
}

/// Compute transitive closure of binary relation in-place.
/// I.e. add tuples (a,c) when there exists (a,b) and (b,c).
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

/// Compute transitive reduction of binary relation in-place.
/// I.e. remove tuples (a,c) when there exists (a,b) and (b,c).
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



#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_transitive_reduction() {
        let mut edges :HashSet<(usize,usize)> = vec![(1,2),(2,3),(1,3)].into_iter().collect();
        transitive_reduction(&mut edges);
        assert!(edges == vec![(1,2),(2,3)].into_iter().collect());
    }

    #[test]
    fn test_transitive_closure() {
        let edges :HashSet<(usize,usize)> = vec![(1,2),(2,3)].into_iter().collect();
        let edges = transitive_closure(&edges);
        assert_eq!(edges, vec![(1,2),(2,3),(1,3)].into_iter().collect());
    }

    #[test]
    pub fn edgeorder_termination_issue() {
        println!("Edge order termiation issue test");
        
        let nodes = vec![
            Node {
                name: format!("start"),
                pos: 0.0,
                shape: Shape::Begin,
            },
            Node {
                name: format!("a"),
                pos: 1.0,
                shape: Shape::Switch(Side::Right, Dir::Down),
            },
            Node {
                name: format!("b"),
                pos: 2.0,
                shape: Shape::Switch(Side::Left, Dir::Up),
            },
            Node {
                name: format!("start"),
                pos: 3.0,
                shape: Shape::End,
            },
        ];

        let edges = vec![
            ((1,Port::Right),(2,Port::Left)),
            ((2,Port::Right),(1,Port::Left)),
            ((0,Port::Out),  (1,Port::Trunk)),
            ((2,Port::Trunk), (3,Port::In)),
        ];

        let result = edgeorder(&nodes, &edges);
        assert!(result.is_ok());
    }

    #[test]
    pub fn test_edge_order_ambiguity() {
        // The edge order relation may not be completely determined from 
        // the track topology only. For example, the "clothes iron" example:
        //       /-----
        //      /   -----\
        // ----/----------\-----------
        // The function find_edgeorder_ambiguities detects these.

        let nodes = vec![
            Node { name: format!("t1start"), pos: 0.0, shape: Shape::Begin },
            Node { name: format!("sw1"), pos: 100.0, shape: Shape::Switch(Side::Left, Dir::Up) },
            Node { name: format!("t3end"), pos: 200.0, shape: Shape::Begin },
            Node { name: format!("t2end"), pos: 300.0, shape: Shape::End },
            Node { name: format!("sw2"), pos: 400.0, shape: Shape::Switch(Side::Right, Dir::Down) },
            Node { name: format!("tlend"), pos: 500.0, shape: Shape::End },
        ];

        let edges = vec![
            /* 0. */ ((0, Port::Out), (1, Port::Trunk)),
            /* 1. */ ((1, Port::Left), (3, Port::In)),
            /* 2. */ ((1, Port::Right), (4, Port::Left)),
            /* 3. */ ((2, Port::Out), (4, Port::Right)),
            /* 4. */ ((4, Port::Trunk), (5, Port::In)),
        ];

        let edge_order = edgeorder(&nodes, &edges).unwrap();
        let result = find_edgeorder_ambiguities(&nodes, &edges, &edge_order);
        println!("result {:?}", result);
        assert!(result == Err((1,3)) || result == Err((3,1)));

        let mut edge_order = edge_order;
        fix_edgeorder_ambiguities(&nodes, &edges, &mut edge_order);
        assert!(edge_order.contains(&(1,3)) || edge_order.contains(&(3,1)));

    }
}
