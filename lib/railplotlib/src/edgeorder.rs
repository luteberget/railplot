use std::collections::{HashMap, HashSet};
use crate::model::*;
use std::hash::Hash;
use log::*;

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
pub fn edgeorder(nodes :&[Node], edges :&[Edge]) -> Result<HashSet<(EdgeRef, EdgeRef)>,String> {

    fn node_ports(node :&Node) -> Vec<Port> {
        match &node.shape {
            Shape::Switch(_,_) => vec![Port::Trunk, Port::Left, Port::Right],
            Shape::Crossing => vec![Port::InLeft, Port::InRight, Port::OutLeft, Port::OutRight],
            _ => vec![],
        }
    }

    let port_edge = edges.iter().enumerate().flat_map(|(i,e)| vec![(i,e.0),(i,e.1)])
        .map(|(i,(n,p))| ((n,p),i)).collect::<HashMap<_,_>>();

    let mut lt  :HashSet<(EdgeRef,EdgeRef)> = 
      nodes.iter().enumerate().flat_map(|(ni,n)| match &n.shape {
        Shape::Switch(_,Dir::Up) => vec![(port_edge[&(ni,Port::Right)], port_edge[&(ni,Port::Left)])],
        Shape::Switch(_,Dir::Down) => vec![(port_edge[&(ni,Port::Left)], port_edge[&(ni,Port::Right)])],
        Shape::Crossing => vec![(port_edge[&(ni,Port::InLeft)],  port_edge[&(ni,Port::InRight)]),
                                (port_edge[&(ni,Port::OutRight)],port_edge[&(ni,Port::OutLeft)])],
        _ => vec![],
      }).collect();
    let mut delta = lt.clone();

    let mut new_delta = HashSet::new();
    loop {
        for (a,b) in delta.iter() {
            let dirs :&[(&Edge,&Edge,&dyn Fn(EdgeRef) -> (EdgeRef,EdgeRef))]
                = &[  (&edges[*a],&edges[*b],&|c| (*a,c)), 
                      (&edges[*b],&edges[*a],&|c| (c,*b))  ];
            for (long_edge,node_edge,insert_f) in dirs {
                for (n,_) in &[node_edge.0,node_edge.1] {
                    if !((long_edge.0).0 < *n && *n < (long_edge.1).0) { continue; }
                    for p2 in node_ports(&nodes[*n]) {
                        let new = insert_f(port_edge[&(*n,p2)]);
                        if !lt.contains(&new) {
                            new_delta.insert(new);
                        }
                        if lt.contains(&(new.1,new.0)) {
                            return Err(format!("Inconsistent edge vertical ordering. The input graph may have unmarked crossings."));
                        }
                    } 
                }
            }
        }

        if new_delta.is_empty() { break; }
        lt.extend(new_delta.iter().cloned());
        delta = new_delta;
        new_delta = HashSet::new();
    }
    Ok(lt)
}

/// Compute transitive closure of binary relation.
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

    fn add_node(nodes :&mut Vec<Node>, pos :f64, shape :Shape) -> usize {
        let i = nodes.len();
        nodes.push( Node { name: format!("n{}", i), pos, shape });
        i
    }

    #[test]
    pub fn edgeorder_test1() {
        println!("Two tracks with switch from siding track.  ");
        //
        //            /-3------
        //       /-1-/--4-\
        //  -0--/-------2--\---5------
        //  Would like to discover 5 < 3.
        //
        let mut nodes = Vec::new();
        let n1 = add_node(&mut nodes, 0.0, Shape::Begin);
        let n2 = add_node(&mut nodes, 1.0, Shape::Switch(Side::Left, Dir::Up));
        let n3 = add_node(&mut nodes, 2.0, Shape::Switch(Side::Left, Dir::Up));
        let n4 = add_node(&mut nodes, 3.0, Shape::Switch(Side::Right, Dir::Down));
        let n5 = add_node(&mut nodes, 4.0, Shape::End);
        let n6 = add_node(&mut nodes, 5.0, Shape::End);
        let mut edges = Vec::new();
        edges.push(((n1,Port::Out), (n2,Port::Trunk)));   // #0
        edges.push(((n2,Port::Left), (n3,Port::Trunk)));  // #1
        edges.push(((n2,Port::Right), (n4,Port::Left)));  // #2
        edges.push(((n3,Port::Left), (n5,Port::In)));     // #3
        edges.push(((n3,Port::Right), (n4,Port::Right))); // #4
        edges.push(((n4,Port::Trunk), (n6,Port::In)));    // #5

        let result = edgeorder(&nodes, &edges).unwrap();
        assert!(result.contains(&(2,1)));
        assert!(result.contains(&(2,4)));
        assert!(result.contains(&(4,3)));
        assert!(result.contains(&(5,3)));
    }

    #[test]
    pub fn edgeorder_test2() {
        println!("Junction with opposing switch");
        //
        //  
        //         /---3--d-4-e
        //        /  c-2-/
        //  a-0--b-----1-----------f
        //  Would like to discover 1 < 4.
        //
        let mut nodes = Vec::new();
        let a = add_node(&mut nodes, 0.0, Shape::Begin);
        let b = add_node(&mut nodes, 1.0, Shape::Switch(Side::Left, Dir::Up));
        let c = add_node(&mut nodes, 2.0, Shape::Begin);
        let d = add_node(&mut nodes, 2.0, Shape::Switch(Side::Left, Dir::Down));
        let e = add_node(&mut nodes, 3.0, Shape::End);
        let f = add_node(&mut nodes, 4.0, Shape::End);
        let mut edges = Vec::new();
        edges.push(((a,Port::Out), (b,Port::Trunk))); // #0
        edges.push(((b,Port::Right), (f,Port::In)));  // #1
        edges.push(((c,Port::Out), (d,Port::Left)));  // #2
        edges.push(((b,Port::Left), (d,Port::Right)));// #3
        edges.push(((d,Port::Trunk), (e,Port::In)));  // #4

        let result = edgeorder(&nodes, &edges).unwrap();
        assert!(result.contains(&(1,2)));
        assert!(result.contains(&(2,3)));
        assert!(result.contains(&(1,4)));
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
