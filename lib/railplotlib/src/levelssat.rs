use crate::model::*;
use crate::solvers::Goal;
use diffsolver::*;
use diffsolver::minisat::Bool;
use diffsolver::minisat::unary::*;
use diffsolver::minisat::symbolic::*;
use ordered_float::OrderedFloat;
use log::{info,debug,trace,warn};
use std::collections::{HashMap, HashSet};

#[derive(Debug)]
pub struct Edge {
    pub a: NodePort,
    pub b: NodePort,
}
#[derive(Debug)]
pub struct NodePort {
    pub node :NodeRef,
    pub port :Port,
}

type EdgeRef = usize;
type NodeRef = usize;
type EdgePair = (EdgeRef, EdgeRef);

pub type Pt = (f64,f64);
pub struct Output {
    pub node_coords: Vec<Pt>,
    pub edge_levels: Vec<f64>,
    pub symbol_xs: Vec<f64>,
}

pub struct Node {
    pub shape :Shape,
    pub pos :f64,
}

pub fn idx_between<T>(slice :&[T], r :Result<usize,usize>) -> (usize,usize) {
    let r = match r {
        Ok(i) => i-1,
        Err(i) => i-1,
    };
    let r = if r <= 0 { 0 } else { if r < slice.len()-1 { r } else { slice.len()-2 } };
    (r,r+1)
}

pub fn solve(nodes :&[Node], edges :&[Edge], symbols:&[(EdgeRef,&Symbol)], edges_lt :&[EdgePair], main_tracks :&[EdgeRef], goals :&[Goal], nodes_distinct: bool) -> Result<Output, String> {
    info!("Solving node/edge/symbols model using Levels/SAT method.");

    if nodes.len() == 0 { return Err(format!("No nodes specified.")); }
    if edges.len() == 0 { return Err(format!("No edges specified.")); }

    let mut s :SATModDiff<isize> = SATModDiff::new();

    let node_delta_xs = nodes.iter().zip(nodes.iter().skip(1))
        .map(|_| Unary::new(&mut s.sat,2)).collect::<Vec<_>>();

    if nodes_distinct {
        for n in &node_delta_xs {
            s.sat.add_clause(vec![n.gte_const(1)]);
        }
    }

    let node_ys = nodes.iter().enumerate()
        .map(|(i,_)| s.diff.named_var(Some(format!("ny_{}",i)))).collect::<Vec<_>>();
    let edge_ys = edges.iter().enumerate()
        .map(|(i,_)| s.diff.named_var(Some(format!("ey_{}",i)))).collect::<Vec<_>>();
    let edge_short = edges.iter()
        .map(|_| (s.sat.new_lit(), s.sat.new_lit())).collect::<Vec<_>>();
    let slanted = nodes.iter()
        .map(|x| if let Shape::Switch(_,_) | Shape::Crossing = x.shape { s.sat.new_lit() }
             else { Bool::Const(false) } )
        .collect::<Vec<_>>();
    let continuations = nodes.iter()
        .map(|x| if let Shape::Continuation = x.shape { 
            Some(Symbolic::new(&mut s.sat, vec![EdgeDir::Down, EdgeDir::Straight, EdgeDir::Up]))
        } else { None }).collect::<Vec<_>>();


    trace!("($) adding constraints");
    // constraint: edges push nodes apart:  sum(delta_x_a .. delta_x_b) >= 1
    for Edge { a,b } in edges {
        s.sat.add_clause(((a.node) .. (b.node)) .map(|x| node_delta_xs[x].gte_const(1)));
    }

    debug!("Edges LT {:?}", edges_lt);
    // constraint: edge ordering
    for (a,b) in edges_lt {
        let ((a_short_up,a_short_down),_) = (edge_short[*a],edge_short[*b]);
        let (ya,yb) = (edge_ys[*a],edge_ys[*b]);
        let c1 = s.cond_constraint(ya,yb, 0);
        let c2 = s.cond_constraint(ya,yb,-1);
        s.sat.add_clause(vec![c1]);
        s.sat.add_clause(vec![a_short_up, a_short_down, c2]);
    }

    //// constraints: for each edge
    let mut edge_shapes = Vec:: new();
    for (i,e) in edges.iter().enumerate() {
        trace!("edge {:?}", e);
        let (short_up, short_down) = edge_short[i];
        let e_begin = mk_port_shape(EdgeSide::Begin, &nodes[e.a.node].shape,
                                    e.a.port, slanted[e.a.node], &continuations[e.a.node]);
        let e_end  = mk_port_shape(EdgeSide::End, &nodes[e.b.node].shape,
                                   e.b.port, slanted[e.b.node], &continuations[e.b.node]);

        // Short up/down requires both edge side in same slant
        s.sat.add_clause(vec![!short_up, e_begin.up]);
        s.sat.add_clause(vec![!short_up, e_end.up]);
        s.sat.add_clause(vec![!short_down, e_begin.down]);
        s.sat.add_clause(vec![!short_down, e_end.down]);

        // going straight
        let straight_begin_1 = s.cond_constraint(node_ys[e.a.node], edge_ys[i], 0);
        let straight_begin_2 = s.cond_constraint(edge_ys[i], node_ys[e.a.node], 0);
        let straight_end_1 = s.cond_constraint(node_ys[e.b.node], edge_ys[i], 0);
        let straight_end_2 = s.cond_constraint(edge_ys[i], node_ys[e.b.node], 0);
        s.sat.add_clause(vec![!e_begin.straight, straight_begin_1]);
        s.sat.add_clause(vec![!e_begin.straight, straight_begin_2]);
        s.sat.add_clause(vec![!e_end.straight, straight_end_1]);
        s.sat.add_clause(vec![!e_end.straight, straight_end_2]);

        // going down
        let down_begin_short = s.cond_constraint(edge_ys[i], node_ys[e.a.node], 0);
        let down_begin       = s.cond_constraint(edge_ys[i], node_ys[e.a.node], -1);
        let down_end         = s.cond_constraint(node_ys[e.b.node], edge_ys[i], -1);
        s.sat.add_clause(vec![!e_begin.down, short_down, down_begin]);
        s.sat.add_clause(vec![!e_begin.down, down_begin_short]);
        s.sat.add_clause(vec![!e_end.down  , down_end]);

        // going up
        let up_begin         = s.cond_constraint(node_ys[e.a.node], edge_ys[i], -1);
        let up_end           = s.cond_constraint(edge_ys[i], node_ys[e.b.node], -1);
        let up_end_short     = s.cond_constraint(edge_ys[i], node_ys[e.b.node], 0);
        s.sat.add_clause(vec![!e_begin.up, up_begin]);
        s.sat.add_clause(vec![!e_end.up,   short_up, up_end]);
        s.sat.add_clause(vec![!e_end.up,   up_end_short]);

        // short up => y_b - y_a < 1
        // node_b must be less than 1.0 above node_a.  node_b - node_a <= 1.0
        let short_up_y1 = s.cond_constraint(node_ys[e.b.node], node_ys[e.a.node], 1);
        s.sat.add_clause(vec![!short_up, short_up_y1]);
        //
        // short up => y_b - y_a <= 1  ++  y_a - y_b <= -1
        // node_a must be less than 1.0 above node_b
        let short_dn_y1 = s.cond_constraint(node_ys[e.a.node], node_ys[e.b.node], 1);
        s.sat.add_clause(vec![!short_down, short_dn_y1]);

        let both_straight = s.sat.and_literal(vec![e_begin.straight, e_end.straight]);
        // push X values apart
        trace!("sum unary {} {} {}", node_delta_xs.len(), e.a.node, e.b.node);
        let dx = Unary::sum_truncate(&mut s.sat, node_delta_xs[(e.a.node)..(e.b.node)].iter().cloned().collect(), 2);
        s.sat.add_clause(vec![!short_up,            dx. lt_const(2)]);
        s.sat.add_clause(vec![!short_down,          dx. lt_const(2)]);
        s.sat.add_clause(vec![short_down, short_up, both_straight, dx.gte_const(2)]);

        // X=1 requires either short-edge or straight
        //s.sat.add_clause(vec![dx.lte_const(0), !dx.lte_const(1), short_down, short_up, both_straight]);
        //(same as above)

        let all_dx_lte1 = !s.sat.or_literal(
            node_delta_xs[(e.a.node)..(e.b.node)].iter().map(|x| x.gt_const(1)));
        // all dx <= 1 on the edge, so there is no stretching freedom
        // to enusure that later constraints will work on the X axis after Y
        // values have been fixed, we need at least one "stretchable" element.
        s.sat.add_clause(vec![!dx.gte_const(2), !all_dx_lte1]);

        edge_shapes.push((e_begin,e_end));
    }

    // Create a set of X values in the difference logic, 
    // scaled by a factor to allow fractional numbers,
    // independent of Y values, related only through
    // the dx unary numbers.

    let symbol_node_xs = nodes.iter().enumerate()
        .map(|(i,_)| s.diff.named_var(Some(format!("snx_{}",i)))).collect::<Vec<_>>();
    let symbol_xs = symbols.iter().enumerate()
        .map(|(i,_)| s.diff.named_var(Some(format!("s_x_{}",i)))).collect::<Vec<_>>();

    let symbol_factor = 10000isize;
    //
    // symbol node x deltas
    for (i,dx) in node_delta_xs.iter().enumerate() {
        //let dx0a = s.cond_constraint(symbol_node_xs[i], symbol_node_xs[i+1], 0);
        //s.sat.add_clause(vec![!dx.lt_const(0),dx0a]);

        let dx0b = s.cond_constraint(symbol_node_xs[i+1], symbol_node_xs[i], 0);
        let dx1a = s.cond_constraint(symbol_node_xs[i], symbol_node_xs[i+1], -1*symbol_factor);
        let dx1b = s.cond_constraint(symbol_node_xs[i+1], symbol_node_xs[i], 1*symbol_factor);
        s.sat.add_clause(vec![!dx.lte_const(0),dx0b]);
        s.sat.add_clause(vec![dx.lte_const(0), !dx.lte_const(1), dx1a]);
        s.sat.add_clause(vec![dx.lte_const(0), !dx.lte_const(1), dx1b]);
    }


    //
    // bug fix idea: 
    //  if you have a begin/end, its immediate y neighbors need to have a distance constraint.
    //
    // if track end AND y-neighbor to an edge ending in a switch on the left/right.
    //
    // if track begin AND y-neighbor to an edge starting in a switch on the left/right.
    //
    // let (dx,dy) be the difference between switch node and begin/end node.
    // then for every bound b: (dy < b) OR (dx > b+1)
    //

    for (ea,eb) in edges_lt {
        // case 1: a starts in a start node, b starts in switch left/right.
        // case 2: a ends in an end ndoe, b ends in switch left/right.
        // case 3: b start in a start node, a starts in switch left/right.
        // case 3: b ends in an end node, a ends in switch left/right.
        let (short_up_lo, short_down_lo) = edge_short[*ea];
        let (short_up_hi, short_down_hi) = edge_short[*eb];
        let (lo,hi) = (&edges[*ea], &edges[*eb]);

        if let Port::Out = lo.a.port {
            if let Port::Left | Port::Right = hi.a.port {
                debug!("bug1 CASE1 {} {}", ea, eb);

                // CASE 1

                for bound in 0..10 {

                    // lo_y - hi_y <= b
                    // dy <= b.
                    let dyc = s.cond_constraint(
                        node_ys[lo.a.node],
                        node_ys[hi.a.node], bound);

                    // dx >= b+1.
                    // lo_x - hi_x >= b+1
                    // hi_x - lo_x <= -(b+1)
                    let dxc = s.cond_constraint(
                        symbol_node_xs[hi.a.node],
                        symbol_node_xs[lo.a.node],
                        -(2 + bound)*symbol_factor);

                    let e_begin = mk_port_shape(EdgeSide::Begin, &nodes[hi.a.node].shape, 
                                                hi.a.port, slanted[hi.a.node], &continuations[hi.a.node]);

                    s.sat.add_clause(vec![short_up_hi, !e_begin.up, dyc, dxc]);
                }

            }
        } 
        if let Port::In = lo.b.port {
            if let Port::Left | Port::Right = hi.b.port {
                debug!("bug1 CASE2 {} {}", ea, eb);
                // CASE 2


                for bound in 0..10 {

                    // lo_y - hi_y <= b
                    // dy <= b.
                    let dyc = s.cond_constraint(
                        node_ys[lo.b.node],
                        node_ys[hi.b.node], bound);

                    // dx >= b+1.
                    // hi_x - lo_x >= b+1
                    // lo_x - hi_x <= -(b+1)
                    let dxc = s.cond_constraint(
                        symbol_node_xs[lo.b.node],
                        symbol_node_xs[hi.b.node],
                        -(2 + bound)*symbol_factor);

                    let e_end = mk_port_shape(EdgeSide::End, &nodes[hi.b.node].shape, 
                                              hi.b.port, slanted[hi.b.node], &continuations[hi.b.node]);

                    s.sat.add_clause(vec![short_down_hi, !e_end.down, dyc, dxc]);
                }

            }
        } 
        if let Port::Out = hi.a.port {
            if let Port::Left | Port::Right = lo.a.port {
                debug!("bug1 CASE3 {} {}", ea, eb);

                for bound in 0..10 {

                    // lo_y - hi_y <= b
                    // dy <= b.
                    let dyc = s.cond_constraint(
                        node_ys[lo.a.node],
                        node_ys[hi.a.node], bound);

                    // dx >= b+1.
                    // lo_x - hi_x >= b+1
                    // hi_x - lo_x <= -(b+1)
                    let dxc = s.cond_constraint(
                        symbol_node_xs[lo.a.node],
                        symbol_node_xs[hi.a.node],
                        -(2 + bound)*symbol_factor);

                    let e_begin = mk_port_shape(EdgeSide::Begin, &nodes[lo.a.node].shape, 
                                                lo.a.port, slanted[lo.a.node], &continuations[lo.a.node]);

                    s.sat.add_clause(vec![short_down_lo, !e_begin.down, dyc, dxc]);
                }

            }
        } 
        if let Port::In = hi.b.port {
            if let Port::Left | Port::Right = lo.b.port {
                debug!("bug1 CASE4 {} {}", ea, eb);

                for bound in 0..10 {

                    // lo_y - hi_y <= b
                    // dy <= b.
                    let dyc = s.cond_constraint(
                        node_ys[lo.b.node],
                        node_ys[hi.b.node], bound);

                    // dx >= b+1.
                    // lo_x - hi_x >= b+1
                    // hi_x - lo_x <= -(b+1)
                    let dxc = s.cond_constraint(
                        symbol_node_xs[hi.b.node],
                        symbol_node_xs[lo.b.node],
                        -(2 + bound)*symbol_factor);

                    let e_end = mk_port_shape(EdgeSide::End, &nodes[lo.b.node].shape, 
                                                lo.b.port, slanted[lo.b.node], &continuations[lo.b.node]);

                    s.sat.add_clause(vec![short_up_lo, !e_end.up, dyc, dxc]);
                }

            }
        }
    }


    // if we have two edges neighboring in/out with overlapping km, then 
    // they should have xdiff >= 1.
    for (a,b) in edges_lt.iter().cloned().chain(edges_lt.iter().map(|(a,b)| (*b,*a))) {
        if let Port::Out = edges[a].a.port {
            if let Port::In = edges[b].b.port {
                if nodes[edges[b].b.node].pos > nodes[edges[a].a.node].pos {
                    s.sat.add_clause(((edges[a].a.node) .. (edges[b].b.node)) 
                                     .map(|x| node_delta_xs[x].gte_const(1)));
                }
            }
        }
    }


    // (c1) global order
    {
        let mut v = Vec::new();
        for ((_ei,s),var) in symbols.iter().zip(symbol_xs.iter()) {
            v.push((s.pos, *var));
        }
        for (n,x) in nodes.iter().zip(symbol_node_xs.iter()) {
            v.push((n.pos, *x));
        }

        v.sort_by_key(|(p,_)| OrderedFloat(*p));
        for ((_,a),(_,b)) in v.iter().zip(v.iter().skip(1)) {
            let c = s.cond_constraint(*a,*b,0);
            s.sat.add_clause(vec![c]);
        }
    }

    // (c2) edge-class order and bounds
    for (a,b) in symbol_edgeclass_constraints(&nodes, &edges, &symbols, 
                                              &symbol_node_xs, &symbol_xs, symbol_factor) {
        debug!("Constraint {:?}", (a,b));
        match (a,b) {
            // So, we have x+a <= y+b, and we want  z - w < -m,    z+m<w
            //  x-y<=b-a
            (DiffOffset(Some(v1),d1),DiffOffset(Some(v2),d2)) => {
                let c = s.cond_constraint(v1,v2,d2-d1);
                s.sat.add_clause(vec![c]);
            },
            _ => { unimplemented!(); }
        }

    }

    debug!("($) starting solve ({} dvars, {} dclauses)", s.diff.num_vars(), s.diff.num_constraints());

    {
        let _model = s.solve().map_err(|_| "Unconstrained solve failed".to_string())?;
    }


    for goal in goals {
        match goal {
            Goal::Bends  => { goal_bends(&mut s, &edge_shapes)?; },
            Goal::LocalY  => { goal_local_ydiff(&mut s, edges, &edges_lt, &edge_ys)?; },
            Goal::LocalX  => { goal_local_xdiffs(&mut s, &node_delta_xs, &nodes)?; },
            Goal::Width  => { goal_width(&mut s, &node_delta_xs)?; },
            Goal::Height => { goal_height(&mut s, &edge_ys, &edges_lt)?; },
            Goal::Diagonals => { goal_diagonals(&mut s, &edge_shapes)?; },
            Goal::Nodeshapes => { goal_nodeshapes(&mut s, &slanted)?; },
            Goal::Shortedges => { goal_shortedges(&mut s, &edge_short)?; },
            Goal::MainTrackHeight => { goal_maintrackheight(&mut s, &edge_ys, &edges_lt, &main_tracks)?; }
            //_ => unimplemented!(), --- unreachable pattern for now, activate if new
            //  criteria are added.
        }
    }

    // After optimization, we extract the node_dx, ndoe_y, edge_y values
    // and use them add constraints to symbol_node_xs.

    let (node_dx_values, node_y_values, edge_y_values, edge_short_values) = {
        let m = s.solve().unwrap();
        let node_dx_values = node_delta_xs.iter()
            .map(|x| m.sat.value(x)).collect::<Vec<_>>();
        let node_y_values = node_ys.iter()
            .map(|x| m.diff.get_value(*x)).collect::<Vec<_>>();
        let edge_y_values = edge_ys.iter()
            .map(|x| m.diff.get_value(*x)).collect::<Vec<_>>();
        let edge_shapes = edge_shapes.iter().map(|(b,e)| {
            let begin = if m.sat.value(&b.straight) { EdgeDir::Straight }
                        else if m.sat.value(&b.up) { EdgeDir::Up }
                        else if m.sat.value(&b.down) { EdgeDir::Down }
                        else { panic!() };
            let end = if m.sat.value(&e.straight) { EdgeDir::Straight }
                        else if m.sat.value(&e.up) { EdgeDir::Up }
                        else if m.sat.value(&e.down) { EdgeDir::Down }
                        else { panic!() };
            (begin,end)
        }).collect::<Vec<_>>();
        let edge_short_values = edge_short.iter().map(|x| m.sat.value(&x.0) || m.sat.value(&x.1)).collect::<Vec<_>>();
        debug!("Node dx {:?}", node_dx_values);
        debug!("Edge shapes {:?}", edge_shapes);
        let sxs = symbol_node_xs.iter().map(|v| m.diff.get_value(*v)).collect::<Vec<_>>();
        debug!("Symbol Node Xs {:?}", sxs);
        debug!("Symbol min {:?}", sxs.iter().min());

        (node_dx_values,node_y_values,edge_y_values, edge_short_values)
    };

    // Minimum DX values.
    for (i,dx) in node_dx_values.iter().enumerate() {
        let c = s.cond_constraint(symbol_node_xs[i], symbol_node_xs[i+1],
                              -(*dx as isize)*symbol_factor);
        s.sat.add_clause(vec![c]);
    }
    // Edge X lengths
    for (i,e) in edges.iter().enumerate() {
        let x1 = symbol_node_xs[e.a.node];
        let x2 = symbol_node_xs[e.b.node];
        let dy1 = node_y_values[e.a.node] - edge_y_values[i];
        let dy2 = edge_y_values[i] - node_y_values[e.b.node];
        let samedir = dy1.signum() == dy2.signum() && dy1.signum() != 0;
        let short = edge_short_values[i];
        let dist = dy1.abs() + dy2.abs() + if samedir || short { 0 } else { 1 };
        debug!("Edge {:?} dist{} dy{} dy{} {:?} {:?} {}", e,dist,dy1,dy2,x1,x2,-(dist*symbol_factor));

        let c = s.cond_constraint(x1,x2,-(dist*symbol_factor));
        s.sat.add_clause(vec![c]);
    }

    let node_x_values = {
        let m = s.solve().map_err(|_| format!("Unable to solve."))?;
        symbol_node_xs.iter().map(|v| m.diff.get_value(*v)).collect::<Vec<_>>()
    };

    let node_abspos_values = nodes.iter().enumerate()
        .map(|(i,n)| (OrderedFloat(n.pos), node_x_values[i])).collect::<Vec<_>>();
    debug!("node abspos values {:?}", node_abspos_values);

    // TODO the optimization of symbol x values to be close to their proportional
    //   location can be done locally for each edge, which might be faster, and
    //   the whole optimization can be skipped if the edge is "saturated" with symbols,
    //   i.e. there is no room to move.
    let symbol_x_values = {
        let mut goal = HashMap::new();
        for (i,(_,s)) in symbols.iter().enumerate() {
            let node_idx = node_abspos_values.binary_search_by_key(&OrderedFloat(s.pos), |&(p,_)| p);
            let (before,after) = idx_between(&node_abspos_values, node_idx);
            let ((pos_before,x_before),(pos_after,x_after)) = 
            (node_abspos_values[before],node_abspos_values[after]);
            let (pos_before,pos_after) = (pos_before.into_inner(),pos_after.into_inner());
            let prop = if pos_after - pos_before > 1e-5 {
                (s.pos - pos_before)/(pos_after - pos_before)
            } else { 0.0 };
            let x = (x_before as f64 + prop*(x_after as f64 -x_before as f64)).round() as isize;
            trace!("Symbol: {:?} {} {} {} {} {} {}", s,pos_before,pos_after,prop,
                     x_before,x_after,x);
            goal.insert(symbol_xs[i], x);
        }
        let optimized_symbol_x_values = s.diff.optimize(goal);
        match optimized_symbol_x_values {
            Ok(values) => {
                info!("Successfully optimized symbol proportional positions.");
                let mut vec = vec![0; symbols.len()];
                let idxs = symbols.iter().enumerate()
                    .map(|(i,_)| (symbol_xs[i], i)).collect::<HashMap<_,_>>();
                for (k,v) in values {
                    vec[idxs[&k]] = v;
                }
                vec
            },
            Err(msg) => {
                warn!("Could not optimize symbol x values: {}", msg);
                let m = s.solve().map_err(|_| format!("Unable to solve."))?;
                symbol_xs.iter().map(|v| m.diff.get_value(*v)).collect::<Vec<_>>()
            }
        }
    };

    goal_symbol_width(&mut s, symbol_node_xs[0], symbol_node_xs[symbol_node_xs.len()-1], symbol_factor)?;

    let (node_x_values,symbol_x_values) = {
        let m = s.solve().map_err(|_| format!("Unable to solve."))?;
        let nx = symbol_node_xs.iter().map(|v| m.diff.get_value(*v)).collect::<Vec<_>>();
        let sx = symbol_xs.iter().map(|v| m.diff.get_value(*v)).collect::<Vec<_>>();
        (nx,sx)
    };

    let x_min = *node_x_values.iter().min().unwrap();
    let node_x_values :Vec<isize> = node_x_values.into_iter().map(|x| x - x_min).collect();
    let symbol_x_values :Vec<isize> = symbol_x_values.into_iter().map(|x| x - x_min).collect();

    Ok(Output {
        node_coords: node_x_values.into_iter().zip(node_y_values.into_iter())
            .map(|(x,y)| ((x as f64)/(symbol_factor as f64), y as f64)).collect(),
        edge_levels: edge_y_values.into_iter()
            .map(|l| l as f64).collect(),
        symbol_xs: symbol_x_values.into_iter()
            .map(|x| (x as f64)/(symbol_factor as f64)).collect(),
    })
}



struct PortShape {
    up:Bool,
    straight:Bool,
    down:Bool,
}

fn portsh(up :Bool, straight :Bool, down :Bool) -> PortShape {
    PortShape { up, straight, down }
}

#[derive(Debug,Copy,Clone)]
pub enum EdgeSide { Begin, End }

fn mk_port_shape(side :EdgeSide, shape :&Shape, port :Port, s :Bool, cont :&Option<Symbolic<EdgeDir>>) -> PortShape {

    use self::EdgeSide as ES;
    use self::Shape as N;
    use self::Port as P;
    use self::Side as S;
    use self::Dir as D;
    let ff = Bool::Const(false);
    let tt = Bool::Const(true);
    match (side,shape,port) {
        (ES::Begin, N::Begin, P::Out) => portsh(ff,tt,ff),
        (ES::End,   N::End  , P::In ) => portsh(ff,tt,ff),

        (ES::Begin, N::Switch(S::Left, D::Up  ),   P::Left)  => portsh(!s,  s, ff),
        (ES::Begin, N::Switch(S::Left, D::Up  ),   P::Right) => portsh(ff, !s,  s),
        (ES::Begin, N::Switch(S::Right,D::Up  ),   P::Left)  => portsh(s,  !s, ff),
        (ES::Begin, N::Switch(S::Right,D::Up  ),   P::Right) => portsh(ff,  s, !s),
        (ES::Begin, N::Switch(S::Left, D::Down), P::Trunk) => portsh(ff, !s,  s),
        (ES::Begin, N::Switch(S::Right,D::Down), P::Trunk) => portsh(s,  !s, ff),

        (ES::End,   N::Switch(S::Left, D::Down), P::Left)  => portsh(!s,  s, ff),
        (ES::End,   N::Switch(S::Left, D::Down), P::Right) => portsh(ff, !s,  s),
        (ES::End,   N::Switch(S::Right,D::Down), P::Left)  => portsh( s, !s, ff),
        (ES::End,   N::Switch(S::Right,D::Down), P::Right) => portsh(ff,  s, !s),
        (ES::End,   N::Switch(S::Left, D::Up  ),   P::Trunk) => portsh(ff, !s,  s),
        (ES::End,   N::Switch(S::Right,D::Up  ),   P::Trunk) => portsh( s, !s, ff),

        (ES::Begin, N::Crossing, P::OutLeft) =>  portsh(!s, s, ff),
        (ES::Begin, N::Crossing, P::OutRight) => portsh(ff, !s, s),
        (ES::End,   N::Crossing, P::InLeft) =>  portsh(!s, s, ff),
        (ES::End,   N::Crossing, P::InRight) => portsh(ff, !s, s),

        (ES::Begin, N::Continuation, P::Out) => {
            let cont = cont.as_ref().unwrap();
            portsh(cont.has_value(&EdgeDir::Up), cont.has_value(&EdgeDir::Straight), cont.has_value(&EdgeDir::Down))
        },
        (ES::End, N::Continuation, P::In) => {
            let cont = cont.as_ref().unwrap();
            portsh(cont.has_value(&EdgeDir::Up), cont.has_value(&EdgeDir::Straight), cont.has_value(&EdgeDir::Down))
        },

        _ =>  panic!("Invalid edge shape {:?}",(side,shape,port)),
    }
}

fn optimize_and_commit_unary(name :&str, s :&mut SATModDiff<isize>, x :&Unary) -> Result<(), String> {
    debug!("(^) Optimizing {}", name);
    let (mut lo, mut hi) = (0,x.bound());
    while lo < hi {
        let mid = (hi-lo)/2 + lo;
        debug!("Constraint on {}={}", name, mid);
        match s.solve_under_assumptions(&vec![x.lte_const(mid as isize)]) {
            Ok(_) => { debug!("(^) successful {} <= {}", name, mid); hi = mid; }
            Err(_) => { debug!("(^) unsuccessful {} <= {}", name, mid); lo = mid+1; }
        }
    }
    assert_eq!(lo,hi);
    debug!("Adding clause {:?}", vec![x.lte_const(lo as isize)]);
    s.sat.add_clause(vec![x.lte_const(lo as isize)]);
    s.solve().map_err(|_| format!("Could not compress {} to {}",name, lo))?;
    Ok(())
}

#[derive(PartialEq,Eq,Debug)]
enum EdgeDir { Up, Straight, Down }

fn map_portshape(s :&minisat::Model, x :&PortShape) -> EdgeDir {
    if s.value(&x.up) { return EdgeDir::Up; }
    if s.value(&x.straight) { return EdgeDir::Straight; }
    if s.value(&x.down) { return EdgeDir::Down; }
    panic!()
}

fn goal_width(s :&mut SATModDiff<isize>, node_delta_xs :&Vec<Unary>) -> Result<(), String> {
    let max_x :usize = {
        let m = s.solve().map_err(|_| format!("Could not solve."))?;
        node_delta_xs.iter().map(|x| m.sat.value(x)).sum()
    };
    let sum_x = Unary::sum_truncate(&mut s.sat, node_delta_xs.clone(), max_x +1);
    optimize_and_commit_unary("width",s,&sum_x)
}

fn goal_diagonals(s :&mut SATModDiff<isize>, edge_shapes :&[(PortShape,PortShape)]) -> Result<(),String> {
    let max_negstraight :usize = {
        let m = s.solve().map_err(|_| format!("Could not solve."))?;
        edge_shapes.iter().flat_map(|(b,e)| vec![b,e]).map(|s|
            if map_portshape(&m.sat,s) == EdgeDir::Straight { 0 } else { 1 }) .sum()
    };
    let negstraight_bits = edge_shapes.iter().flat_map(|(b,e)| vec![b,e])
        .map(|e| Unary::from_bool(!e.straight));
    let sum_negstraight = Unary::sum_truncate(&mut s.sat, 
               negstraight_bits.collect(), max_negstraight+1);

    optimize_and_commit_unary("diagonals",s,&sum_negstraight)
}

fn goal_nodeshapes(s :&mut SATModDiff<isize>, slants :&[Bool]) -> Result<(),String> {
    let max :usize = {
        let m = s.solve().map_err(|_| format!("Could not solve."))?;
        slants.iter().map(|x| if m.sat.value(x) { 1 } else { 0 }).sum()
    };
    let sum = Unary::sum_truncate(&mut s.sat, slants.iter().cloned()
                                  .map(Unary::from_bool).collect(), max+1);
    optimize_and_commit_unary("nodeshape",s,&sum)

}


fn goal_shortedges(s :&mut SATModDiff<isize>, edge_short :&[(Bool,Bool)]) -> Result<(), String> {
    let max_negshorts :usize = {
        let m = s.solve().map_err(|_| format!("Could not solve."))?;
        edge_short.iter().map(|(u,d)|
               if m.sat.value(u) { 0 } else {1 } +
               if m.sat.value(d) { 0 } else {1 } ).sum() };
    let sum_negshorts = Unary::sum_truncate(&mut s.sat,
        edge_short.iter().flat_map(|(u,d)| vec![!*u,!*d])
        .map(Unary::from_bool).collect(), max_negshorts+1);
    optimize_and_commit_unary("shortedges",s,&sum_negshorts)
}

fn goal_local_xdiffs(s :&mut SATModDiff<isize>, node_delta_xs :&[Unary], nodes :&[Node]) -> Result<(), String> {
    let mut nodes_by_distance = nodes.iter().zip(nodes.iter().skip(1))
        .map(|(a,b)| ordered_float::OrderedFloat(b.pos - a.pos))
        .zip(node_delta_xs).enumerate().collect::<Vec<_>>();
    nodes_by_distance.sort_by_key(|(_i,(d,_))| *d);
    for (i,(_d, x_diff)) in nodes_by_distance {
        optimize_and_commit_unary(&format!("dx_{}", i), s, x_diff)?;
    }
    Ok(())
}

fn goal_local_ydiff(s :&mut SATModDiff<isize>, edges :&[Edge], edges_lt :&[EdgePair], edge_ys :&[DVar]) -> Result<(), String> {
    let mut edges_lt = edges_lt.to_vec();
    edges_lt.sort_by_key(|(a,b)| 
        if edges[*a].a.node == edges[*b].a.node ||
           edges[*a].b.node == edges[*b].b.node { 
                0
            } else { 1 });
    for (a,b) in edges_lt.iter() {
        let m = s.solve().map_err(|_| format!("Could not solve."))?;
        let y1 = m.diff.get_value(edge_ys[*a]);
        let y2 = m.diff.get_value(edge_ys[*b]);
        assert!(y1 <= y2);
        let mut bound = y2 - y1;
        loop {
            let c1 = s.cond_constraint(edge_ys[*b], edge_ys[*a], bound-1);
            match s.solve_under_assumptions(&vec![c1]) {
                Ok(_) => {
                    bound -= 1;
                    debug!("(^) Updated localydiff({},{}) to {}", a, b, bound);
                },
                Err(()) => {
                    debug!("(^) could not compress localYdiff({},{})<{}.",a, b, bound);
                    break;
                },
            }
        }

        let c = s.cond_constraint(edge_ys[*b], edge_ys[*a], bound);
        s.sat.add_clause(vec![c]);
    }

    Ok(())
}

fn goal_symbol_width(s :&mut SATModDiff<isize>, xi: DVar, xf :DVar, symbol_factor :isize) -> Result<(), String> {
    let mut bound = {
        let m = s.solve().map_err(|_| format!("Could not solve."))?;
        let d = m.diff.get_value(xf) - m.diff.get_value(xi);
        ((d-1)/symbol_factor) + 1
    };


    loop {
        let c1 = s.cond_constraint(xf, xi, symbol_factor*(bound - 1));
        if s.solve_under_assumptions(&vec![c1]).is_ok() {
            bound -= 1;
            debug!("(^) Updated sx to {}", symbol_factor*bound);
        } else {
            debug!("(^) could not compress sx<{}.", symbol_factor*bound);
            break;
        }

        let c = s.cond_constraint(xf, xi, symbol_factor*bound);
        s.sat.add_clause(vec![c]);
    }

    Ok(())
}


fn goal_bends(s :&mut SATModDiff<isize>, edge_shapes :&[(PortShape,PortShape)]) -> Result<(),String> {
    let max_bends = {
        let m = s.solve().map_err(|_| format!("Could not solve."))?;
        let edgedir = edge_shapes.iter().map(|(begin,end)|
             (map_portshape(&m.sat, begin),
              map_portshape(&m.sat, end))).collect::<Vec<_>>();
        let max_bends :usize = edgedir.iter().map(|(a,b)| {
            if *a == *b { return 0; }
            if *a == EdgeDir::Straight || *b == EdgeDir::Straight { return 1; }
            return 2;
        }).sum();
        max_bends
    };
    debug!("(^) Max bends {}", max_bends);

    let bends_unary = edge_shapes.iter().map(|(begin,end)| {
        let x = Unary::new(&mut s.sat, 2);
        s.sat.add_clause(vec![!begin.straight, end.straight, x.gte_const(1)]);
        s.sat.add_clause(vec![begin.straight, !end.straight, x.gte_const(1)]);
        s.sat.add_clause(vec![!begin.up,   !end.down, x.gte_const(2)]);
        s.sat.add_clause(vec![!begin.down, !end.up,   x.gte_const(2)]);
        x

    }).collect::<Vec<_>>();
    let sum_bends = Unary::sum_truncate(&mut s.sat, bends_unary, max_bends+1);
    optimize_and_commit_unary("bends", s, &sum_bends)
}


fn goal_height(s :&mut SATModDiff<isize>, edge_ys :&[DVar], edges_lt :&[EdgePair]) -> Result<(), String> {

    // find a set of highest edges, and minimize their maximum
    let mut highest_edges : HashSet<usize> = (0.. edge_ys.len()).collect();
    for (a,_b) in edges_lt { highest_edges.remove(a); }
    let height = s.diff.named_var(Some(format!("max_y")));
    for e in highest_edges.iter() {
        let lit = s.cond_constraint(edge_ys[*e], height, 0);
        s.sat.add_clause(vec![lit]);
    }

    let zero = s.diff.zero();
    optimize_and_commit_dvar("max_y", s, zero, height)
}

fn goal_maintrackheight(s :&mut SATModDiff<isize>, edge_ys :&[DVar], edges_lt :&[EdgePair], maintracks :&[EdgeRef]) -> Result<(), String> {
    debug!("Minimizing delta Y for main tracks edge set: {:?}", maintracks);
    println!("Minimizing delta Y for main tracks edge set: {:?}", maintracks);

    // find a set of highest edges, and minimize their maximum
    let mut highest_edges : HashSet<usize> = maintracks.iter().cloned().collect();
    let mut lowest_edges  : HashSet<usize> = maintracks.iter().cloned().collect();
    for (a,b) in edges_lt.iter().filter(|(a,b)| maintracks.contains(a) && maintracks.contains(b)) { 
        highest_edges.remove(a);
        lowest_edges.remove(b);
    }

    let main_low = s.diff.named_var(Some(format!("maintracks_low")));
    for e in lowest_edges.iter() {
        // main_low - e <= 0
        // main_low <= e
        let lit = s.cond_constraint(main_low, edge_ys[*e], 0);
        s.sat.add_clause(vec![lit]);
    }
    let main_high = s.diff.named_var(Some(format!("maintracks_high")));
    for e in highest_edges.iter() {
        // e - high <= 0
        // e <= high
        let lit = s.cond_constraint(edge_ys[*e], main_high, 0);
        s.sat.add_clause(vec![lit]);
    }

    let x = optimize_and_commit_dvar("maintracks", s, main_low, main_high);
    let m = s.solve().map_err(|_| format!("Could not compress"))?;
    let (a, b) = (m.diff.get_value(main_low), m.diff.get_value(main_high));
    println!("Maintrack done {} - {}",a,b);
    x
}

fn optimize_and_commit_dvar(name :&str, s :&mut SATModDiff<isize>, low_x :DVar, high_x :DVar) -> Result<(), String> {
    debug!("(^) Optimizing {}", name);
    let m = s.solve().map_err(|_| format!("Solve failed"))?;
    let (mut lo, mut hi) = (0, m.diff.get_value(high_x) - m.diff.get_value(low_x));
    while lo < hi {
        let mid = (hi-lo)/2 + lo;
        debug!("Constraint on {}={}", name, mid);
        let cond = s.cond_constraint(high_x, low_x, mid);
        match s.solve_under_assumptions(&vec![cond]) {
            Ok(_) => { debug!("(^) successful {} <= {}", name, mid); hi = mid; }
            Err(_) => { debug!("(^) unsuccessful {} <= {}", name, mid); lo = mid+1; }
        }
    }
    assert_eq!(lo,hi);
    let c = s.cond_constraint(high_x, low_x, lo);
    s.sat.add_clause(vec![c]);
    debug!("(^) Committing {}={}", name,lo);
    s.solve().map_err(|_| format!("Could not compress {} to {}",name, lo))?;
    Ok(())
}


#[derive(Debug, Copy, Clone)]
struct DiffOffset(Option<DVar>, isize);

fn symbol_edgeclass_constraints(nodes :&[Node], edges :&[Edge], symbols :&[(EdgeRef,&Symbol)], symbol_node_xs :&[DVar], symbol_xs :&[DVar], symbol_factor :isize) -> Vec<(DiffOffset,DiffOffset)> {
    let edge_ports = edges.iter().enumerate().flat_map(|(i, Edge { a,b })| vec![(i,a),(i,b)])
        .map(|(i,NodePort { node, port })| ((*node,*port),i)).collect::<HashMap<_,_>>();
    let mut sameedgesabove = Vec::new();
    let mut sameedgesbelow = Vec::new();
    let mut edgelevelsymbols = HashMap::new();

    let mut end = |ei:usize, dirfactor:isize, offset:f64, above:isize| {
        let offset :isize = (offset*symbol_factor as f64) as isize;
        let node_x = if dirfactor > 0 { symbol_node_xs[edges[ei].a.node] } 
                else { symbol_node_xs[edges[ei].b.node] };
        for l in 1..=2 {
            let x = DiffOffset(Some(node_x), dirfactor*l*offset);
            edgelevelsymbols.entry((ei,above*l)).or_insert(Vec::new())
                .push((-(dirfactor as f64)*1e6, (x,x)));
        }
    };

    for (i,Node { shape, .. }) in nodes.iter().enumerate() {
        match shape {
            Shape::Switch(side,dir) => {
                let  leftedge = edge_ports[&(i,Port::Left)];
                let rightedge = edge_ports[&(i,Port::Right)];
                let trunkedge = edge_ports[&(i,Port::Trunk)];

                trace!("sw {} l{} r{} t{}", i,leftedge,rightedge,trunkedge);

                match dir {
                    Dir::Up => {
                        end(rightedge, 1, 0.25,  1);
                        end(leftedge,  1, 0.25, -1);
                    },
                    Dir::Down => {
                        end(leftedge , -1, 0.25,  1);
                        end(rightedge, -1, 0.25, -1);
                    },
                }

                match (side,dir) {
                    (Side::Left, Dir::Up)    =>  {
                        sameedgesbelow.push((trunkedge,rightedge)); // below
                        end(leftedge,   1, 0.0, 1); // above
                        end(trunkedge, -1, 0.0, 1); // above
                    },
                    (Side::Left, Dir::Down)  =>  {
                        sameedgesabove.push((trunkedge,rightedge)); // above
                        end(leftedge,  -1, 0.0, -1); // below
                        end(trunkedge,  1, 0.0, -1); // below
                    },
                    (Side::Right, Dir::Up)    => {
                        sameedgesabove.push((trunkedge,leftedge));  // above
                        end(rightedge,  1, 0.0, -1); // below
                        end(trunkedge, -1, 0.0, -1); // below
                    },
                    (Side::Right, Dir::Down)  => {
                        sameedgesbelow.push((trunkedge,leftedge));  // below
                        end(rightedge, -1, 0.0, 1); // above
                        end(trunkedge,  1, 0.0, 1); // above
                    },
                }
            },
            Shape::Begin => { trace!("begin");
                end(edge_ports[&(i,Port::Out)],  1, 0.0, 1);
                end(edge_ports[&(i,Port::Out)],  1, 0.0, -1);
            },
            Shape::End   => { trace!("end");
                end(edge_ports[&(i,Port::In)], -1, 0.0, 1);
                end(edge_ports[&(i,Port::In)], -1, 0.0, -1);
            },
            _ => { continue; } // TODO continuations and crossings
            //_ => panic!("Unsupported shape"),
        }
    }

    let symbol_factor = 10000isize;
    let factor = |x:f64| (x*symbol_factor as f64) as isize;

    for (i,(e,s)) in symbols.iter().enumerate() {
        let left = DiffOffset(Some(symbol_xs[i]),factor(-s.origin));
        let right = DiffOffset(Some(symbol_xs[i]),factor(-s.origin + s.width));
        edgelevelsymbols.entry((*e, s.level)).or_insert(Vec::new())
            .push((s.pos, (left,right)));
    }

    let els = {
        use disjoint_sets::*;
        let mut unionabove = UnionFind::new(edges.len());
        for (a,b) in sameedgesabove { unionabove.union(a,b); }
        let mut unionbelow = UnionFind::new(edges.len());
        for (a,b) in sameedgesbelow { unionbelow.union(a,b); }

        let mut els2 = HashMap::new();
        for ((e,l),v) in edgelevelsymbols {
            let e2 = if l == 0 { e } else {
               if l < 0 { unionbelow.find(e) } else { unionabove.find(e) }
            };

            els2.entry((e2,l)).or_insert(Vec::new()).extend(v);
        }
        els2
    };

    let mut out = Vec::new();
    for (_k,v) in els {
        let mut va = v;
        va.sort_by_key(|(p,_)| OrderedFloat(*p));
        for ((_p1,(_l1,r1)),(_p2,(l2,_r2))) in va.iter().zip(va.iter().skip(1)) {
            out.push((*r1,*l2));
        }
    }
    out
}

