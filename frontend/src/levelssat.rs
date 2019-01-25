use crate::schematic_graph::*;
use crate::solvers::Goal;
use diffsolver::*;
use diffsolver::minisat::Bool;
use diffsolver::minisat::unary::*;
use ordered_float::OrderedFloat;

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

pub fn solve(nodes :&[Node], edges :&[Edge], symbols:&[(EdgeRef,&Symbol)], edges_lt :&[EdgePair], goals :&[Goal]) -> Result<Output, String> {


    let mut s = SATModDiff::new();

        let node_delta_xs = nodes.iter().zip(nodes.iter().skip(1))
        .map(|_| Unary::new(&mut s.sat,2)).collect::<Vec<_>>();
    let node_ys = nodes.iter().enumerate()
        .map(|(i,_)| s.diff.named_var(Some(format!("ny_{}",i)))).collect::<Vec<_>>();
    let edge_ys = edges.iter().enumerate()
        .map(|(i,_)| s.diff.named_var(Some(format!("ey_{}",i)))).collect::<Vec<_>>();
    let edge_short = edges.iter()
        .map(|_| (s.sat.new_lit(), s.sat.new_lit())).collect::<Vec<_>>();
    let slanted = nodes.iter()
        .map(|x| if let Shape::Switch(_,_) = x.shape { s.sat.new_lit() }
             else { Bool::Const(false) } )
        .collect::<Vec<_>>();


    println!("($) adding constraints");
    // constraint: edges push nodes apart:  sum(delta_x_a .. delta_x_b) >= 1
    for Edge { a,b } in edges {
        s.sat.add_clause(((a.node) .. (b.node)) .map(|x| node_delta_xs[x].gte_const(1)));
    }

    println!("Edges LT {:?}", edges_lt);
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
        println!("edge {:?}", e);
        let (short_up, short_down) = edge_short[i];
        let e_begin = mk_port_shape(EdgeSide::Begin, &nodes[e.a.node].shape,
                                    e.a.port, slanted[e.a.node]);
        let e_end  = mk_port_shape(EdgeSide::End, &nodes[e.b.node].shape,
                                   e.b.port, slanted[e.b.node]);

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
        let dx = Unary::sum_truncate(&mut s.sat, node_delta_xs[(e.a.node)..(e.b.node)].iter().cloned().collect(), 2);
        s.sat.add_clause(vec![!short_up,            dx. lt_const(2)]);
        s.sat.add_clause(vec![!short_down,          dx. lt_const(2)]);
        s.sat.add_clause(vec![short_down, short_up, both_straight, dx.gte_const(2)]);

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
        let dx0a = s.cond_constraint(symbol_node_xs[i], symbol_node_xs[i+1], 0);
        let dx0b = s.cond_constraint(symbol_node_xs[i+1], symbol_node_xs[i], 0);
        let dx1a = s.cond_constraint(symbol_node_xs[i], symbol_node_xs[i+1], -1*symbol_factor);
        let dx1b = s.cond_constraint(symbol_node_xs[i+1], symbol_node_xs[i], 1*symbol_factor);
        s.sat.add_clause(vec![!dx.lt_const(0),dx0a]);
        s.sat.add_clause(vec![!dx.lt_const(0),dx0b]);
        s.sat.add_clause(vec![dx.lt_const(0), !dx.lt_const(1), dx1a]);
        s.sat.add_clause(vec![dx.lt_const(0), !dx.lt_const(1), dx1b]);
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
    for (a,b) in symbol_edgeclass_constraints(&nodes, &edges, &symbols, &symbol_node_xs, &symbol_xs, symbol_factor) {
        println!("Constraint {:?}", (a,b));
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


     //let print = |m:&SATDiffModel| {
     //    let mut xs = node_delta_xs.iter().scan(0, |a,x| { *a += m.sat.value(x); Some(*a) }).collect::<Vec<_>>();
     //    xs.insert(0,0);
 
     //    let ys = node_ys.iter().map(|x| m.diff.get_value(*x)).collect::<Vec<_>>();
 
     //    for (i,x) in xs.iter().enumerate() {
     //        println!("Node({}): x={}, y={}", i, x, ys[i]);
     //    }
 
     //    let edge_ys = edge_ys.iter().map(|x| m.diff.get_value(*x)).collect::<Vec<_>>();
     //    for (i,_y) in edges.iter().enumerate() {
     //        println!("Edge({}): y={}", i, edge_ys[i]);
     //    }
 
     //    let z = m.diff.zero();
     //    println!("Zero={}", m.diff.get_value(z));
     //};



    println!("($) starting solve ({} dvars, {} dclauses)", s.diff.num_vars(), s.diff.num_constraints());

    {
        let _model = s.solve().map_err(|_| "Unconstrained solve failed".to_string())?;
    }


    for goal in goals {
        match goal {
            Goal::Bends  => { goal_bends(&mut s, &edge_shapes)?; },
            Goal::Width  => { goal_width(&mut s, &node_delta_xs)?; },
            Goal::Height => { goal_height(&mut s, &edge_ys)?; },
            Goal::Diagonals => { goal_diagonals(&mut s, &edge_shapes)?; },
            Goal::Nodeshapes => { goal_nodeshapes(&mut s, &slanted)?; },
            Goal::Shortedges => { goal_shortedges(&mut s, &edge_short)?; },
            //_ => unimplemented!(), --- unreachable pattern for now, activate if new
            //  criteria are added.
        }
    }

    // After optimization, we extract the node_dx, ndoe_y, edge_y values
    // and use them add constraints to symbol_node_xs.

    let (node_dx_values, node_y_values, edge_y_values) = {
        let m = s.solve().unwrap();
        let node_dx_values = node_delta_xs.iter()
            .map(|x| m.sat.value(x)).collect::<Vec<_>>();
        let node_y_values = node_ys.iter()
            .map(|x| m.diff.get_value(*x)).collect::<Vec<_>>();
        let edge_y_values = edge_ys.iter()
            .map(|x| m.diff.get_value(*x)).collect::<Vec<_>>();
        (node_dx_values,node_y_values,edge_y_values)
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
        let dy2 = node_y_values[e.b.node] - edge_y_values[i];
        let samedir = dy1.signum() == dy2.signum() && dy1.signum() != 0;
        let dist = dy1.abs() + dy2.abs() + if samedir { 0 } else { 1 };
        println!("Edge {:?} dist{} dy{} dy{} {:?} {:?} {}", e,dist,dy1,dy2,x1,x2,-(dist*symbol_factor));

        let c = s.cond_constraint(x1,x2,-(dist*symbol_factor));
        s.sat.add_clause(vec![c]);
    }

    let (node_x_values,symbol_x_values) = {
        let m = s.solve().map_err(|_| format!("Unable to solve."))?;
        let node_x_values = symbol_node_xs.iter()
            .map(|v| m.diff.get_value(*v)).collect::<Vec<_>>();
        let symbol_x_values = symbol_xs.iter()
            .map(|v| m.diff.get_value(*v)).collect::<Vec<_>>();
        (node_x_values, symbol_x_values)
    };

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

fn mk_port_shape(side :EdgeSide, shape :&Shape, port :Port, s :Bool) -> PortShape {

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

        _ =>  panic!("Invalid edge shape {:?}",(side,shape,port)),
    }
}

fn optimize_and_commit_unary(name :&str, s :&mut SATModDiff, x :Unary) -> Result<(), String> {
    println!("(^) Optimizing {}", name);
    let (mut lo, mut hi) = (0,x.bound());
    while lo < hi {
        let mid = (hi-lo)/2 + lo;
        println!("Constraint on {}={}", name, mid);
        match s.solve_under_assumptions(&vec![x.lte_const(mid as isize)]) {
            Ok(_) => { println!("(^) successful {} <= {}", name, mid); hi = mid; }
            Err(_) => { println!("(^) unsuccessful {} <= {}", name, mid); lo = mid+1; }
        }
    }
    assert_eq!(lo,hi);
    s.sat.add_clause(vec![x.lte_const(lo as isize)]);
    s.solve().map_err(|_| format!("Could not compress {}",name))?;
    Ok(())
}

#[derive(PartialEq,Eq)]
enum EdgeDir { Up, Straight, Down }

fn map_portshape(s :&minisat::Model, x :&PortShape) -> EdgeDir {
    if s.value(&x.up) { return EdgeDir::Up; }
    if s.value(&x.straight) { return EdgeDir::Straight; }
    if s.value(&x.down) { return EdgeDir::Down; }
    panic!()
}

fn goal_width(s :&mut SATModDiff, node_delta_xs :&Vec<Unary>) -> Result<(), String> {
    let max_x :usize = {
        let m = s.solve().map_err(|_| format!("Could not solve."))?;
        node_delta_xs.iter().map(|x| m.sat.value(x)).sum()
    };
    let sum_x = Unary::sum_truncate(&mut s.sat, node_delta_xs.clone(), max_x +1);
    optimize_and_commit_unary("width",s,sum_x)
}

fn goal_diagonals(s :&mut SATModDiff, edge_shapes :&[(PortShape,PortShape)]) -> Result<(),String> {
    let max_negstraight :usize = {
        let m = s.solve().map_err(|_| format!("Could not solve."))?;
        edge_shapes.iter().flat_map(|(b,e)| vec![b,e]).map(|s|
            if map_portshape(&m.sat,s) == EdgeDir::Straight { 0 } else { 1 }) .sum()
    };
    let negstraight_bits = edge_shapes.iter().flat_map(|(b,e)| vec![b,e])
        .map(|e| Unary::from_bool(!e.straight));
    let sum_negstraight = Unary::sum_truncate(&mut s.sat, 
               negstraight_bits.collect(), max_negstraight+1);

    optimize_and_commit_unary("diagonals",s,sum_negstraight)
}

fn goal_nodeshapes(s :&mut SATModDiff, slants :&[Bool]) -> Result<(),String> {
    let max :usize = {
        let m = s.solve().map_err(|_| format!("Could not solve."))?;
        slants.iter().map(|x| if m.sat.value(x) { 1 } else { 0 }).sum()
    };
    let sum = Unary::sum_truncate(&mut s.sat, slants.iter().cloned()
                                  .map(Unary::from_bool).collect(), max+1);
    optimize_and_commit_unary("nodeshape",s,sum)

}


fn goal_shortedges(s :&mut SATModDiff, edge_short :&[(Bool,Bool)]) -> Result<(), String> {
    let max_negshorts :usize = {
        let m = s.solve().map_err(|_| format!("Could not solve."))?;
        edge_short.iter().map(|(u,d)|
               if m.sat.value(u) { 0 } else {1 } +
               if m.sat.value(d) { 0 } else {1 } ).sum() };
    let sum_negshorts = Unary::sum_truncate(&mut s.sat,
        edge_short.iter().flat_map(|(u,d)| vec![!*u,!*d])
        .map(Unary::from_bool).collect(), max_negshorts+1);
    optimize_and_commit_unary("shortedges",s,sum_negshorts)
}

fn goal_bends(s :&mut SATModDiff, edge_shapes :&[(PortShape,PortShape)]) -> Result<(),String> {
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
    println!("(^) Max bends {}", max_bends);

    let bends_unary = edge_shapes.iter().map(|(begin,end)| {
        let x = Unary::new(&mut s.sat, 2);
        s.sat.add_clause(vec![!begin.straight, end.straight, x.gte_const(1)]);
        s.sat.add_clause(vec![begin.straight, !end.straight, x.gte_const(1)]);
        s.sat.add_clause(vec![!begin.up,   !end.down, x.gte_const(2)]);
        s.sat.add_clause(vec![!begin.down, !end.up,   x.gte_const(2)]);
        x

    }).collect::<Vec<_>>();
    let sum_bends = Unary::sum_truncate(&mut s.sat, bends_unary, max_bends+1);
    optimize_and_commit_unary("bends", s, sum_bends)
}



fn goal_height(s :&mut SATModDiff, edge_ys :&[DVar]) -> Result<(), String> {
    let zero = s.diff.zero();

    // get y values
    let get_maxy = |model:&SATDiffModel| {
        let mut y_max = -100_000isize;
        for yvar in edge_ys {
            let y = model.diff.get_value(*yvar);
            if y > y_max { y_max = y; }
        }

        let mut y_max_vars = Vec::new();
        for yvar in edge_ys {
            if model.diff.get_value(*yvar) >= y_max {
                y_max_vars.push(*yvar);
            }
        }

        println!("(^) fits inside y={}", y_max);
        (y_max,y_max_vars)
    };

    let (mut y_max,mut y_max_vars) = {
        let model = s.solve().map_err(|_| format!("Solve failed"))?;
        get_maxy(&model)
    };

    loop {
        let assumptions = {
            let mut assumptions = Vec::new();
            for v in &y_max_vars {
                let c1 = s.cond_constraint(*v, zero, y_max-1);
                assumptions.push(c1);
            }
            assumptions };

        match s.solve_under_assumptions(&assumptions) {
            Ok(model) => {
                y_max = y_max - 1;
                y_max_vars = Vec::new();
                for yvar in edge_ys {
                    if model.diff.get_value(*yvar) >= y_max {
                        y_max_vars.push(*yvar);
                    }
                }
                println!("(^) Updated y_max to {} ({:?})", y_max, y_max_vars);
            },
            Err(()) => {
                println!("(^) could not compress Y<{}.",y_max);
                break;
            }
        }
    };

    for y in edge_ys {
        let c = s.cond_constraint(*y, zero, y_max);
        s.sat.add_clause(vec![c]);
    }
    s.solve().map_err(|_| format!("Could not compress in Y direction"))?;
    Ok(())
}


#[derive(Debug, Copy, Clone)]
struct DiffOffset(Option<DVar>, isize);

fn symbol_edgeclass_constraints(nodes :&[Node], edges :&[Edge], symbols :&[(EdgeRef,&Symbol)], symbol_node_xs :&[DVar], symbol_xs :&[DVar], symbol_factor :isize) -> Vec<(DiffOffset,DiffOffset)> {
    use std::collections::HashMap;
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

                println!("sw {} l{} r{} t{}", i,leftedge,rightedge,trunkedge);

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
            Shape::Begin => { println!("begin");
                end(edge_ports[&(i,Port::Out)],  1, 0.0, 1);
                end(edge_ports[&(i,Port::Out)],  1, 0.0, -1);
            },
            Shape::End   => { println!("end");
                end(edge_ports[&(i,Port::In)], -1, 0.0, 1);
                end(edge_ports[&(i,Port::In)], -1, 0.0, -1);
            },
            _ => panic!("Unsupported shape"),
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
        //println!("sorted {:?} {:?}",k,va);
        for ((_p1,(_l1,r1)),(_p2,(l2,_r2))) in va.iter().zip(va.iter().skip(1)) {
            out.push((*r1,*l2));
        }
    }
    out
}

