use crate::schematic_graph::*;
use diffsolver::*;

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

pub fn solve(nodes :&[Shape], edges :&[Edge], symbols:&[(EdgeRef,&Symbol)], edges_lt :&[EdgePair]) -> Result<Output, String> {

    use diffsolver::*;
    use diffsolver::minisat::*;
    use diffsolver::minisat::unary::*;

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
        .map(|x| if let Shape::Switch(_,_) = x { s.sat.new_lit() }
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
        let ((a_short_up,a_short_down),(b_short_up,b_short_down)) = (edge_short[*a],edge_short[*b]);
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
        let e_begin = mk_port_shape(EdgeSide::Begin, &nodes[e.a.node],
                                    e.a.port, slanted[e.a.node]);
        let e_end  = mk_port_shape(EdgeSide::End, &nodes[e.b.node],
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

        let print = |m:&SATDiffModel| {
        let mut xs = node_delta_xs.iter().scan(0, |a,x| { *a += m.sat.value(x); Some(*a) }).collect::<Vec<_>>();
        xs.insert(0,0);

        let ys = node_ys.iter().map(|x| m.diff.get_value(*x)).collect::<Vec<_>>();

        for (i,x) in xs.iter().enumerate() {
            println!("Node({}): x={}, y={}", i, x, ys[i]);
        }

        let edge_ys = edge_ys.iter().map(|x| m.diff.get_value(*x)).collect::<Vec<_>>();
        for (i,y) in edges.iter().enumerate() {
            println!("Edge({}): y={}", i, edge_ys[i]);
        }

        let z = m.diff.zero();
        println!("Zero={}", m.diff.get_value(z));
    };



            println!("($) starting solve ({} dvars, {} dclauses)", s.diff.num_vars(), s.diff.num_constraints());
                {

    let model = s.solve().map_err(|_| "Unconstrained solve failed".to_string())?;
    print(&model);
    }




    println!("started diffsolver");
    unimplemented!()
}


use diffsolver::minisat::Bool;

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


