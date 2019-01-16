
use z3;
use parser;
use parser::{Side, Dir, Port};
use trans_red::trans_red;

pub type EdgeRef = usize;
pub type NodeRef = usize;

//#[derive(Copy,Clone,Debug)]
//pub enum Side { Left, Right}
//#[derive(Copy,Clone,Debug)]
//pub enum Dir { Up, Down }

#[derive(Debug,Clone)]
pub enum NodeShape {
    Begin,
    End,
    Switch {
        side: Side,
        dir :Dir,
        left :EdgeRef,
        right :EdgeRef,
        trunk :EdgeRef,
    },
    Vertical,
}

pub struct Dirs<'c> {
    is_up : z3::Ast<'c>,
    is_straight : z3::Ast<'c>,
    is_down : z3::Ast<'c>,
}
impl<'c> Dirs<'c> {
    pub fn new(ctx: &'c z3::Context) -> Dirs<'c> {
        Dirs {
            is_up: ctx.from_bool(false),
            is_down: ctx.from_bool(false),
            is_straight: ctx.from_bool(false),
        }
    }
}

#[derive(Debug,Clone)]
pub struct Node { 
    pub name :String,
    pub pos: f64,
    pub shape :NodeShape,
}

#[derive(Copy,Clone,Hash,Debug,PartialEq, Eq)]
pub struct PortRef {
    pub node: usize,
    pub port: Port,
}


pub type Pt = (f64,f64);

#[derive(Debug,Clone)]
pub struct Symbol {
    /// symbol name
    pub name: String,

    /// absolute position of insertion point
    pub abspos :f64,

    /// width of symbol
    pub width :f64,

    /// origin (insertion point) distance from left edge of symbol
    pub origin_x :f64,

    /// level
    pub level: isize,
}



pub fn port(node: usize, port: Port) -> PortRef { PortRef { node, port } }

//pub enum Port { Out, In, Left, Right, Trunk }

#[derive(Debug,Clone)]
pub struct Edge {
    pub a : PortRef, // index of node
    pub b : PortRef, // index of node
}

pub enum EdgeSide { Begin, End }


#[derive(Debug,Clone)]
pub struct SolverInput {
    pub nodes: Vec<Node>,
    pub edges: Vec<Edge>,
    pub symbols: Vec<(usize,Symbol)>,
}

use std::collections::HashMap;
use std::collections::HashSet;
pub fn convert(stmts :Vec<parser::Stmt>) -> Result<(SolverInput,HashMap<String,usize>), String> {
    let mut nodes_in = Vec::new();
    let mut edges_in = Vec::new();
    for s in stmts.into_iter() {
        match s {
            parser::Stmt::Node(name,sh,pos) => {
                nodes_in.push((name,sh,pos));
            },
            parser::Stmt::Edge(a,b) => {
                edges_in.push((a,b,vec![]));
            },
            parser::Stmt::Symbol(name,pos,level,w,x) => {
                ((edges_in.last_mut().unwrap()).2).push(Symbol {
                    name: name,
                    abspos: pos,
                    level: level,
                    width: w,
                    origin_x: x,
                });
            }
        }
    }

    //nodes_in.sort_by_key(|k| k.2 );
    nodes_in.sort_by(|a, b| a.2.partial_cmp(&b.2).unwrap());

    let mut edges = Vec::new();
    let mut nodes = Vec::new();
    let mut symbols = Vec::new();
    let mut edge_to : HashMap<PortRef, usize> = HashMap::new();
    let mut node_names = HashMap::new();

    {
        {
            let mut node_i = 0;
            for (ref name, ref _sh, ref _pos) in &nodes_in {
                node_names.insert(name.clone(), node_i);
                node_i += 1;
            }
        }

        edges_in.sort_by_key(|((n1,_),_,_)| node_names[n1].clone());


        for ((n1,p1),(n2,p2),edge_symbols) in edges_in {
            edge_to.insert(port(node_names[&n1], p1), edges.len());
            edge_to.insert(port(node_names[&n2], p2), edges.len());
            edges.push(Edge {
                a: port(node_names[&n1], p1),
                b: port(node_names[&n2], p2),
            });

            for s in edge_symbols {
                symbols.push((edges.len()-1, s));
            }
        }
    }


    for (name,shape,pos) in nodes_in {
        let ni = nodes.len();
        nodes.push(Node {
            name: name,
            pos: pos,
            shape:  match shape {
                parser::Shape::Begin => NodeShape::Begin,
                parser::Shape::End => NodeShape::End,
                parser::Shape::Switch(side,dir) => NodeShape::Switch {
                    side, dir,
                    left: edge_to[&port(ni,Port::Left)],
                    right: edge_to[&port(ni,Port::Right)],
                    trunk: edge_to[&port(ni,Port::Trunk)],
                },
                parser::Shape::Vertical => NodeShape::Vertical,
            }
        });
    }

    Ok((SolverInput { nodes, edges, symbols }, node_names))
}

//  node->edge lookup
//enum NodeEdgeRef {
//    A(usize),
//    B(usize),
//}

//type NodeLookupEdge = [(usize, 

// Solver-internal representation of node
struct NodeRepr<'ctx> {
    x : z3::Ast<'ctx>,
    y : z3::Ast<'ctx>,
}

// Solver-internal representation of edge
struct EdgeRepr<'ctx> {
    y         : z3::Ast<'ctx>,
    // TODO only y is really needed? 
    //       dy1dy2 are only definitions used in the for-loop over edges
    //       shortup,shortdown are variables, but only used in the for-loop over edges
    //       however, y needs to be stored to index from the less_than relation.
    dy1       : z3::Ast<'ctx>,
    dy2       : z3::Ast<'ctx>,
    shortup   : z3::Ast<'ctx>,
    shortdown : z3::Ast<'ctx>,
}

type EdgePair = (EdgeRef, EdgeRef);
fn less_than(nodes :&[Node], edges :&[Edge]) -> Vec<EdgePair> {
    use std::collections::HashSet;
    use std::collections::BinaryHeap;
    let mut lt : HashSet<(EdgeRef,EdgeRef)> = HashSet::new();

    let lr = |n:NodeRef,dir| {
        // BinaryHeap is max-heap, so negate for increasing  and unit for decreasing mileage
        let dirfactor = match dir { Dir::Up => -1, Dir::Down => 1 };

        let top_edge = |n:NodeRef| match (dir,&nodes[n].shape) {
            (Dir::Up, NodeShape::Switch { dir: Dir::Up, left, .. }) => left,
            (Dir::Down, NodeShape::Switch { dir: Dir::Down, right, .. }) => right,
            _ => panic!(),
        };

        let bottom_edge = |n:NodeRef| match (dir, &nodes[n].shape) {
            (Dir::Up, NodeShape::Switch { dir: Dir::Up, right, .. }) => right,
            (Dir::Down, NodeShape::Switch { dir: Dir::Down, left, .. }) => left,
            _ => panic!(),
        };

        let edge_node = |e:EdgeRef| match dir {
            Dir::Up => edges[e].b.node,
            Dir::Down => edges[e].a.node,
        };

        let next_edges = |n:NodeRef| match (dir, &nodes[n].shape) {
            // TODO get rid of allocation with smallvec? probably not worth much.
            (Dir::Up, NodeShape::End) => vec![],
            (Dir::Up, NodeShape::Switch { dir: Dir::Up, left, right, .. }) => vec![left, right],
            (Dir::Up, NodeShape::Switch { dir: Dir::Down, trunk, .. }) => vec![trunk],
            (Dir::Down, NodeShape::Begin) => vec![],
            (Dir::Down, NodeShape::Switch { dir: Dir::Down, left, right, .. }) => vec![left, right],
            (Dir::Down, NodeShape::Switch { dir: Dir::Up, trunk, .. }) => vec![trunk],
            // TODO is this correct?
            (_, NodeShape::Vertical) => vec![],
             _ => panic!("next_edges does not support vertical nodes"),
        };

        let mut over_edges = HashSet::new();
        let mut over_nodes = vec![edge_node(*top_edge(n))].into_iter().collect::<HashSet<usize>>();
        let mut over_queue :BinaryHeap<(isize,usize)> = BinaryHeap::new();
        over_queue.push((dirfactor*edge_node(*top_edge(n)) as isize, *top_edge(n)));

        let mut under_edges = HashSet::new();
        let mut under_nodes = vec![edge_node(*bottom_edge(n))].into_iter().collect::<HashSet<usize>>();
        let mut under_queue :BinaryHeap<(isize,usize)>  = BinaryHeap::new();
        under_queue.push((dirfactor*edge_node(*bottom_edge(n)) as isize, *bottom_edge(n)));

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
                            over_nodes.insert(edge_node(*edge_i));
                            over_queue.push((dirfactor*edge_node(*edge_i) as isize, *edge_i));
                        }
                    } else {
                        let (_,edge) = under_queue.pop().unwrap();
                        under_edges.insert(edge);
                        for edge_i in next_edges(edge_node(edge)) {
                            under_nodes.insert(edge_node(*edge_i));
                            under_queue.push((dirfactor*edge_node(*edge_i) as isize, *edge_i));
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
            NodeShape::Switch { dir, side, .. } => {
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

    let mut lt = lt.into_iter().collect::<Vec<_>>();
    lt.sort();
    lt
}

#[derive(Debug,Clone)]
pub struct TrackOutput {
    pub node_coords: Vec<(String, f64, f64)>,
    // TODO do we need to have names for edges for later consumption?
    pub edge_levels: Vec<(PortRef, PortRef, f64)>,
}

#[derive(Debug,Clone)]
pub struct SolverOutput {
    pub tracks :TrackOutput,
    pub symbols :Vec<((usize,Symbol), (Pt,Pt))>,
}

// TODO: is this too messy? could it be done better directly in the logic?
fn resolve_topbottom(input :&mut SolverInput, lt :&[EdgePair]) -> Vec<(Edge,Edge)> {
    let lt : HashSet<EdgePair> = lt.iter().cloned().collect();
    let mut changes = Vec::new();

    let mut topbottom_pairs : HashMap<(usize,Dir), Vec<usize>> = HashMap::new();
    for (i,e) in input.edges.iter().enumerate() {
        if let Port::TopBottom = e.a.port {
            topbottom_pairs.entry((e.a.node, Dir::Up)).or_insert(Vec::new()).push(i);
        }
        if let Port::TopBottom = e.b.port {
            topbottom_pairs.entry((e.b.node, Dir::Down)).or_insert(Vec::new()).push(i);
        }
    }

    for ((node,dir),edges) in topbottom_pairs.into_iter() {
        if edges.len() != 2 {
            panic!("TopBottom edges must come in pairs.");
        }
        let ea = edges[0];
        let eb = edges[1];

        let ea_old = input.edges[ea].clone();
        let eb_old = input.edges[eb].clone();

        if lt.contains(&(ea,eb)) {
            match dir {
                Dir::Up => {
                    input.edges[ea].a.port = Port::Bottom;
                    input.edges[eb].a.port = Port::Top;
                },
                Dir::Down => {
                    input.edges[ea].b.port = Port::Bottom;
                    input.edges[eb].b.port = Port::Top;
                },
            }
        } else if lt.contains(&(eb,ea)) {
            match dir {
                Dir::Up => {
                    input.edges[eb].a.port = Port::Bottom;
                    input.edges[ea].a.port = Port::Top;
                },
                Dir::Down => {
                    input.edges[eb].b.port = Port::Bottom;
                    input.edges[ea].b.port = Port::Top;
                },
            }
        } else {
            panic!("Could not determine above/below relation on TopBottom pair.");
        }

        changes.push((ea_old, input.edges[ea].clone()));
        changes.push((eb_old, input.edges[eb].clone()));
    }

    changes
}

pub fn solve_difftheory(mut input :SolverInput) -> Result<(SolverOutput, Vec<(Edge,Edge)>),String> {
    println!("(%) Finding less-than relation");
    let edges_lt = less_than(&input.nodes, &input.edges);
    println!("(%) Resolving top-bottom");
    let portref_changes = resolve_topbottom(&mut input, &edges_lt);
    println!("(%) Transitive reduction of less-than");
    let edges_lt :Vec<EdgePair> = {
        let mut set : HashSet<(usize,usize)> = edges_lt.into_iter().collect();
        trans_red(&mut set);
        set.into_iter().collect()
    };

    let tracks  = solve_diff(input.clone(), edges_lt)?;
    use symbols::place_symbols;



    let mut s_edges = Vec::new();
    for ((pr1,pr2,y),e) in tracks.edge_levels.iter().zip(input.edges.iter()) {
        let (_n,x1,y1) = &tracks.node_coords[pr1.node];
        let (_n,x2,y2) = &tracks.node_coords[pr2.node];
        s_edges.push((e.clone(), conv_line((*x1,*y1),*y,(*x2,*y2))));
	}

    let s_nodes = tracks.node_coords.clone().into_iter()
        .zip(input.nodes.clone().into_iter())
        .map(|((_,x,y),n)| (n,(x,y))).collect::<Vec<_>>();


    let symbols = place_symbols(&s_nodes, &s_edges, &input.symbols)
        .map_err(|_| "Place_symbols error".to_string())?;

    //let symbols = symbols.into_iter().zip(input.symbols.iter().cloned())
    //    .map(|((x,s),(p,t))| (s,x)).collect();
    //
    
    println!("SYMBOLS {:#?}", symbols);

    Ok((SolverOutput { tracks: tracks, symbols: symbols}, portref_changes))
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

fn mk_port_shape(side :EdgeSide, shape :&NodeShape, port :Port, s :Bool) -> PortShape {
    use self::EdgeSide as ES;
    use self::NodeShape as N;
    use self::Port as P;
    use self::Side as S;
    use self::Dir as D;
    let ff = Bool::Const(false);
    let tt = Bool::Const(true);
    match (side,shape,port) {
        (ES::Begin, N::Begin, P::Out) => portsh(ff,tt,ff),
        (ES::End,   N::End  , P::In ) => portsh(ff,tt,ff),

        (ES::Begin, N::Switch { side: S::Left,  dir: D::Up, .. },   P::Left)  => portsh(!s,  s, ff),
        (ES::Begin, N::Switch { side: S::Left,  dir: D::Up, .. },   P::Right) => portsh(ff, !s,  s),
        (ES::Begin, N::Switch { side: S::Right, dir: D::Up, .. },   P::Left)  => portsh(s,  !s, ff),
        (ES::Begin, N::Switch { side: S::Right, dir: D::Up, .. },   P::Right) => portsh(ff,  s, !s),
        (ES::Begin, N::Switch { side: S::Left,  dir: D::Down, .. }, P::Trunk) => portsh(ff, !s,  s),
        (ES::Begin, N::Switch { side: S::Right, dir: D::Down, .. }, P::Trunk) => portsh(s,  !s, ff),

        (ES::End,   N::Switch { side: S::Left,  dir: D::Down, .. }, P::Left)  => portsh(!s,  s, ff),
        (ES::End,   N::Switch { side: S::Left,  dir: D::Down, .. }, P::Right) => portsh(ff, !s,  s),
        (ES::End,   N::Switch { side: S::Right, dir: D::Down, .. }, P::Left)  => portsh( s, !s, ff),
        (ES::End,   N::Switch { side: S::Right, dir: D::Down, .. }, P::Right) => portsh(ff,  s, !s),
        (ES::End,   N::Switch { side: S::Left,  dir: D::Up, .. },   P::Trunk) => portsh(ff, !s,  s),
        (ES::End,   N::Switch { side: S::Right, dir: D::Up, .. },   P::Trunk) => portsh( s, !s, ff),
        
        _ =>  panic!("Invalid edge shape"),
    }
}

pub fn solve_diff(input :SolverInput, edges_lt :Vec<EdgePair>) -> Result<TrackOutput, String> {
    println!("($) solve_diff");
    use diffsolver::*;
    use diffsolver::minisat::*;
    use diffsolver::minisat::unary::*;

    let mut s = SATModDiff::new();

    let nodes = &input.nodes;
    let edges = &input.edges;

    println!("NODES {:?}", nodes);
    println!("EDGES {:?}", edges);
    println!("SYMBOLS {:?}", &input.symbols);

    println!("($) creating representation");
    // representation
    let node_delta_xs = nodes.iter().zip(nodes.iter().skip(1))
        .map(|_| Unary::new(&mut s.sat,2)).collect::<Vec<_>>();
    let node_ys = nodes.iter().enumerate()
        .map(|(i,_)| s.diff.named_var(Some(format!("ny_{}",i)))).collect::<Vec<_>>();
    let edge_ys = edges.iter().enumerate()
        .map(|(i,_)| s.diff.named_var(Some(format!("ey_{}",i)))).collect::<Vec<_>>();
    let edge_short = edges.iter()
        .map(|_| (s.sat.new_lit(), s.sat.new_lit())).collect::<Vec<_>>();
    let slanted = nodes.iter()
        .map(|x| if let NodeShape::Switch {..} = x.shape { s.sat.new_lit() } 
             else { Bool::Const(false) } )
        .collect::<Vec<_>>();


    println!("($) adding constraints");
    // constraint: edges push nodes apart:  sum(delta_x_a .. delta_x_b) >= 1 
    for Edge { a, b } in edges {
        s.sat.add_clause(((a.node) .. (b.node)) .map(|x| node_delta_xs[x].gte_const(1)));
    }

    println!("Edges LT {:?}", edges_lt);
    // constraint: edge ordering
    for (a,b) in &edges_lt {
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
    }



    let zero = s.diff.zero();


    #[derive(PartialEq,Eq)]
    enum EdgeDir { Up, Straight, Down };
    // Find edge dirs
    fn map_portshape(s :&minisat::Model, x :&PortShape) -> EdgeDir {
        if s.value(&x.up) { return EdgeDir::Up; }
        if s.value(&x.straight) { return EdgeDir::Straight; }
        if s.value(&x.down) { return EdgeDir::Down; }
        panic!()
    }

    // minimize kinks
    let max_kinks = {
        let m = s.solve().unwrap();

        // begin.up && end.up == 0
        // begin.up && end.straight == 1.
        // begin.up && end.down == 2
        // begin.straight && end.straight == 0
        // begin.straight && end.up == 1
        // begin.straight && end.down == 1
        // begin.down && end.up == 2
        // begin.down && end.straight == 1
        // begin.down && end.down == 0

        // 1 is straight but not both --  >= 1
        // down/up, up/down           --  >= 2


        let edgedir = edge_shapes.iter().map(|(begin,end)| 
                 (map_portshape(&m.sat, begin),
                  map_portshape(&m.sat, end)) ).collect::<Vec<_>>();

        let max_kinks :usize= edgedir.iter().map(|(a,b)| {
            if *a == *b { return 0; }
            if (*a == EdgeDir::Straight || *b == EdgeDir::Straight)  { return 1; }
            return 2;
        }).sum();

        max_kinks
    };
    println!("(^) max kinks = {}", max_kinks);

    let kinks_unary = edge_shapes.iter().map(|(begin,end)| {
        let x = Unary::new(&mut s.sat, 2);
        // one is straight and the other is not
        //println!("x.gte_consts(1) = {:?}", x.gte_const(1));
        s.sat.add_clause(vec![!begin.straight, end.straight, x.gte_const(1)]);
        s.sat.add_clause(vec![begin.straight, !end.straight, x.gte_const(1)]);
        // down/up or up/down
        //println!("x.gte_consts(2) = {:?}", x.gte_const(2));
        s.sat.add_clause(vec![!begin.up,   !end.down, x.gte_const(2)]);
        s.sat.add_clause(vec![!begin.down, !end.up,   x.gte_const(2)]);
        x

    }).collect::<Vec<_>>();
    let sum_kinks = Unary::sum_truncate(&mut s.sat, kinks_unary, max_kinks+1);
    let (mut lo, mut hi) = (0,max_kinks);
    while lo < hi {
        let mid = (hi-lo)/2 + lo;
        println!("Constraint on {}", mid);
        match s.solve_under_assumptions(&vec![sum_kinks.lte_const(mid as isize)]) {
            Ok(_) => { println!("(^) successful kinks <= {}", mid); hi = mid; }
            Err(_) => { println!("(^) unsuccessful kinks <= {}", mid); lo = mid+1; }
        }
    }

    println!("kinks={}/{}",lo,hi);
    assert_eq!(lo,hi);
    s.sat.add_clause(vec![sum_kinks.lte_const(lo as isize)]);
    s.solve().expect("Could not compress in kinks direction");



    // maximize shortups
    let max_negshorts = {
        let m = s.solve().unwrap();
        let max_negshorts : usize = edge_short.iter().map(|(u,d)|
                if m.sat.value(u) { 0 } else { 1 } +
                if m.sat.value(d) { 0 } else { 1 } ).sum();
        println!("(^) max_negshorts={}", max_negshorts);
        max_negshorts
    };
    let sum_negshorts = Unary::sum_truncate(&mut s.sat, 
            edge_short.iter().flat_map(|(u,d)| vec![!*u,!*d])
            .map(Unary::from_bool).collect(), max_negshorts+1);

    let (mut lo, mut hi) = (0,max_negshorts);
    while lo < hi {
        let mid = (hi-lo)/2 + lo;
        println!("Constraint on {}", mid);
        match s.solve_under_assumptions(&vec![sum_negshorts.lte_const(mid as isize)]) {
            Ok(_) => { println!("(^) successful negshorts <= {}", mid); hi = mid; }
            Err(_) => { println!("(^) unsuccessful negshorts <= {}", mid); lo = mid+1; }
        }
    }

    println!("negshorts={}/{}",lo,hi);
    assert_eq!(lo,hi);
    s.sat.add_clause(vec![sum_negshorts.lte_const(lo as isize)]);
    s.solve().expect("Could not compress in X direction");
    //let x_size = lo;

    //
    // minimize X values
    // count xs
    let max_x = {
        let m = s.solve().unwrap();
        let max_x :usize = node_delta_xs.iter().map(|x| m.sat.value(x)).sum();
        println!("(^) max_x={}", max_x);
        max_x
    };
    let sum_x = Unary::sum_truncate(&mut s.sat, node_delta_xs.clone(), max_x+1);

    let (mut lo, mut hi) = (0,max_x);
    while lo < hi {
        let mid = (hi-lo)/2 + lo;
        println!("Constraint on {}", mid);
        match s.solve_under_assumptions(&vec![sum_x.lte_const(mid as isize)]) {
            Ok(_) => { println!("(^) successful negshorts <= {}", mid); hi = mid; }
            Err(_) => { println!("(^) unsuccessful negshorts <= {}", mid); lo = mid+1; }
        }
    }

    println!("x={}/{}",lo,hi);
    assert_eq!(lo,hi);
    s.sat.add_clause(vec![sum_x.lte_const(lo as isize)]);
    println!("sum_x is now lte {}",lo);
    s.solve().expect("Could not compress in X direction");
    let x_size = lo;
    //
    //
    //  maximise straightness
    // count xs
    let max_negstraight = {
        let m = s.solve().unwrap();
        let edgedir = edge_shapes.iter().map(|(begin,end)| 
                 (map_portshape(&m.sat, begin),
                  map_portshape(&m.sat, end)) ).collect::<Vec<_>>();

        let max :usize= edgedir.iter().map(|(a,b)| {
            (if *a != EdgeDir::Straight { 1 } else { 0 }) +
            (if *b != EdgeDir::Straight { 1 } else { 0 })
        }).sum();

        max
    };

    let negstraights_bits = edge_shapes.iter().flat_map(|(b,e)| vec![b,e])
        .map(|e| Unary::from_bool(!e.straight));
    let sum_negstraight = Unary::sum_truncate(&mut s.sat, 
                         negstraights_bits.collect(), max_negstraight+1);

    let (mut lo, mut hi) = (0,max_negstraight);
    while lo < hi {
        let mid = (hi-lo)/2 + lo;
        println!("Constraint on {}", mid);
        match s.solve_under_assumptions(&vec![sum_negstraight.lte_const(mid as isize)]) {
            Ok(_) => { println!("(^) successful sum_negstraight <= {}", mid); hi = mid; }
            Err(_) => { println!("(^) unsuccessful sum_negstraight <= {}", mid); lo = mid+1; }
        }
    }

    println!("negstraight={}/{}",lo,hi);
    assert_eq!(lo,hi);
    s.sat.add_clause(vec![sum_negstraight.lte_const(lo as isize)]);
    s.solve().expect("Could not compress in negstraight direction");



    // minimzie slant count?
    // 
    //
    let max_slants =  {
        let m = s.solve().unwrap();
        let max_slants :usize = slanted.iter().map(|s| if m.sat.value(s) { 1 } else { 0 }).sum();
        println!("(^) max_slants={}", max_slants);
        max_slants
    };
    let sum_slants = Unary::sum_truncate(&mut s.sat, slanted.iter().cloned().map(Unary::from_bool).collect(), max_slants+1);

    let (mut lo, mut hi) = (0,max_slants);
    while lo < hi {
        let mid = (hi-lo)/2 + lo;
        println!("Constraint on {}", mid);
        match s.solve_under_assumptions(&vec![sum_slants.lte_const(mid as isize)]) {
            Ok(_) => { println!("(^) successful slants <= {}", mid); hi = mid; }
            Err(_) => { println!("(^) unsuccessful slant <= {}", mid); lo = mid+1; }
        }
    }

    println!("n_slanted={}/{}",lo,hi);
    assert_eq!(lo,hi);
    s.sat.add_clause(vec![sum_slants.lte_const(lo as isize)]);
    s.solve().expect("Could not compress in num_slants");
    //
    //
    // first, get the y value

    let get_maxy = |model:&SATDiffModel| {
        let mut y_max = -100_000isize;
        for yvar in &edge_ys {
            let y = model.diff.get_value(*yvar);
            if y > y_max { y_max = y; }
        }

        let mut y_max_vars = Vec::new();
        for yvar in &edge_ys {
            if model.diff.get_value(*yvar) >= y_max {
                y_max_vars.push(*yvar);
            }
        }

        println!("(^) fits inside y={}", y_max);
        (y_max,y_max_vars)
    };

    let (mut y_max,mut y_max_vars) = {
        let model = s.solve().map_err(|_| "Unconstrained solve failed".to_string())?;
        println!("(^) basic solve successful.");
        //print(&model);
        get_maxy(&model)
    };

    // minimize y value

    let mut assumptions = Vec::new();
    loop {
        for v in &y_max_vars {
            println!("(^) Adding constraint v - 0 <= {}", y_max-1);
            let c1 = s.cond_constraint(*v, zero, y_max-1); // v <= y_max -1
            assumptions.push(c1);
        }

        println!("Solving with {} assumptions", assumptions.len());
        match s.solve_under_assumptions(&assumptions) {
            Ok(model) => {

                y_max = y_max - 1;
                y_max_vars = Vec::new();
                for yvar in &edge_ys {
                    if model.diff.get_value(*yvar) >= y_max {
                        y_max_vars.push(*yvar);
                    }
                }

                //print(&model);
                println!("(^) Updated y_max to {} ({:?})", y_max, y_max_vars);
            },
            Err(()) => {
                println!("(^) could not compress Y<{}.",y_max);
                break;
            }
        }

        assumptions.clear();
    }

    // Commit to y = y_max
    for y in &edge_ys {
        let c = s.cond_constraint(*y, zero, y_max); 
        s.sat.add_clause(vec![c]);
    }
    s.solve().expect("Could not compress in Y direction");
    


    println!("(^) ");
    println!("(^) Finished SAT/Diff optimization");
    println!("(^) ");

    let output= {
        {
            let m = s.solve().unwrap();
            let ups :usize = edge_short.iter().map(|e| if m.sat.value(&e.0) { 1 } else { 0 }).sum();
            println!("(^) short_ups={}", ups);
        }
        {
            let m = s.solve().unwrap();
            let downs :usize = edge_short.iter().map(|e| if m.sat.value(&e.1) { 1 } else { 0 }).sum();
            println!("(^) short_downs={}", downs);
        }

        // extract slant values
       
        let sat_gen = s.sat_clauses_generated.clone();
        let m = s.solve().unwrap();
        print(&m);
        let slanted_out = slanted.iter().map(|x| m.sat.value(x)).collect::<Vec<_>>();
        let mut c = 0;
        for (i,x) in slanted_out.iter().enumerate() {
            //println!("node{} slanted={:?}", i,x);
            if *x { c += 1; }
        }
        println!("x count {}", x_size);
        println!("y count {}", y_max);
        println!("slanted count {}", c);


        let edgedir = edge_shapes.iter().map(|(begin,end)| 
                     (map_portshape(&m.sat, begin),
                      map_portshape(&m.sat, end)) ).collect::<Vec<_>>();

        let node_dx_values = node_delta_xs.iter().map(|x| m.sat.value(x)).collect::<Vec<_>>();
        let node_y_values = node_ys.iter().map(|x| m.diff.get_value(*x)).collect::<Vec<_>>();
        let edge_y_values = edge_ys.iter().map(|x| m.diff.get_value(*x)).collect::<Vec<_>>();


        let mut xdiff = Differences::new();
        let x_vars = nodes.iter().enumerate().
            map(|(i,_x)| xdiff.named_var(Some(format!("x{}",i)))).collect::<Vec<_>>();
        let mut ci = 0;
        for (i,dx) in node_dx_values.iter().enumerate() {
            xdiff.add_constraint(ci, x_vars[i], x_vars[i+1], -(*dx as isize));
            ci += 1;
        }
        for (i,(e,(begin,end))) in edges.iter().zip(edgedir.iter()).enumerate() {
            let x1 = x_vars[e.a.node];
            let x2 = x_vars[e.b.node];

            xdiff.add_constraint(ci, x1, x2, -1);
            ci += 1;

            // y distance + y distance
            let mut dist = (node_y_values[e.a.node] - edge_y_values[i]).abs() +
                           (node_y_values[e.b.node] - edge_y_values[i]).abs();

            if  ! ((*begin == EdgeDir::Up   && *end == EdgeDir::Up) ||
                   (*begin == EdgeDir::Down && *end == EdgeDir::Down)) {
               dist += 1;
           }

            xdiff.add_constraint(ci, x1, x2, -dist);
            ci += 1;
        }
        for c in (0..ci) {
            xdiff.enable(c).map_err(|_| "Could not solve X values.".to_string())?;

        }

        let mut max = xdiff.get_value(x_vars[x_vars.len()-1]);
        let zero = xdiff.zero();
        loop {
            println!("(^) Trying to set x={}", max-1);
            xdiff.add_constraint(ci,x_vars[x_vars.len()-1], zero, max-1);
            ci += 1;
            let e = xdiff.enable(ci-1);
            match e {
                Ok(()) => {
                    println!("(^) Success.");
                    max = max-1;
                }
                Err(_) => {
                    println!("(^) Failed.");
                    break;
                }
            }
        }

        let x_values = x_vars.iter().map(|v| xdiff.get_value(*v)).collect::<Vec<_>>();
        println!("Solved for X values: {:?}", x_values);

        println!("(^) total number of diff constraints propagated to SAT: {}", sat_gen);
        

        //solve_linear(&input, edges_lt, slanted_out)
        let mut output = TrackOutput {
            node_coords: Vec::new(),
            edge_levels: Vec::new(),
        };

        for (i,n) in nodes.iter().enumerate() {
            output.node_coords.push((n.name.clone(), x_values[i] as f64, node_y_values[i] as f64));
        }

        for (i,e) in edges.iter().enumerate() {
            output.edge_levels.push((e.a, e.b, edge_y_values[i] as f64));
        }
        output
    };
    s.report_time();
    Ok(output)
}

//pub fn solve_linear(input :&SolverInput, edges_lt :Vec<EdgePair>, slants :Vec<bool>) -> Result<SolverOutput, String> {
//
//    let conf = z3::Config::new();
//    let ctx = z3::Context::new(&conf);
//    let opt = z3::Optimize::new(&ctx);
//
//
//    let nodes = &input.nodes;
//    let edges = &input.edges;
//
//    let node_x = nodes.iter().map(|_| { let x = ctx.fresh_real_const("nx"); opt.assert(&x.ge(&ctx.from_real(0,1))); x}).collect::<Vec<_>>();
//    let node_y = nodes.iter().map(|_| { let y = ctx.fresh_real_const("ny"); opt.assert(&y.ge(&ctx.from_real(0,1))); y}).collect::<Vec<_>>();
//    let edge_y = edges.iter().map(|_| { let y = ctx.fresh_real_const("ny"); opt.assert(&y.ge(&ctx.from_real(0,1))); y}).collect::<Vec<_>>();
//
//    // Node x distance because of edges
//    for e in edges {
//        let x1 = &node_x[e.a.node];
//        let x2 = &node_x[e.b.node];
//        opt.assert(&x1.add(&[&ctx.from_real(1,1)]).le(x2));
//    }
//
//    // Node x distance increasing
//    for i in (0..(node_x.len()-2)) {
//        let x1 = &node_x[i];
//        let x2 = &node_x[i+1];
//        opt.assert(&x1.add(&[&ctx.from_real(0,1)]).le(x2));
//    }
//
//    unimplemented!()
//}


pub fn solve(mut input :SolverInput) -> Result<(TrackOutput, Vec<(Edge,Edge)>) ,String> {
    let conf = z3::Config::new();
    let ctx = z3::Context::new(&conf);
    let opt = z3::Optimize::new(&ctx);


    let edges_lt = less_than(&input.nodes, &input.edges);
    let portref_changes = resolve_topbottom(&mut input, &edges_lt);
    let edges_lt :Vec<EdgePair> = {
        let mut set : HashSet<(usize,usize)> = edges_lt.into_iter().collect();
        trans_red(&mut set);
        set.into_iter().collect()
    };

    let nodes = input.nodes;
    let edges = input.edges;

    let zero_real = ctx.from_real(0,1);     //  0.0
    let one_real  = ctx.from_real(1,1);     //  1.0
    let two_real  = ctx.from_real(2,1);     //  2.0
    let negone_real  = ctx.from_real(-1,1); // -1.0
    let true_bool = || ctx.from_bool(true);
    let false_bool = || ctx.from_bool(false);

    let mut node_data = Vec::new();
    for n in &nodes {
        let x = ctx.fresh_real_const("nx");
        opt.assert(&x.ge(&zero_real));
        let y = ctx.fresh_real_const("ny");
        opt.assert(&y.ge(&zero_real));

        node_data.push(NodeRepr { x, y });
    }

    let mut edge_data = Vec::new();
    for e in &edges {
        let y = ctx.fresh_real_const("ey");
        opt.assert(&y.ge(&zero_real));

        // start end (dy1) and end end (dy2) have 45-degree
        // sections with given length. When going into a top port,
        // 1 is added to the node Y value so that we get room for
        // a vertical section.
        let dy1 = match (&nodes[e.a.node].shape,&e.a.port) {
            (NodeShape::Vertical { .. }, Port::Top) => 
                 y.sub(&[&node_data[e.a.node].y, &one_real]),
            _ => y.sub(&[&node_data[e.a.node].y]),
        };
        let dy2 = match (&nodes[e.b.node].shape,&e.b.port) {
            (NodeShape::Vertical { .. }, Port::Top) => 
                 (node_data[e.b.node].y).add(&[&one_real]).sub(&[&y]),
            _ => (node_data[e.b.node].y).sub(&[&y]),
        };

        let shortup = ctx.fresh_bool_const("e_bothup");
        let shortdown = ctx.fresh_bool_const("e_bothdown");
        opt.assert(&shortup.and(&[&shortdown]).not());

        edge_data.push(EdgeRepr {y, dy1, dy2, shortup, shortdown});
    }

    // Edge space node X
    for (i,e) in edges.iter().enumerate() {
        let node_a_x = &node_data[e.a.node].x;
        let node_b_x = &node_data[e.b.node].x;

        let dist = &one_real;
        opt.assert(&node_a_x.add(&[dist]).le(node_b_x));
    }

    // Node ordering X
    for ((i,na),nb) in node_data.iter().enumerate().zip(node_data.iter().skip(1)) {
        let small = ctx.from_real(0,1);
        opt.assert(&na.x.add(&[&small]).le(&nb.x));
        // KM / MILEAGE DIFF
        // 300 m must be one unit
        //let posdiff = (1.0/(300.0)*(nodes[i+1].pos - nodes[i].pos)) as i32;
        //opt.assert(&na.x.add(&[&ctx.from_real(posdiff,1)]).le(&nb.x));
    }

    // Node shape
    let mut node_slanted :Vec<z3::Ast> = Vec::new();
    for (i,n) in nodes.iter().enumerate() {
        match n.shape {
            NodeShape::Switch {.. } => node_slanted.push(ctx.fresh_bool_const("slanted")),
            _ => node_slanted.push(false_bool()),
        }
    }

    let mut count_straight = Vec::new();
    let mut absys = Vec::new();
    for (i,e) in edges.iter().enumerate() {
        let node_a = &nodes[e.a.node];
        let node_b = &nodes[e.b.node];

        let (edge_out, edge_in, absdy1factor, absdy2factor) = {
            use self::NodeShape::*;
            use self::Port::*;
            let a_slanted = node_slanted[e.a.node].and(&[&true_bool()]);
            let a_straight = a_slanted.not();
            let b_slanted = node_slanted[e.b.node].and(&[&true_bool()]);
            let b_straight = b_slanted.not();

            let absdy1factor = match (&node_a.shape, &e.a.port) {
                (Switch { dir: Dir::Up, ..  },                   Port::Right) => -1,
                (Switch { side: Side::Left,  dir: Dir::Down, .. }, Port::Trunk) => -1,
                (Vertical { .. }, Port::Bottom) => -1,
                _ => 1,
            };
            let absdy2factor = match (&node_b.shape, &e.b.port) {
                (Switch { dir: Dir::Down, ..  },                   Port::Right) => -1,
                (Switch { side: Side::Left,  dir: Dir::Up, .. }, Port::Trunk) => -1,
                (Vertical { .. }, Port::Top) => -1,
                _ => 1,
            };

            let edge_out = match (&node_a.shape, &e.a.port) {

                (Begin, Port::Out) => 
                    Dirs { is_straight: true_bool(), .. Dirs::new(&ctx) },
                    
                (Switch { side: Side::Left, dir: Dir::Up, .. }, Port::Left) => 
                    Dirs { is_up: a_straight, is_straight: a_slanted, .. Dirs::new(&ctx) },
                (Switch { side: Side::Left, dir: Dir::Up, .. }, Port::Right) => 
                    Dirs { is_straight: a_straight, is_down: a_slanted, .. Dirs::new(&ctx) },
                (Switch { side: Side::Right, dir: Dir::Up, .. }, Port::Right) => 
                    Dirs { is_down: a_straight, is_straight: a_slanted, .. Dirs::new(&ctx) },
                (Switch { side: Side::Right, dir: Dir::Up, .. }, Port::Left) => 
                    Dirs { is_straight: a_straight, is_up: a_slanted, .. Dirs::new(&ctx) },

                (Switch { side: Side::Left, dir: Dir::Down, .. }, Port::Trunk) => 
                    Dirs { is_straight: a_straight, is_down: a_slanted, .. Dirs::new(&ctx) },
                (Switch { side: Side::Right, dir: Dir::Down, .. }, Port::Trunk) => 
                    Dirs { is_straight: a_straight, is_up: a_slanted, .. Dirs::new(&ctx) },
                (Vertical { .. }, Port::Top) => 
                    Dirs { is_up: true_bool(), .. Dirs::new(&ctx) },
                (Vertical { .. }, Port::Bottom) => 
                    Dirs { is_down: true_bool(), .. Dirs::new(&ctx) },

                _ => panic!("Bad combination"),
            };

            let edge_in = match (&node_b.shape, &e.b.port) {
                (End, In) => 
                    Dirs { is_straight: true_bool(), .. Dirs::new(&ctx) },

                    // Down
                (Switch { side: Side::Left, dir: Dir::Down, ..}, Port::Left) =>
                    Dirs { is_up: b_straight, is_straight: b_slanted, .. Dirs::new(&ctx) },
                (Switch { side: Side::Left, dir: Dir::Down, ..}, Port::Right) =>
                    Dirs { is_straight: b_straight, is_down: b_slanted, .. Dirs::new(&ctx) },
                (Switch { side: Side::Right, dir: Dir::Down, ..}, Port::Right) =>
                    Dirs { is_down: b_straight, is_straight: b_slanted, .. Dirs::new(&ctx) },
                (Switch { side: Side::Right, dir: Dir::Down, ..}, Port::Left) =>
                    Dirs { is_straight: b_straight, is_up: b_slanted, .. Dirs::new(&ctx) },

                    // Up
                (Switch { side: Side::Right, dir: Dir::Up, .. }, Port::Trunk) => 
                    Dirs { is_straight: b_straight, is_up: b_slanted, .. Dirs::new(&ctx) },
                (Switch { side: Side::Left, dir: Dir::Up, .. }, Port::Trunk) => 
                    Dirs { is_straight: b_straight, is_down: b_slanted, .. Dirs::new(&ctx) },
                (Vertical { .. }, Port::Top) => 
                    Dirs { is_down: true_bool(), .. Dirs::new(&ctx) },
                (Vertical { .. }, Port::Bottom) => 
                    Dirs { is_up: true_bool(), .. Dirs::new(&ctx) },

                _ => panic!("Bad combo"),
            };


            (edge_out,edge_in, absdy1factor, absdy2factor)
        };

        let shortup = &edge_data[i].shortup;
        let shortdown = &edge_data[i].shortdown;

        opt.assert(&shortup.implies(&edge_out.is_up.and(&[ &edge_in .is_up])));
        opt.assert(&shortdown.implies(&edge_out.is_down.and(&[ &edge_in .is_down])));

        // if end is straihgt, dy = 0
        opt.assert(&edge_out.  is_straight.implies(&edge_data[i].dy1._eq(&zero_real)));
        opt.assert(&edge_in.  is_straight.implies(&edge_data[i].dy2._eq(&zero_real)));

        // if end is down, dy <= 0.0, it must also be <= -1.0, see below
        opt.assert(&edge_out.  is_down.implies(&edge_data[i].dy1.le(&zero_real)));
        opt.assert(&edge_in.  is_down.implies(&edge_data[i].dy2.le(&zero_real)));
        // Both down -> dy1+dy2 < -1
        opt.assert(&shortdown.implies(&edge_data[i].dy1.add(&[&edge_data[i].dy2]).le(&negone_real)));
        // Leftmost end down and not both -> dy1 < -1
        opt.assert(&edge_out.is_down.and(&[&shortdown.not()]).implies(&edge_data[i].dy1.le(&negone_real)));
        // Rightmost end down and not both -> dy2 < -1
        opt.assert(&edge_in .is_down.and(&[&shortdown.not()]).implies(&edge_data[i].dy2.le(&negone_real)));


        // if end is up, dy >= 0.0, it must also be >= 1.0, see below
        opt.assert(&edge_out.  is_up.implies(&edge_data[i].dy1.ge(&zero_real)));
        opt.assert(&edge_in.  is_up.implies(&edge_data[i].dy2.ge(&zero_real)));
        // Both up -> dy1+dy2 > 1
        opt.assert(&shortup.implies(&edge_data[i].dy1.add(&[&edge_data[i].dy2]).ge(&one_real)));
        // Left most up and not both -> dy1 > 1
        opt.assert(&edge_out.is_up.and(&[&shortup.not()]).implies(  &edge_data[i].dy1.ge(&one_real)));
        // Rightmost up and not both -> dy2 > 1
        opt.assert(&edge_in.is_up.and(&[&shortup.not()]).implies(  &edge_data[i].dy2.ge(&one_real)));


        // use sum of abs of dy1 and dy2 as min x-dist between nodes
        let x1 = &node_data[e.a.node].x;
        let x2 = &node_data[e.b.node].x;

        let absdy1 = edge_data[i].dy1.mul(&[&ctx.from_real(absdy1factor,1)]);
        let absdy2 = edge_data[i].dy2.mul(&[&ctx.from_real(absdy2factor,1)]);

        let absy = absdy1.add(&[&absdy2]);
        opt.assert(&x1.add(&[&absy]).le(x2));

        // If not same dir, then add one
        let upup = &edge_in.is_up.  and(&[&edge_out.is_up]);
        let dndn = &edge_in.is_down.and(&[&edge_out.is_down]);
        opt.assert(&upup.or(&[&dndn]).not().implies( &x1.add(&[&absy, &one_real]).le(x2) ));

        absys.push(absy);


        // X distnace is affected by short
        let is_short = shortdown.or(&[shortup]);
        opt.assert(&is_short.implies(&x1.add(&[&one_real]).ge(x2)));
        opt.assert(&is_short.not().implies(&x1.add(&[&one_real]).le(x2)));

        count_straight.push(edge_out.is_straight);
        count_straight.push(edge_in.is_straight);

    }

    // Edge Y ordering
    println!("Edge Y ordering size {:?}", edges_lt.len());
    for (a,b) in edges_lt {
        opt.assert(&edge_data[a].y.le(&edge_data[b].y));
        let short = edge_data[a].shortdown.or(&[&edge_data[b].shortup]);
        opt.assert(&short.not().implies(&edge_data[a].y.add(&[&one_real]).le(&edge_data[b].y)));
    }

    // First minimize slant
    //let absys_ref = absys.iter().collect::<Vec<_>>();
    //opt.minimize(&zero_real.add(&absys_ref));

    // Then minimize size of drawing
    //let node_xs = node_data.iter().map(|n| &n.x).collect::<Vec<&z3::Ast>>();
    //let node_ys = node_data.iter().map(|n| &n.y).collect::<Vec<&z3::Ast>>();
    //opt.minimize(&zero_real.add(&node_xs).add(&node_ys));

    //// Then minimize height
    //let edge_ys = edge_data.iter().map(|e| &e.y).collect::<Vec<&z3::Ast>>();
    //opt.minimize(&zero_real.add(&edge_ys));
    
    // Something goes wrong sometimes when adding these multiple
    // optimization criteria. Output is: "Error: Z3 exception".
    // Possibly a bug in Z3?
    //
    // Instead, we can minimize on a single criterion, the sum of the above criteria
    let node_xs = node_data.iter().map(|n| &n.x).collect::<Vec<&z3::Ast>>();
    let node_ys = node_data.iter().map(|n| &n.y).collect::<Vec<&z3::Ast>>();
    let edge_ys = edge_data.iter().map(|e| &e.y).collect::<Vec<&z3::Ast>>();
    let absys_ref = absys.iter().collect::<Vec<_>>();

    let slants = node_slanted.iter().map(|b| b.ite(&ctx.from_real(1,1),&ctx.from_real(0,1))).collect::<Vec<z3::Ast>>();
    let slants_ref = slants.iter().collect::<Vec<_>>();

    let n_straight = count_straight.iter().map(|b| b.ite(&ctx.from_real(1,1),&ctx.from_real(0,1))).collect::<Vec<z3::Ast>>();
    let n_straight_ref = n_straight.iter().collect::<Vec<_>>();


    opt.minimize(&zero_real
                 .add(&[&zero_real.add(&node_xs).mul(&[&ctx.from_real(100,1)])])
    //             //.add(&node_ys)
                 .add(&[&zero_real.add(&edge_ys).mul(&[&ctx.from_real(1,1)])]));
    //             .add(&[&zero_real.add(&absys_ref).mul(&[&ctx.from_real(10,1)])])
    //             .add(&[&zero_real.add(&n_straight_ref).mul(&[&ctx.from_real(100,1)])]));

    //println!("absys length:  {}", absys_ref.len());


    //println!("Optimize edge_ys");
    //opt.minimize(&zero_real.add(&edge_ys));
    ////println!("Optimize straight switches");
    ////opt.minimize(&zero_real.add(&n_straight_ref));
    ////println!("Optimize absys_ref");
    ////opt.minimize(&zero_real.add(&absys_ref));
    //println!("Optimize width");
    //opt.minimize(&zero_real.add(&node_xs));



    //let absys_val = {
    //    let m = opt.get_model();
    //    m.eval(&zero_real.add(&absys_ref)).unwrap().as_real().unwrap()
    //};
    //println!("absys_val {:?}",absys_val);
    ////opt.assert(&zero_real.add(&absys_ref)._eq(&ctx.from_real(absys_val.0 as i32, absys_val.1 as i32)));


    let zero_real = ctx.from_real(0,1); // 0.0


    let status = opt.check();
    println!("Status: {:?}", status);
    if !status { return Err("Solver failed".to_string()); }
    let model = opt.get_model();

    let mut output = TrackOutput {
        node_coords: Vec::new(),
        edge_levels: Vec::new(),
    };

    for ((i,n),repr) in nodes.into_iter().enumerate().zip(node_data.iter()) {
        let x = model.eval(&repr.x).unwrap().as_real().unwrap();
        let y = model.eval(&repr.y).unwrap().as_real().unwrap();
        println!("node {}: slant={} (x={:?},y={:?})", i, 
                 model.eval(&node_slanted[i]).unwrap().as_bool().unwrap(),x,y);
        output.node_coords.push((n.name, x.0 as f64 / x.1 as f64, 
                                 y.0 as f64 / y.1 as f64));
    }

    for ((i,e),repr) in edges.iter().enumerate().zip(edge_data.iter()) {
        let y = model.eval(&repr.y).unwrap().as_real().unwrap();
        let dy1 = model.eval(&repr.dy1).unwrap().as_real().unwrap();
        let dy2 = model.eval(&repr.dy2).unwrap().as_real().unwrap();
        let shortup = model.eval(&repr.shortup).unwrap().as_bool().unwrap();
        let shortdown = model.eval(&repr.shortdown).unwrap().as_bool().unwrap();
        println!("edge {}: {:?} {:?} {:?} {:?} {:?} ", i,y, dy1,dy2,shortup,shortdown);
        output.edge_levels.push((e.a,e.b,y.0 as f64 / y.1 as f64));
    }

    Ok((output,portref_changes))
}





pub fn conv_line((x1,y1) :(f64,f64), l :f64, (x2,y2) :(f64,f64)) -> Vec<(f64,f64)> {
    let dx1 = (y1-l).abs();
    let dx2 = (y2-l).abs();
    
    let mut line = Vec::new();
    
    let p1 = (x1,y1);
    let p2 = (x1+dx1, l);
    let p3 = (x2-dx2, l);
    let p4 = (x2,y2);
    
    line.push(p1);
    if (p2.0-p1.0)*(p2.0-p1.0) + 
       (p2.1-p1.1)*(p2.1-p1.1) > 1e-5 { line.push(p2); }
    if (p3.0-p2.0)*(p3.0-p2.0) + 
       (p3.1-p2.1)*(p3.1-p2.1) > 1e-5 { line.push(p3); }
    if (p4.0-p3.0)*(p4.0-p3.0) + 
       (p4.1-p3.1)*(p4.1-p3.1) > 1e-5 { line.push(p4); }

    line
}

