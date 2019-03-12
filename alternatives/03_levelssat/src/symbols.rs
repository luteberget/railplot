use parser::*;
use solver::*;
use ordered_float::OrderedFloat;


/// Try to place the list of symbols on the given track plan.
/// Can fail if there is not enough space, in case an interval
/// of node indices are given to indicate where the space is 
/// insufficient. On success, returns insertion points for 
/// each symbol.
pub fn place_symbols(nodes :&[(Node,Pt)], edges :&[(Edge,Vec<Pt>)], symbols :&[(EdgeRef,Symbol)]) -> Result<Vec<((EdgeRef,Symbol),(Pt,Pt))>, (usize,usize)> {

    println!("place_symbols");
    use lp_modeler::problem::{LpObjective, Problem, LpProblem};
    use lp_modeler::operations::{LpOperations};
    use lp_modeler::variables::{LpInteger, LpContinuous, lp_sum, LpExpression};
    use lp_modeler::solvers::{SolverTrait, CbcSolver};
    let constant = |x:f64| LpExpression::LitVal(x as f32);

    let mut lp = LpProblem::new("symbols", LpObjective::Minimize);

    let x_vars = symbols.iter().enumerate().map(|(i,_)| LpContinuous::new(&format!("s{}", i)))
        .collect::<Vec<_>>();

    let absdiff_vars = symbols.iter().enumerate().map(|(i,_)| LpContinuous::new(&format!("d{}", i)))
        .collect::<Vec<_>>();


    // Constraints:
    //
    // Split edges on bends! Bends are nodes. Assign them abspos.
    // OR: heuristic approach: solve first, if any symbols are 
    //     in bend pits, choose to place it before or after the bend,
    //     whichever is closer.
    //  TODO: ignoring this for now ...
    //
    // On edge:
    //   1. symbol-left is right-of start of edge + edge-disallowed -> (c2)
    //   2. symbol-right is left-of end of edge - edge-disallowed   -> (c2)
    //   edge-disallowed can be +INF if the next edge continues straight
    //
    // Sequence of symbols by abspos
    //   3. ordered by symbol_ip_abspos -> (c1)
    //   4. respect nodes' abspos       -> (c1)
    //
    // Sequence of symbols on edge-level by abspos
    //   4. symbol1-right is left-of symbol2-left   -> (c2)
    //   if edge continues (edge-disallowed = +INF) then this also goes for that.
    //
    // Optimize:
    //   sum(symbol_defaultpos - symbol_ip_abspos)
    //
    //
    // TODO linking / autolinking
    //    i.e. two signals which have almost the same abspos
    //     should have the same x pos.
    //    this can be explicit input or derived from some heuristic.



    #[derive(Debug)]
    enum CV { Const, Var };

    // (c1) global order
    {
        let mut v = Vec::new();
        for ((_e,s),var) in symbols.iter().zip(x_vars.iter()) {
            v.push((s.abspos, CV::Var, var.into()));
        }

        for (n,(x,_)) in nodes.iter() {
            v.push((n.pos, CV::Const, constant(*x)));
        }

        v.sort_by_key(|(x,_,_)| OrderedFloat(*x));
        for ((_x1, cv1, e1),(_x2, cv2, e2)) in v.iter().zip(v.iter().skip(1)) {
            match (cv1,cv2) {
                (CV::Const,CV::Const) => {}, // dont add const constraint
                _ => { lp += (e1).le(e2); },
            }
        }
    }

    println!("place_symbols c1 done");

    // (c2) edge-class order and bounds
    {
        use std::collections::HashMap;

        let mut sameedgesabove = Vec::new();
        let mut sameedgesbelow = Vec::new();

        type Dings = (f64, CV, (LpExpression,LpExpression)); // (abspos,(left_var,right_var))
        let mut edgelevelsymbols : HashMap<(usize,isize),Vec<Dings>> = HashMap::new();

        {
            let mut end = |ei:usize,dirfactor:isize,offset,above:isize| {

                let pt :Pt = if dirfactor > 0 { (edges[ei].1)[0] } else { *(edges[ei].1).last().unwrap() };

                for l in (1..=2)  {
                    let x = pt.0 + (dirfactor as f64)*(l as f64)*offset;
                    println!("edge {} level {} x {}", ei, l, x);
                    edgelevelsymbols.entry((ei,above*l)).or_insert(Vec::new()).
                        push((-(dirfactor as f64) *1e6, CV::Const, (constant(x),constant(x))));

                }
            };

            let inv_dir = |d| if d == Dir::Up { Dir::Down } else { Dir::Up };
            let find_edge = |dir,ni,p| {
                if dir == Dir::Up {
                    edges.iter().enumerate().find_map(|(i,(Edge { a, .. },_))| 
                          if a.node == ni && p == a.port { Some(i) } else { None }).unwrap()
                } else {
                    edges.iter().enumerate().find_map(|(i,(Edge { b, .. },_))| 
                          if b.node == ni && p == b.port { Some(i) } else { None }).unwrap()
                }
            };


            // For each node
            for (i,(n,p)) in nodes.iter().enumerate() {
                match n.shape {
                    NodeShape::Switch {side, dir, ..} => {
                        let leftedge  = find_edge(dir,i,Port::Left);
                        let rightedge = find_edge(dir,i,Port::Right);
                        let trunkedge = find_edge(inv_dir(dir),i,Port::Trunk);
                        println!("sw {} l{} r{} t{}", i,leftedge,rightedge,trunkedge);
                        match dir {
                            Dir::Up   => {
                                end(rightedge, 1, 0.25, 1); // right above
                                end(leftedge,  1, 0.25, -1); // left below
                            }
                            Dir::Down => {
                                end(leftedge,   -1, 0.25, 1);
                                end(rightedge,  -1, 0.25, -1);
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
                    NodeShape::Begin => { println!("begin"); 
                        end(find_edge(Dir::Up, i, Port::Out),  1, 0.0, 1); 
                        end(find_edge(Dir::Up, i, Port::Out),  1, 0.0, -1); 
                    },
                    NodeShape::End   => { println!("end"); 
                        end(find_edge(Dir::Down, i, Port::In), -1, 0.0, 1); 
                        end(find_edge(Dir::Down, i, Port::In), -1, 0.0, -1); 
                    },

                    _ => panic!("Vertical nodes not implemented for symbols."),
                }
            }
        }

        println!("nodes ok");

        for (i,(e,s)) in symbols.iter().enumerate() {
            let left  = LpExpression::ConsCont(x_vars[i].clone()) + constant(-s.origin_x);
            let right = LpExpression::ConsCont(x_vars[i].clone()) + constant(-s.origin_x + s.width);
            edgelevelsymbols.entry((*e, s.level))
                .or_insert(Vec::new()).push((s.abspos, CV::Var,  (left,right)));
        }

        println!("els ok");
        println!("above {:?}", sameedgesabove);
        println!("below {:?}", sameedgesbelow);

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
        println!("els2 ok");

        for (k,v) in els2 {
            let mut va = v;
            va.sort_by_key(|(x,_,_)| OrderedFloat(*x));
            println!("sorted {:?} {:?}",k,va);

            for ((_p1,cv1,(_l1,r1)),(_p2,cv2,(l2,_r2))) in va.iter().zip(va.iter().skip(1)) {
                match (cv1,cv2) {
                    (CV::Const,CV::Const) => {}, // Don't add constant expression constraint.
                    (CV::Const,_) => { 
                        println!(" ADDing {:?} >= {:?}", l2,r1);
                        lp += l2.ge(r1);// r1.le(l2); 
                    },
                    _ => {
                        println!(" ADDing {:?} <= {:?}", r1, l2);
                        lp += r1.le(l2); 
                    }
                }
            }
            println!("added constraints");
        }

        println!("els2 constraints ok");
    }

    println!("place_symbols c2 done");


    // optimize
    // calculate the default position for each symbol and 
    // sum the absolute value of the difference between 
    // actual and default position
    //
    for (i,(_e,s)) in symbols.iter().enumerate() {

        let node_idx_prev = match nodes.binary_search_by_key(&s.abspos.into(), 
                                                             |&(ref n,_)| OrderedFloat(n.pos)) {
            Ok(x) => x,
            Err(x) => x,
        };

        let offset = (s.abspos - nodes[node_idx_prev].0.pos)  / 
                     (nodes[node_idx_prev+1].0.pos - nodes[node_idx_prev+0].0.pos);
        let default_pos = (nodes[node_idx_prev].1).0 + offset;

        // Absolute value optimization is implemented as linear programming in this way:
        // (p1-x1) <= x'
        // (x1-p1) <= x'
        // min x'
            println!("ADDING {:?} ", (constant(default_pos) - &x_vars[i])
            .le(&absdiff_vars[i]));
        lp += 
            (constant(default_pos) - &x_vars[i])
            .le(&absdiff_vars[i]);
        lp += (LpExpression::ConsCont(x_vars[i].clone()) - constant(default_pos))
            .le(&absdiff_vars[i]);

    }
    // objective
    lp += lp_sum(&absdiff_vars);

    // Specify solver
    let solver = CbcSolver::new();
    use lp_modeler::problem::LpFileFormat;
    println!("Problem {}", lp.to_lp_file_format());

    // Run optimisation and process output hashmap
    let mut xs = vec![0.0f32;symbols.len()];
    match solver.run(&lp) {
        Ok((status, var_values)) => {
            println!("Status {:?}", status);
            for (name, value) in var_values.iter() {
                println!("value of {} = {}", name, value);
                if name.chars().nth(0).unwrap() == 's' {
                    let i = name[1..].parse::<usize>().unwrap();
                    xs[i] = *value as f32;
                }
            }
        },
        Err(msg) => println!("{}", msg),
    }

    println!("xs {:?}", xs);

    // TODO calc x/y value and rotation?
    let mut output : Vec<((EdgeRef,Symbol),(Pt,Pt))>= Vec::new();


    fn lerp2((x0,y0) :Pt, (x1,y1) :Pt, s :f64) -> Pt {
        (x0 + s * (x1 - x0),
         y0 + s * (y1 - y0))
    }

    fn line_pt_at_x(l :&[Pt], x: f64) -> Result<Pt,()> {
        l.binary_search_by_key(&OrderedFloat(x), |(x0,y0)| OrderedFloat(*x0))
         .map(|i| l[i]).or_else(|i| {
             if i == 0 || i == l.len() { Err(()) }
             else { Ok(lerp2(l[i-1],l[i], (x - l[i-1].0) / (l[i].0 - l[i-1].0))) }
         })
    }

    fn rot90((x,y) :Pt) -> Pt { (-y,x) }
    fn addpt((x0,y0):Pt,(x1,y1):Pt) -> Pt { (x0+x1,y0+y1) }
    fn scale(s:f64, (x,y):Pt) -> Pt { (s*x,s*y) }

    fn line_tangent_at_x(l :&[Pt], x :f64) -> Result<Pt,()> {
        l.binary_search_by_key(&OrderedFloat(x), |(x0,y0)| OrderedFloat(*x0))
            .map(|i| l[i]).or_else(|i| {
                if i == 0 || i == l.len() { Err(()) }
                else { 
                    let (dx,dy) = (l[i].0-l[i-1].0, l[i].1-l[i-1].1);
                    let len = (dx*dx+dy*dy).sqrt();
                    Ok((dx/len,dy/len))
                }})
    }


    for (i,(ei,s)) in symbols.iter().enumerate() {
        let line_pt = line_pt_at_x(&edges[*ei].1, xs[i] as f64).unwrap();
        let line_tangent  = line_tangent_at_x(&edges[*ei].1, xs[i] as f64).unwrap();
        let pt = addpt(line_pt, scale(0.25*(s.level as f64),rot90(line_tangent)));

        output.push(((*ei,s.clone()),(pt,line_tangent)));
    }

    Ok(output)
}
