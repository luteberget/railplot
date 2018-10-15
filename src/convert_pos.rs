// new conversion from D-graph to pos / solver input
//
use rolling::get_infrastructure;
use rolling::input::staticinfrastructure::*;
use solver::SolverInput;
use std::path::Path;
use failure::Error;
use std::collections::HashSet;
use std::collections::HashMap;

use z3;

fn is_switch(n :&Node) -> bool {
    use rolling::input::staticinfrastructure::Edges::*;
    match n.edges {
        Switchable(_) => true,
        _ => false,
    }
}

pub fn turn_nodes(inf :&StaticInfrastructure) -> Result<Vec<(usize,usize)>, Error> {
    let mut node_pairs = HashSet::new();
    for (i,n) in inf.nodes.iter().enumerate() {
        if i < n.other_node {
            node_pairs.insert((i,n.other_node));
        }
    }

    let node_pairs :Vec<(usize,usize)> = node_pairs.into_iter().collect();

    let conf = z3::Config::new();
    let ctx = z3::Context::new(&conf);
    let opt = z3::Optimize::new(&ctx);

    let mut sws = Vec::new();
    let mut node_up_pos = HashMap::new();
    for (a,b) in &node_pairs {
        let a_up  = ctx.fresh_bool_const("a_up");
        let b_up  = ctx.fresh_bool_const("b_up");
        let ab_sw = if is_switch(&inf.nodes[*a]) || is_switch(&inf.nodes[*b]) {
            ctx.from_bool(false)
        } else {
            ctx.fresh_bool_const("ab_sw")
        };
        let n_pos = ctx.fresh_real_const("pos");
        opt.assert(&n_pos.ge(&ctx.from_real(0,1)));

        // a_up = b_up ^ ab_sw
        opt.assert(&a_up._eq(&b_up.not().xor(&ab_sw)));
        //node_pair_data.push((&n_pos, &a_up,&ab_sw,&b_up));
        node_up_pos.insert(*a, (a_up, ctx.from_real(0,1).add(&[&n_pos])));
        node_up_pos.insert(*b, (b_up, n_pos));
        sws.push(ab_sw);
    }

    for (i,_) in inf.nodes.iter().enumerate() {
        use rolling::input::staticinfrastructure::Edges::*;
        let mut mk = |a,b,d| {
            let (a_up,a_pos) = &node_up_pos[&a];
            let (b_up,b_pos) = &node_up_pos[&b];
            opt.assert(&a_up._eq(&b_up.not()));
            opt.assert(&a_up.implies(&a_pos.add(&[&ctx.from_real(1, 1)]).le(b_pos)));
            opt.assert(&a_up.not().implies(&b_pos.add(&[&ctx.from_real(1, 1)]).le(a_pos)));
        };
        match inf.nodes[i].edges {
            Single(o,d) => mk(i,o,d),
            Switchable(id) => {
                if let StaticObject::Switch { left_link, right_link, .. } = &inf.objects[id] {
                    mk(i,left_link.0, left_link.1);
                    mk(i,right_link.0, right_link.1);
                } else {
                    panic!("Not a switch");
                }
            },
            _ => {},
        };
    }

    let zero_real = ctx.from_real(0,1);
    let one_real = ctx.from_real(1,1);
    let sw_count = sws.iter().map(|x| x.ite(&one_real,&zero_real)).collect::<Vec<z3::Ast>>();
    let a = ctx.from_real(0,1).add(&sw_count.iter().collect::<Vec<&z3::Ast>>());
    //let b = ctx.from_real(0,1).add(&node_up_pos.iter().map(|(_,(_,p))| p).collect::<Vec<&z3::Ast>>());
    opt.minimize(&a);
    //opt.minimize(&b);
    //opt.minimize(&a.mul(&[&ctx.from_real(50000,1)]).add(&[&b]));

    let status = opt.check();
    if !status { panic!("Solver failed"); }
    let model = opt.get_model();
    let mut max_pos = 0.0;

    for (i,_) in inf.nodes.iter().enumerate() {
        let (up,pos) = &node_up_pos[&i];
        let up = model.eval(up).unwrap().as_bool().unwrap();
        let pos = model.eval(pos).unwrap().as_real().unwrap();
        let pos_f = (pos.0 as f64) / (pos.1 as f64);
        if pos_f > max_pos { max_pos = pos_f; }
        println!("node {}: up={} pos={:?} --- {:?}", i,up,pos,inf.nodes[i]);
    }

    for (i,(a,b)) in node_pairs.iter().enumerate() {
        let sw = &sws[i];
        let sw = model.eval(sw).unwrap().as_bool().unwrap();
        println!("node pair {} {}: switch={:?}", a,b,sw);
    }

    let n_sw = node_pairs.iter().enumerate().map(|(i,_)| if model.eval(&sws[i]).unwrap().as_bool().unwrap() { 1 } else  {0} ).sum::<isize>();
    println!("Number of turns {}", n_sw);
    println!("Max pos : {}", max_pos);

    let turn_node_pairs = node_pairs.iter().enumerate().filter_map(|(i,p)| Some(*p).filter(|_| model.eval(&sws[i]).unwrap().as_bool().unwrap())).collect();
    Ok(turn_node_pairs)
}
