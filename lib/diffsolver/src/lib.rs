pub extern crate minisat;
extern crate time;
use time::{PreciseTime, Duration};

use minisat::{Bool,Lit};
use log::{debug,  trace};

use std::hash::Hash;
use std::collections::HashMap;
use std::collections::VecDeque;
use std::fmt::Debug;

type CAddr = (usize,usize,usize);
//const INF :f64 = 1e7;
const INF :isize = 10_000_000;

// Unsatisfiability is reported as a list of edges
// constituting a negative cycle in the graph of 
// vars/constraints.
type Unsat<CId> = Vec<CId>;

#[derive(Copy,Clone,Debug)]
pub struct DVar(usize);
pub struct Differences<CId> {
    vars  :Vec<Option<String>>,
    constraints  :HashMap<CId,CAddr>,
    edges        :Vec<HashMap<usize, Edges<CId>>>,
    distance_map :Vec<isize>,
}

pub struct Edges<CId> {
    weight: isize,
    constraints: Vec<Constraint<CId>>,
}

#[derive(Copy,Clone, Debug)]
pub struct Constraint<CId> {
    id :Option<CId>,
    active :bool,
    weight :isize,
}

impl<CId:Copy+Hash+Eq+Debug> Differences<CId> {
    pub fn new() -> Self {
        let mut d = Differences { 
            vars: Vec::new(),
            constraints: HashMap::new(),
            edges: Vec::new(),
            distance_map: Vec::new(),
        };
        let _x0 = d.new_var();
        d
    }

    pub fn num_vars(&self) -> usize { self.vars.len() }
    pub fn num_constraints(&self) -> usize { self.constraints.len() }

    pub fn zero(&self) -> DVar { DVar(0) }

    pub fn new_var(&mut self) -> DVar { self.named_var(None) }

    pub fn named_var(&mut self, name :Option<String>) -> DVar {
        let v = DVar(self.vars.len());
        self.vars.push(name);
        self.edges.push(HashMap::new());
        self.distance_map.push(0);
        if v.0 > 0 {
            // add zero edge
            self.edges[0].insert(v.0, Edges { weight: 0, constraints: 
                vec![Constraint { id: None, active: true, weight: 0 }] });
        }
        v
    }

    pub fn add_constraint(&mut self, id :CId, DVar(x) :DVar, DVar(y) :DVar, k :isize) {
        if self.constraints.contains_key(&id) || x >= self.vars.len() || y >= self.vars.len() {
            panic!("Inconsistent ids in new_constraint.");
        }

        let edge = self.edges[x].entry(y)
            .or_insert(Edges { weight: INF, constraints: vec![] });

        edge.constraints.push(Constraint { id: Some(id), active: false, weight: k });
        let idx = edge.constraints.len() -1;

        self.constraints.insert(id, (x,y,idx));
        trace!("Constraints: {:?}", self.constraints);
    }

    pub fn is_enabled(&self, id :CId) -> &bool {
        let (x,y,i) = self.constraints[&id];
        &self.edges[x][&y].constraints[i].active
    }

    pub fn disable(&mut self, id :CId) {
        let (x,y,i) = self.constraints[&id];
        trace!("  (T) test-disabling {:?}", (id,(x,y,i)));
        let a = &mut (*self.edges[x].get_mut(&y).unwrap()).constraints[i].active;
        if !*a { return; }
        *a = false;
        trace!("  (T) disabling {:?}", (id,(x,&self.vars[x],y,&self.vars[y],i)));

        // Update weight from all active
        let mut w = INF;
        for &Constraint { active, weight, .. } in &self.edges[x][&y].constraints {
            if active && weight < w {
                w = weight;
            }
        }
        (*self.edges[x].get_mut(&y).unwrap()).weight = w;
    }

    pub fn enable(&mut self, id :CId) -> Result<(), Unsat<CId>> {
        let (x,y,i) = self.constraints[&id];
        trace!("  (T) test-enabling {:?}", (id,(x,y,i)));
        let a = &mut (*self.edges[x].get_mut(&y).unwrap()).constraints[i].active;
        if *a { return Ok(()); }
        *a = true;
        trace!("  (T) enabling {:?}", (id,(x,&self.vars[x],y,&self.vars[y],i)));

        let old_w = self.edges[x][&y].weight;
        let w = self.edges[x][&y].constraints[i].weight;
        if w < self.edges[x][&y].weight {
            trace!("  (T) enabling updated weight: {}->{}", 
                     &self.edges[x][&y].weight, w);
            (*self.edges[x].get_mut(&y).unwrap()).weight = w;
        } else {
            trace!("  (T) enabling did not update w: {}->{}", 
                     &self.edges[x][&y].weight, w);
            (*self.edges[x].get_mut(&y).unwrap()).constraints[i].active = true;
            return Ok(());
        }

        match self.detect_cycle(x,y) {
            Ok(dists) => {
                // Consistent constraints, so we can update distance map.
                self.distance_map = dists;
                (*self.edges[x].get_mut(&y).unwrap()).constraints[i].active = true;
                Ok(())
            },
            Err(parents) => {

                // Backtrack through constraints graph finding a negative cycle
                let mut unsat = vec![];
                trace!("  (T) Backtracking through parent list {:?}", parents);
                let (mut a, mut b) = (x,y);
                loop {
                    let (mut w, mut i) = (INF, None);
                    trace!("  (T) backtrack {:?}", &self.edges[a][&b].constraints); 
                    for &Constraint { active, weight, id } in &self.edges[a][&b].constraints {
                        if active && weight < w {
                            w = weight;
                            i = Some(id)
                        }
                    }

                    trace!("  (T) backtracking {} {} {:?} {:?} {} {}", a,b,&self.vars[a], &self.vars[b],x,y);
                    if let Some(c) = i.expect(&format!("did not find constraint for {} {}",a,b)) {
                        unsat.push(c);
                        trace!("  (T) backtracking c{:?}", c);
                    }
                    b = a;
                        a = parents[a].unwrap();
                        if (a,b) == (x,y) { break; }
                }
                trace!("  (T) unsat: {:?}", unsat);

                // Revert the update of the weight matrix
                (*self.edges[x].get_mut(&y).unwrap()).weight = old_w;
                (*self.edges[x].get_mut(&y).unwrap()).constraints[i].active = false;

                Err(unsat)
            }
        }
    }

    fn get_weight(&self, u:usize, v:usize) -> isize {
        self.edges[u][&v].weight
    }

    fn detect_cycle(&self, u:usize, v:usize) -> Result<Vec<isize>, Vec<Option<usize>>> {
        trace!("  (T) detect_cycle({},{})", u,v);
        if !(self.distance_map[v] > self.distance_map[u] + self.get_weight(u,v)) {
            trace!("  (T) detect_cycle: no update");
            return Ok(self.distance_map.clone());
        }
        trace!("  (T) detect_cycle: update!");

        let mut d = self.distance_map.clone();
        let mut p = vec![None; self.distance_map.len()];
        let mut queue = VecDeque::new();
        d[v] = d[u] + self.get_weight(u,v);
        p[v] = Some(u);
        trace!("  (T) detect_cycle: going from {}", v);
        queue.push_back(v);

        while let Some(x) = queue.pop_front() {
            trace!("  (T) detect_cycle: pop {}", x);
            // TODO slight optimization to have a separate adjacency list
            //   for only nodes to which there are active edges?
            for (&y, &Edges { weight, .. }) in &self.edges[x] {
                trace!("  (T) detect_cycle: check {}->{} ({})", x,y,weight);
                if d[y] > d[x] + weight {
                    if x == u && y == v {
                        return Err(p);
                    } else {
                        d[y] = d[x] + weight;
                        trace!("updating {} {} -> {}", x,y, d[y]);
                        p[y] = Some(x);
                        queue.push_back(y);
                    }
                }
            }
        }

        Ok(d)
    }

    pub fn get_value(&self, DVar(x) :DVar) -> isize {
        self.distance_map[0] - self.distance_map[x]
    }

//    pub fn optimize(&self, func :Expression) -> Vec<isize> {
//
//    }
}

pub enum Expression {
    Constant(isize),
    Variable(DVar),
    Add(Box<Expression>,Box<Expression>),
    Mul(Box<Expression>,Box<Expression>),
}



// Sat modulo difference logic (on isize ints) solver
pub struct SATModDiff {
    pub sat :minisat::Solver,
    pub diff :Differences<Lit>,
    pub sat_clauses_generated :usize,
    sat_time :Duration,
    diff_time :Duration,
}

impl SATModDiff {
    pub fn report_time(&self) {
        println!("(T) solver time in SAT: {}, in Diff: {}", 
                 self.sat_time, self.diff_time);
    }

    pub fn new() -> Self {
        SATModDiff { sat: minisat::Solver::new(), diff: Differences::new(),
        sat_clauses_generated: 0, sat_time: Duration::zero(), diff_time: Duration::zero()}
    }

    //pub fn const_constraint(&mut self, x :DVar, y :DVar, k :f64) {
    //    unimplemented!()
    //}

    pub fn cond_constraint(&mut self, x :DVar, y :DVar, k :isize) -> Bool {
        let lit = match self.sat.new_lit() {
            Bool::Lit(x) => x,
            _ => panic!(),
        };

        self.diff.add_constraint(lit,x,y,k);
        Bool::Lit(lit)
    }

    fn theory_consistent(model :&minisat::Model, diff :&mut Differences<Lit>) -> Result<(), Vec<Lit>> {
        // first, disable constraints
        let cs = diff.constraints.iter().map(|(k,_)| *k).collect::<Vec<_>>();
        for &c in &cs {
            if !model.value(&Bool::Lit(c)) { 
                diff.disable(c);
            }
        }
        // then enable constraints, reporting unsat bool clauses
        for &c in &cs {
            if model.value(&Bool::Lit(c)) { 
               match diff.enable(c) {
                   Ok(()) => {},
                   Err(unsat) => {
                       return Err(unsat);
                   }
               }
            }
        }

        return Ok(())
    }

    fn make_consistent(&mut self, prefix :&[Bool]) -> bool {
        loop {
            debug!("(*) solving sat");

            let t1 = PreciseTime::now();
            let m  =self.sat.solve_under_assumptions(prefix.iter().cloned());
            let t2 = PreciseTime::now();
            self.sat_time = self.sat_time + t1.to(t2);

            match m {
                Err(()) => return false,
                Ok(model) => {
                    debug!("(*) sat ok, going to theory");

                    let t1 = PreciseTime::now();
                    let t = Self::theory_consistent(&model, &mut self.diff);
                    let t2 = PreciseTime::now();
                    self.diff_time = self.diff_time + t1.to(t2);

                    match t {
                        Ok(()) => {
                            debug!("(*) theory ok");
                            return true;
                        },
                        Err(unsat) => {
                            debug!("(*) unsat theory, removing {}", unsat.len());
                            self.sat_clauses_generated += 1;
                            self.sat.add_clause(unsat.into_iter().map(|x| Bool::Lit(!x)));
                        },
                    }
                }
            }
        }
    }

    pub fn solve<'a>(&'a mut self) -> Result<SATDiffModel<'a>, ()> {
        self.solve_under_assumptions(&vec![])
    }

    pub fn solve_under_assumptions<'a>(&'a mut self, v :&[Bool]) -> Result<SATDiffModel<'a>, ()> {
        if !self.make_consistent(v) { return Err(()); }
        match self.sat.solve_under_assumptions(v.iter().cloned()) {
            Ok(m) => Ok(SATDiffModel { sat: m, diff :&self.diff }),
            _ => unreachable!(),
        }
    }

}

pub struct SATDiffModel<'a> {
    pub sat :minisat::Model<'a>,
    pub diff :&'a Differences<Lit>,
}

#[test]
fn test1() {
    let mut s = SATModDiff::new();
    let x = s.diff.new_var();
    let y = s.diff.new_var();
    let z = s.diff.new_var();
    let c1 = s.cond_constraint(x,y,-2);
    let c2 = s.cond_constraint(y,z,-5);
    let c3 = s.cond_constraint(z,x,3);

    s.sat.add_clause(vec![c1,c2]);
    s.sat.add_clause(vec![c3]);

    match s.solve() {
        Ok(m) => {
            println!("c1={},c2={},c3={}",m.sat.value(&c1),m.sat.value(&c2),m.sat.value(&c3));
            println!("x={},y={},z={}",m.diff.get_value(x),m.diff.get_value(y),
                m.diff.get_value(z));
        }, 
        _ => panic!(),
    }
}

#[test]
fn test2() {
    let mut d = Differences::new();
    let x = d.new_var();
    let y = d.new_var();
    let z = d.new_var();
    println!("vars:  x={:?}, y={:?}, z={:?}",x,y,z);

    d.add_constraint(1, x, y, -5);
    d.add_constraint(2, y, z, -10);
    d.add_constraint(3, x, z, -15);
    d.add_constraint(4, z, x, 14); // z-x <= 19 --> x+19 >= z

    println!("(x,y,z)=({:?},{:?},{:?})", d.get_value(x), d.get_value(y), d.get_value(z));

    println!("{:?}", d.enable(1));
    println!("(x,y,z)=({:?},{:?},{:?})", d.get_value(x), d.get_value(y), d.get_value(z));
    println!("{:?}", d.enable(3));
    println!("(x,y,z)=({:?},{:?},{:?})", d.get_value(x), d.get_value(y), d.get_value(z));
    println!("{:?}", d.enable(2));
    println!("(x,y,z)=({:?},{:?},{:?})", d.get_value(x), d.get_value(y), d.get_value(z));
    println!("{:?}", d.enable(4));
    println!("(x,y,z)=({:?},{:?},{:?})", d.get_value(x), d.get_value(y), d.get_value(z));
}


