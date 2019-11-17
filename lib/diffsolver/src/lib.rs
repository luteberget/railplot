pub extern crate minisat;
use num_traits::{Num, Bounded};
use std::fmt::Display;
extern crate time;
use time::{PreciseTime, Duration};

use minisat::{Bool,Lit};
use log::{debug,  trace};

use std::hash::Hash;
use std::collections::HashMap;
use std::collections::VecDeque;
use std::fmt::Debug;

type CAddr = (usize,usize,usize);

// Unsatisfiability is reported as a list of edges
// constituting a negative cycle in the graph of 
// vars/constraints.
type Unsat<CId> = Vec<CId>;

#[derive(Copy,Clone,Debug,Hash,PartialEq,Eq)]
pub struct DVar(usize);

pub struct Differences<N, CId> {
    vars  :Vec<Option<String>>,
    constraints  :HashMap<CId,CAddr>,
    edges        :Vec<HashMap<usize, Edges<N, CId>>>,
    distance_map :Vec<N>,
}

pub struct Edges<N, CId> {
    weight: N,
    constraints: Vec<Constraint<N, CId>>,
}

#[derive(Copy,Clone, Debug)]
pub struct Constraint<N, CId> {
    id :Option<CId>,
    active :bool,
    weight :N,
}

impl<N :Num+PartialOrd+Debug+Display+Bounded+Copy+Clone, CId:Copy+Hash+Eq+Debug> Differences<N, CId> {
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
        self.distance_map.push(N::zero());
        if v.0 > 0 {
            // add zero edge
            self.edges[0].insert(v.0, Edges { weight: N::zero(), constraints: 
                vec![Constraint { id: None, active: true, weight: N::zero() }] });
        }
        v
    }

    pub fn add_constraint(&mut self, id :CId, DVar(x) :DVar, DVar(y) :DVar, k :N) {
        if self.constraints.contains_key(&id) || x >= self.vars.len() || y >= self.vars.len() {
            panic!("Inconsistent ids in new_constraint.");
        }

        let edge = self.edges[x].entry(y)
            .or_insert(Edges { weight: N::max_value(), constraints: vec![] });

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
        let mut w = N::max_value();
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
                    let (mut w, mut i) = (N::max_value(), None);
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

    fn get_weight(&self, u:usize, v:usize) -> N {
        self.edges[u][&v].weight
    }

    fn detect_cycle(&self, u:usize, v:usize) -> Result<Vec<N>, Vec<Option<usize>>> {
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

    pub fn get_value(&self, DVar(x) :DVar) -> N {
        self.distance_map[0] - self.distance_map[x]
    }
}

impl<CId:Copy+Hash+Eq+Debug> Differences<isize, CId> {

    pub fn optimize(&self, goal :HashMap<DVar,isize>) -> Result<HashMap<DVar,isize>,String> {
        debug!("Running Differences.optimize");
        let mut max_iter = 500*goal.len()*goal.len();
        let mut values = (0..self.num_vars())
            .map(|i| self.get_value(DVar(i)) as f64).collect::<Vec<_>>();

        let mut constraints :Vec<(usize,usize,isize)>= Vec::new();
        for (xi,out) in self.edges.iter().enumerate() {
            for (xj, &Edges { weight: w, .. }) in out {
                if w < isize::max_value() {
                    constraints.push((xi,*xj,w));
                }
            }
        }
        trace!("Constraints: {:?}", constraints);

        // Set values to goal
        for (&DVar(x),&v) in &goal { values[x] = v as f64; }

        // Project onto constraints' feasible region.
        loop {
            let (cmax,cval) = {
                let (mut cmax, mut cval) = (0,f64::min_value());
                let constraint_values = constraints.iter()
                    .map(|(xi,xj,w)| values[*xi]-values[*xj]-(*w as f64)).enumerate();
                for (i,v) in constraint_values {
                    if v > cval {
                        cmax = i;
                        cval = v;
                    }
                }
                (cmax,cval)
            };

            if cval <= 0.01 {
                break Ok(goal.into_iter().map(|(k,_)| (k,values[k.0].round() as isize)).collect())
            }

            {
                let (i,j,w) = constraints[cmax];
                trace!("Max violated constraint {:?} --  {} {} ",(i,j,w),values[i],values[j]);
            }

            let (xi,xj,_) = constraints[cmax];
            if goal.contains_key(&DVar(xi)) && goal.contains_key(&DVar(xj)) {
                //let (quot,rem) = (cval/2,cval%2);
                let half = cval*0.5;
                trace!("moving {}={}->{} {}={}->{}", xi,values[xi],values[xi]-half,
                                                       xj,values[xj],values[xj]+half);
                values[xi] -= half;
                values[xj] += half;
            } else if goal.contains_key(&DVar(xi)) {
                trace!("moving {}={}->{}", xi,values[xi],values[xi]-cval);
                values[xi] -= cval;
            } else if goal.contains_key(&DVar(xj)) {
                trace!("moving {}={}->{}", xj,values[xj],values[xj]+cval);
                values[xj] += cval;
            } else {
                return Err(format!("Violated constraint which is not in goal set."));
            }

            if max_iter == 0 {
                return Err(format!("Optimization did not converge."));
            } else {
                max_iter -= 1;
                trace!("diffsolver optimize iteration {}", max_iter);
            }
        }
    }
}



// Sat modulo difference logic (on isize ints) solver
pub struct SATModDiff<N> {
    pub sat :minisat::Solver,
    pub diff :Differences<N, Lit>,
    pub sat_clauses_generated :usize,
    sat_time :Duration,
    diff_time :Duration,
}

impl<N :Num+PartialOrd+Debug+Display+Bounded+Copy+Clone> SATModDiff<N> {
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

    pub fn cond_constraint(&mut self, x :DVar, y :DVar, k :N) -> Bool {
        let lit = match self.sat.new_lit() {
            Bool::Lit(x) => x,
            _ => panic!(),
        };

        self.diff.add_constraint(lit,x,y,k);
        Bool::Lit(lit)
    }

    fn theory_consistent(model :&minisat::Model, diff :&mut Differences<N, Lit>) -> Result<(), Vec<Lit>> {
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

    pub fn solve<'a>(&'a mut self) -> Result<SATDiffModel<'a,N>, ()> {
        self.solve_under_assumptions(&vec![])
    }

    pub fn solve_under_assumptions<'a>(&'a mut self, v :&[Bool]) -> Result<SATDiffModel<'a,N>, ()> {
        if !self.make_consistent(v) { return Err(()); }
        match self.sat.solve_under_assumptions(v.iter().cloned()) {
            Ok(m) => Ok(SATDiffModel { sat: m, diff :&self.diff }),
            _ => unreachable!(),
        }
    }

}

pub struct SATDiffModel<'a, N> {
    pub sat :minisat::Model<'a>,
    pub diff :&'a Differences<N, Lit>,
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



#[test]
fn test3() {
    let mut d = Differences::new();

    let node1 = d.new_var();
    let node2 = d.new_var();

    let s1 = d.new_var();
    let s2 = d.new_var();
    let s3 = d.new_var();

    d.add_constraint(0,node1,node2,-1000);
    d.enable(0).unwrap();

    d.add_constraint(1,node1,s1,-10);
    d.add_constraint(2,s1,s2,-10);
    d.add_constraint(3,s2,s3,-10);
    d.add_constraint(4,s3,node2,-10);
    d.enable(1).unwrap();
    d.enable(2).unwrap();
    d.enable(3).unwrap();
    d.enable(4).unwrap();

    println!("n1={}, s1={}, s2={}, s3={}, n2={}", 
             d.get_value(node1),
             d.get_value(s1),
             d.get_value(s2),
             d.get_value(s3),
             d.get_value(node2));

    let mut goal = HashMap::new();
    goal.insert(s1,500);
    goal.insert(s2,500);
    goal.insert(s3,600);
    let mut expected = HashMap::new();
    expected.insert(s1,495);
    expected.insert(s2,505);
    expected.insert(s3,600);

    println!("Optimize to {:?}", goal);
    let opt = d.optimize(goal).unwrap();
    println!("Optimized: {:?}", opt);

    assert_eq!(opt,expected);

    println!("\n\ntest ok\n\n");

}
