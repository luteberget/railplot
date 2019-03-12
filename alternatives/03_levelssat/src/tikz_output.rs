use parser::*;
use solver::*;

pub fn convert(input :&SolverInput, model :&SolverOutput) -> Result<String, String> {
    let mut out = String::new();
    let mut lines :Vec<Vec<(f64,f64)>>= Vec::new();
    fn coords(x:f64,y:f64) -> String { format!("({},{})",x,y) }
    for (pr1,pr2,y) in &model.tracks.edge_levels {
        let (_n,x1,y1) = &model.tracks.node_coords[pr1.node];
        let (_n,x2,y2) = &model.tracks.node_coords[pr2.node];
        lines.push(conv_line((*x1,*y1),*y,(*x2,*y2)));
    }

    for l in &lines {
        use itertools::Itertools;
        out.push_str(&format!("\\draw[track] {};\n", l.iter().map(|(x,y)| coords(*x,*y)).join(" -- ")));
    }

    // Switches

    for (i,n) in input.nodes.iter().enumerate() {
        let t = match &n.shape {
            NodeShape::Switch { dir: Dir::Up, .. } => {
                let l =  input.edges.iter().
                    position(|e| e.a.node == i && e.a.port == Port::Left).unwrap();
                let r = input.edges.iter().
                    position(|e| e.a.node == i && e.a.port == Port::Right).unwrap();

                println!("left {:?}, right {:?}", lines[l], lines[r]);

                Some((lines[l][1], lines[r][1]))
            },
            NodeShape::Switch { dir: Dir::Down, .. } => {
                let l =  input.edges.iter().
                    position(|e| e.b.node == i && e.b.port == Port::Left).unwrap();
                let r = input.edges.iter().
                    position(|e| e.b.node == i && e.b.port == Port::Right).unwrap();

                println!("left {:?}, right {:?}", lines[l], lines[r]);

                Some((*lines[l].iter().rev().nth(1).unwrap(), 
                 *lines[r].iter().rev().nth(1).unwrap()))
            },
            _ => None,
        };

        if let Some((v1,v2)) = t {
            let (_,x,y) = model.tracks.node_coords[i];

            let t1 = (v1.0 - x, v1.1 - y);
            let t2 = (v2.0 - x, v2.1 - y);
            let ut1 = (t1.0 / t1.0.abs(), t1.1 / t1.0.abs());
            let ut2 = (t2.0 / t2.0.abs(), t2.1 / t2.0.abs());

            println!("{:?} -- {:?} -- {:?}", (v1,v2), (t1,t2), (ut1,ut2));

            // \fill (2.0,0.0)--(2.4,0.4)--(2.4,0.0);
            out.push_str(&format!("\\fill {} -- {} -- {};\n",
                                 coords(x,y),
                                 coords(x+ut1.0*0.4, y+ut1.1*0.4),
                                 coords(x+ut2.0*0.4, y+ut2.1*0.4)));

        }
    }

    fn angle((x,y):Pt) -> f64 { y.atan2(x) }
    fn rad2deg(x :f64) -> f64 { x*180.0/::std::f64::consts::PI }

    for ((_,s),(p,t)) in &model.symbols {
        out.push_str(&format!("%% symbol {:?}\n", s));
        let mut deg = rad2deg(angle(*t));
        let mut name = &s.name[..];
        if name.ends_with("rev") {
            name = &name[..(name.len()-3)];
            deg += 180.0;
        }

        out.push_str(&format!("\\begin{{scope}}[shift={{{}}},rotate={}]\n", 
                              coords(p.0,p.1), deg));
        out.push_str(&format!("\\{}\n", name));
        out.push_str(&format!("\\end{{scope}}\n\n"));
    }

    Ok(out)
}
