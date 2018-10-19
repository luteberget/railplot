use failure::Error;
use solver::SolverOutput;
use convert::{OrigEdges, PosRange};
use std::collections::HashMap;
use serde_json;
use parser::Port;

type Pt = (f64,f64);
type Line = (Pt,Pt);

pub fn lines(o :&SolverOutput, orig :&OrigEdges, pos_range :&PosRange, node_names :&HashMap<String,usize>) -> Result<(serde_json::Value, serde_json::Value), Error> {
    let rev_vec =  |(a,b):(f64,f64)| (-1.0*a,-1.0*b);
    let unit_vec = |(a,b):(f64,f64)| { let l = (a*a+b*b).sqrt(); (a/l,b/l) };
    let line_tangent = |((a,b),(c,d))| unit_vec((c-a,d-b));

    let mut edge_lines = json!({});
    let mut node_data = json!({});

    let mut node_idxs = HashMap::new();
    for (k,v) in node_names.into_iter() {
        node_idxs.insert(v,k);
    }

    // Each edge should have a list of nodes and distances 
    // example: [("n1a-n1b",200.0),...]

    for (p1,p2,y) in &o.edge_levels {
        //println!("\n\nEDGE");
        let (_,x1,y1) = &o.node_coords[p1.node];
        let (_,x2,y2) = &o.node_coords[p2.node];
        //println!("{:?} -- {:?}", &o.node_coords[p1.node],
        //                         &o.node_coords[p2.node]);

        let (y1,mut top_a) = if let Port::Top = p1.port { (y1 + 1.0, true) } else { (*y1, false) };
        let (y2,mut top_b) = if let Port::Top = p2.port { (y2 + 1.0, true) } else { (*y2, false) };

        let out_length = (*y as isize - y1 as isize).abs();
        let in_length = (y2 as isize - *y as isize).abs();

        let mut lines = Vec::new();
        lines.push(((*x1,y1 as f64),(x1+out_length as f64,*y as f64)));
        lines.push(((*x1+out_length as f64, *y as f64),(*x2-in_length as f64,*y as f64)));
        lines.push(((*x2-in_length as f64, *y as f64),(*x2, y2 as f64)));
        //println!(" LINES out={} in={} {:?}", out_length, in_length, lines);
        if out_length == 0 { lines.remove(0); }
        if out_length as f64 + in_length as f64 + 1e-9 > (x2-x1) {
            lines.remove(if  out_length == 0  { 0 } else {1 } ); // Should not happen
            // TODO: panic!("Edge has no space on its level");
        }
        if in_length == 0 { let i = lines.len()-1; lines.remove(i); }
        //println!(" LINES {:?}", lines);

        let km_length = x2-x1;
        let mut pos = 0.0;
        //for k in orig_edges {
        //...
        //}

        let edge_id = ((node_idxs[&p1.node].clone(), p1.port),
                       (node_idxs[&p2.node].clone(), p2.port));
        //println!("looking up edge {:?}", edge_id);
        let orig_edges = &orig[&edge_id];

        let total :f64 = orig_edges.iter().map(|(_a,_b,d)| *d).sum();
        let mut cum :f64 = 0.0;

        let num_edges = orig_edges.len();
        let mut last_tangent = None;

        let mut curr_point = (lines[0].0);

        for (i,(a,b,d)) in orig_edges.into_iter().enumerate() {

            let node_a_name = format!("{}", a);
            let node_b_name = format!("{}", b);
            let mut node_a = json!({});
            let mut node_b = json!({});

            let tangent_a = last_tangent.unwrap_or((1.0,0.0));
            node_a.as_object_mut().unwrap().insert(format!("tangent"), json!([tangent_a.0, tangent_a.1]));
            node_a.as_object_mut().unwrap().insert(format!("point"), json!([curr_point.0, curr_point.1]));

            if (*d > 0.0) { 
                let x1_rel = cum     / total;
                let x2_rel = (cum+d) / total;
                //println!("oe:::{:?} {} {}", &lines, x1_rel, x2_rel);
                let mut lines = get_slice(&lines, x1_rel, x2_rel);
                //println!("res::{:?}", lines);
                cum += d;

                if top_a {
                    top_a = false;
                    lines.insert(0, ((*x1, y1-1.0),(*x1,y1)));
                }

                if i == num_edges-1 && top_b {
                    top_b = false;
                    lines.push(((*x2, y2),(*x2,y2-1.0)));
                }

                curr_point = lines.last().unwrap().1;

                last_tangent = Some(line_tangent(lines[lines.len()-1].clone()));
                let lines : Vec<serde_json::Value> = lines.iter().map(|(s,e)| json!([[s.0,s.1],[e.0,e.1]])).collect();

                let (pos_a, pos_b) = pos_range[&edge_id];

                let pos_start = pos_a + (pos_b-pos_a)*x1_rel;
                let pos_end = pos_a + (pos_b-pos_a)*x2_rel;

                let node_name = format!("{}-{}", a,b);
                edge_lines.as_object_mut().unwrap().insert(node_name, 
                        json!({"pos_start":a, "pos_end":b}));

                //println!("line name {}-{}",a,b);
                edge_lines.as_object_mut().unwrap().insert(format!("{}-{}", a, b), 
                     json!({"length": d, 
                            "lines": lines,
                            "pos_start": pos_start,
                            "pos_end": pos_end}));

            }

            let tangent_b = rev_vec(last_tangent.unwrap_or((1.0,0.0)));
            node_b.as_object_mut().unwrap().insert(format!("tangent"), json!([tangent_b.0, tangent_b.1]));
            node_b.as_object_mut().unwrap().insert(format!("point"), json!([curr_point.0, curr_point.1]));

            node_data.as_object_mut().unwrap().insert(node_a_name, node_a);
            node_data.as_object_mut().unwrap().insert(node_b_name, node_b);

        }

        //println!("edge from {:?} to {:?} through y={:?}", (x1,y1),(x2,y2),y);
        //println!("  out {} in  {}", out_length, in_length);
        //println!("  lines: {:?}", lines);

    }

    //Ok(format!("var edges = {};",serde_json::to_string_pretty(&json).unwrap()))
    Ok((edge_lines,node_data))
}


fn lerp(a: f64, b :f64, p :f64) -> f64 { (1.0-p)*a + p*b }

fn get_slice(lines :&[Line], a:f64, b:f64) -> Vec<Line> {
    let km_start = (lines[0].0).0;
    let km_length = (lines[lines.len()-1].1).0 - (lines[0].0).0;
    let to_km = |x| (x-km_start)/km_length;
    //println!("get slice km={}", km_length);

    let mut out : Vec<Line> = Vec::new();
    for ((x1,y1),(x2,y2)) in lines.iter() {
        let start_rel = to_km(x1);
        let end_rel   = to_km(x2);

        //println!(" sliceline {} {} {} {}", a,b,start_rel,end_rel);

        if end_rel < a { continue; }
        if start_rel > b { continue; }

        let mut start = (*x1,*y1);
        let mut end =   (*x2,*y2);

        if start_rel < a {
            let a_in_interval = (a - start_rel)/(end_rel-start_rel);
            start = (lerp(*x1,*x2, a_in_interval), 
                     lerp(*y1,*y2, a_in_interval));
        }
        if end_rel > b {
            let b_in_interval = (b - start_rel)/(end_rel-start_rel);
            end = (lerp(*x1,*x2, b_in_interval), 
                   lerp(*y1,*y2, b_in_interval));
        }

        out.push((start,end));
    }


    out
}
