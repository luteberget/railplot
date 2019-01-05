use svg;
use std::str::from_utf8;
use solver::SolverOutput;
use parser::Port;

pub fn convert(model :&SolverOutput) -> Result<String, String> {
    let width = model.node_coords.iter().map(|(_,x,_)| *x).fold(-1./0., f64::max);
    let height = model.node_coords.iter().map(|(_,_,y)| *y)
        .chain(  model.edge_levels.iter().map(|(_,_,y)| *y))
        .fold(-1./0.,f64::max);

    use svg::Document;
    use svg::node::element::Path;
    use svg::node::element::path::Data;

    let scale = 100.0;
    let doc_width = width*scale+2.0*scale;
    let doc_height = height*scale+2.0*scale;

    let tr_x = |x:f64| x*scale;
    let tr_y = |y:f64| (height-y)*scale;

    let mut document = Document::new()
        //.set("viewBox", (-scale,-scale,scale*width+2.0*scale, scale*height+2.0*scale))
        .set("viewBox", (-scale,-scale, doc_width, doc_height))
        .set("width",doc_width)
        .set("height",doc_height);

    for (pr1,pr2,y) in &model.edge_levels {
        let (_n,x1,y1) = &model.node_coords[pr1.node];
        let (_n,x2,y2) = &model.node_coords[pr2.node];

        let (y1,mut top_a) = if let Port::Top = pr1.port { (y1 + 1.0, true) } else { (*y1, false) };
        let (y2,mut top_b) = if let Port::Top = pr2.port { (y2 + 1.0, true) } else { (*y2, false) };

        let dx1 = (y1-y).abs();
        let dx2 = (y2-y).abs();

        if top_a {
            let mut data = Data::new()
                .move_to((tr_x(*x1),tr_y(y1-1.0)))
                .line_to((tr_x(*x1),tr_y(y1)));
            let path = Path::new()
                .set("fill", "none")
                .set("stroke", "black")
                .set("stroke-width", 5)
                .set("d", data);
            document = document.add(path);
        }


        let mut data = Data::new()
            .move_to((tr_x(*x1),tr_y(y1)))
            .line_to((tr_x(x1+dx1),tr_y(*y)))
            .line_to((tr_x(x2-dx2),tr_y(*y)))
            .line_to((tr_x(*x2),tr_y(y2)));
        let path = Path::new()
            .set("fill", "none")
            .set("stroke", "black")
            .set("stroke-width", 5)
            .set("d", data);

        if top_b {
            let mut data = Data::new()
                .move_to((tr_x(*x2),tr_y(y2)))
                .line_to((tr_x(*x2),tr_y(y2-1.0)));
            let path = Path::new()
                .set("fill", "none")
                .set("stroke", "black")
                .set("stroke-width", 5)
                .set("d", data);
            document = document.add(path);
        }

        document = document.add(path);

    }


    // Add nodes
    for (name,x,y) in &model.node_coords {
        let c = svg::node::element::Circle::new()
            .set("cx",tr_x(*x))
            .set("cy",tr_y(*y))
            .set("r",7.5)
            .set("fill", "red");

        let mut t = svg::node::element::Text::new()
            .set("x",tr_x(*x))
            .set("y",tr_y(*y));
        use svg::Node;
        t.append(svg::node::Text::new(name.clone()));

        document = document.add(c);
        document = document.add(t);
    }

    let mut s :Vec<u8>= Vec::new();
    svg::write(&mut s, &document);
    Ok(from_utf8(&s).unwrap().to_string())

}
