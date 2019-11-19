use crate::model::*;
use crate::solvers::*;
use std::ffi::CString;
use log::*;

pub struct FFISolver {
    solver :Option<LevelsSatSolver>,
    graph :Option<SchematicGraph<()>>,
    solution: Option<SchematicOutput<()>>,
    error_message: Option<CString>,
}

#[cfg(target_os = "windows")]
#[no_mangle]
pub extern "system" fn railplot_enable_logging() {
    struct SimpleLogger;
    impl log::Log for SimpleLogger {
        fn enabled(&self, metadata: &Metadata) -> bool {
            metadata.level() <= Level::Trace
        }

        fn log(&self, record: &Record) {
            if self.enabled(record.metadata()) {
                let s = format!("{} - {}\n", record.level(), record.args());
                let cstr = std::ffi::CString::new(s).unwrap();
                unsafe {winapi::um::debugapi::OutputDebugStringA(cstr.as_ptr()); }
            }
        }

        fn flush(&self) {}
    }

    drop(log::set_boxed_logger(Box::new(SimpleLogger))
        .map(|()| log::set_max_level(LevelFilter::Trace)));
}

#[no_mangle]
pub extern "system" fn railplot_new() -> *mut FFISolver {
    Box::into_raw(Box::new(FFISolver {
        solver :Some(LevelsSatSolver { 
            criteria: Vec::new(),
            nodes_distinct: false
        }),
        graph: Some(SchematicGraph {
            nodes: Vec::new(),
            edges: Vec::new(),
        }),
        solution: None,
        error_message: None,
    }))
}

#[no_mangle]
pub extern "system" fn railplot_addendnode(s :*mut FFISolver, pos :f64, dir :Dir) -> u32 {
    let graph = unsafe { &mut *s }.graph.as_mut().unwrap();
    let name = format!("node_{}", graph.nodes.len()+1);
    graph.nodes.push(Node {
        name: name,
        pos: pos,
        shape: match dir {
            Dir::Up => Shape::Begin,
            Dir::Down => Shape::End,
        }
    });

    (graph.nodes.len()-1) as u32
}

#[no_mangle]
pub extern "system" fn railplot_addswitchnode(s :*mut FFISolver, pos :f64, side :Side, dir :Dir) -> u32 {
    let graph = unsafe { &mut *s }.graph.as_mut().unwrap();
    let name = format!("node_{}", graph.nodes.len()+1);
    graph.nodes.push(Node {
        name: name,
        pos: pos,
        shape: Shape::Switch(side,dir),
    });

    (graph.nodes.len()-1) as u32
}

#[no_mangle]
pub extern "system" fn railplot_addedge(s :*mut FFISolver, a_node :u32, a_port :Port, 
                                                           b_node :u32, b_port :Port) -> u32 {
    let graph = unsafe { &mut *s }.graph.as_mut().unwrap();
    graph.edges.push(Edge {
        a: (graph.nodes[a_node as usize].name.clone(),
            a_port),
        b: (graph.nodes[b_node as usize].name.clone(),
            b_port),
        objects: Vec::new(),
    });

    (graph.edges.len()-1) as u32
}

#[no_mangle]
pub extern "system" fn railplot_num_nodes(s :*mut FFISolver) -> u32 { 
    let s = unsafe { &*s };
    if let Some(g) = &s.graph { g.nodes.len() as u32 }
    else if let Some(s) = &s.solution { s.nodes.len() as u32 }
    else { 0 }
}

#[no_mangle]
pub extern "system" fn railplot_num_edges(s :*mut FFISolver) -> u32 { 
    let s = unsafe { &*s };
    if let Some(g) = &s.graph { g.edges.len() as u32 }
    else if let Some(s) = &s.solution { s.lines.len() as u32 }
    else { 0 }
}

#[no_mangle]
pub extern "system" fn railplot_get_error_message(s :*mut FFISolver) -> *const std::os::raw::c_char { 
    let s = unsafe { &mut *s };
    s.error_message.as_ref().map(|e| e.as_ptr()).unwrap_or(std::ptr::null())
}

#[no_mangle]
pub extern "system" fn railplot_solve(s :*mut FFISolver) -> u32 {
    let s = unsafe { &mut *s};
    let solver = s.solver.take().unwrap();
    let graph = s.graph.take().unwrap();
    debug!("Starting solve with graph:\n{:#?}",  graph);
    match solver.solve(graph) {
        Ok(result) => {
            s.solution = Some(result);
            1
        },
        Err(msg) => {
            s.error_message = Some(CString::new(msg).unwrap());
            0
        },
    }
}

#[no_mangle]
pub extern "system" fn railplot_has_solution(s :*mut FFISolver) -> u32 {
    let s = unsafe { &mut *s};
    if s.solution.is_some() { 1 } else { 0 }
}

#[no_mangle]
pub extern "system" fn railplot_solution_node_x(s :*mut FFISolver, node :u32) -> f64 {
    let sol = unsafe { &mut *s}.solution.as_ref().unwrap();
    (sol.nodes[node as usize].1).0
}

#[no_mangle]
pub extern "system" fn railplot_solution_node_y(s :*mut FFISolver, node :u32) -> f64 {
    let sol = unsafe { &mut *s}.solution.as_ref().unwrap();
    (sol.nodes[node as usize].1).1
}

#[no_mangle]
pub extern "system" fn railplot_solution_edge_num_vertices(s :*mut FFISolver, edge :u32) -> u32 {
    let sol = unsafe { &mut *s}.solution.as_ref().unwrap();
    (sol.lines[edge as usize].1).len() as u32
}

#[no_mangle]
pub extern "system" fn railplot_solution_edge_vertex_x(s :*mut FFISolver, edge :u32, vertex :u32) -> f64 {
    let sol = unsafe { &mut *s}.solution.as_ref().unwrap();
    (sol.lines[edge as usize].1)[vertex as usize].0
}

#[no_mangle]
pub extern "system" fn railplot_solution_edge_vertex_y(s :*mut FFISolver, edge :u32, vertex :u32) -> f64 {
    let sol = unsafe { &mut *s}.solution.as_ref().unwrap();
    (sol.lines[edge as usize].1)[vertex as usize].1
}


#[no_mangle]
pub extern "system" fn railplot_destroy(s :*mut FFISolver) {
    unsafe { Box::from_raw(s) };
}
