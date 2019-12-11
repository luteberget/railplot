//! Input model for automatic drawing of schematic plans.
//!
//! See Ch. 6 of Luteberget's thesis.

/// Mileage directions up (increasing) and down (decreasing).
#[derive(Debug, Copy, Clone)]
pub enum Dir { Up=0, Down=1 }

impl Dir {
    pub fn opposite(&self) -> Dir {
        match self {
            Dir::Up => Dir::Down,
            Dir::Down => Dir::Up,
        }
    }
}

/// Side left/right.
///
/// For specifying the left/right legs of a switch or the switch's deviating side.
///
/// ```text
///     /---    <- Left leg
/// ---O----    <- Right leg
///
///    ^--- Left switch
/// ```
#[derive(Debug, Copy, Clone)]
pub enum Side { Left=0, Right=1 }


/// Node shapes.
///
///    
#[derive(Debug, Copy, Clone)]
pub enum Shape { 
    /// A track ends here (open end, buffer stop, model boundary) at the lower-mileage end.
    Begin, 
    /// A track ends here (open end, buffer stop, model boundary) at the higher-mileage end.
    End, 
    /// A switch node with parameters for the switch deviating side (left/right) and direction (Up = switch facing when traveling in increasing mileage direction).
    Switch(Side, Dir), 
    /// A node which simply connects two tracks, included for convencience when translating from railML.
    Continuation ,

    Crossing,
        
}

/// Node port.
///
/// For begin/end nodes:
/// ```text
///   *Begin* node type ----------->  o----     <- edge connected to *Out* port.
///   Edge connected to *In* port ->  ----o     <- *End* node type.
/// ``` 
/// 
/// For switches:
///   
/// ```text
///                     /---    <- Left port
///  Trunk port --> ---O----    <- Right port
/// ```
///
/// For crossings:
///   
/// ```text
///                       /---    <- OutLeft port
///  InRight port ->  ---O----    <- OutRight port
///  InLeft port  ->  --/
/// ```
#[derive(Debug, Copy, Clone, Hash, PartialEq, Eq)]
pub enum Port { 
    In=0, Out=1, Left=2, Right=3, Trunk=4,
    ContA=5, ContB=6,
    InLeft=7, InRight=8, OutLeft=9, OutRight=10}


/// Nodes have a mileage (`pos`), shape and name.
#[derive(Debug, Clone)]
pub struct Node {
    pub name :String,
    pub pos :f64,
    pub shape :Shape,
}

/// Edges connect two nodes specified by name and port. The port should
/// correspond to one of the available ports for the node type.
/// Where the mileage of a should be lower than or equal to the mileage of b.
#[derive(Debug, Clone )]
pub struct Edge<Obj> {
    pub a :(String,Port),
    pub b :(String,Port),
    pub objects :Vec<(Symbol,Obj)>,
}

/// A symbol on an edge with a given level (`leve`) and mileage (`pos`).
#[derive(Debug, Copy, Clone)]
pub struct Symbol {
    /// The mileage of the symbol's origin.
    pub pos :f64,
    /// The width of the symbol in drawing units.
    pub width :f64,
    /// The distance from the left of the symbol to the point on the 
    /// symbol which has the given mileage (`pos`).
    pub origin: f64,
    /// The level that the symbol is placed into (+1 is immediately above the
    /// edge, -1 is below).
    pub level: isize,
}

/// The topology input needed for producing automatic schematic drawings.
#[derive(Debug)]
pub struct SchematicGraph<Obj> {
    /// Required: a set of nodes.
    pub nodes :Vec<Node>,
    /// Required: as set of edges.
    pub edges :Vec<Edge<Obj>>,
    /// Optional: a set of edge indices (indexing into `edges`) 
    /// which should be located close together. If this set is non-empty,
    /// the optimization criterion `Goal::MainTrackHeight` will minimize 
    /// the total y coordinate range for this set of edges.
    pub main_tracks_edges: Vec<usize>,
}

pub type Pt = (f64,f64);

/// Output from algorithm for automatic drawing of schematic railway plans.
pub struct SchematicOutput<Obj> {
    /// For each node: the original node specification together with its assigned 2D point (in
    /// drawing units).
    pub nodes :Vec<(Node, Pt)>, // node coords
    /// For each edge: the original edge specification together with its assigned 2D polyline (in
    /// drawing units).
    pub lines :Vec<(Edge<Obj>,Vec<Pt>)>, // edge lines
    /// For each symbol: the original symbol specification together with its assigned 2D point (in
    /// drawing units) and tangent vector.
    pub symbols :Vec<(Obj,(Pt,Pt))>, // origin pt and rotation
}

