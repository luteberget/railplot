-- solver for graph -> graph-like drawing
--
--

module GridSolver where

import Prelude hiding (reverse, flip)
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Maybe (catMaybes)
import Control.Monad (forM, forM_, join)
import Data.Tuple (swap)
import Data.List (nub, sortOn)
import Data.Maybe (listToMaybe)

import qualified SAT.Unary as Unary
import qualified SAT.Optimize as Optimize

import SAT
import SAT.Val
import SAT.Bool

type Pt = (Int, Int)
type NodeId = Int
type Node = [NodeAlt]
type NodeAlt = ((N,N,N),(N,N,N))
type N = Maybe NodeId

nop = Nothing

flip :: Node -> Node
flip = fmap swap

reverse (a,b,c) = (c,b,a)

horiz x     = (nop, Just x, nop)
left (y,z)  = (Just y, Just z, nop)
right (y,z) = (nop, Just y, Just z)

rotLeft  ((a,b,c),(d,e,f)) = ((nop, a, b), (e, f, nop))
rotRight ((a,b,c),(d,e,f)) = ((b, c, nop), (nop, d, e))

nothing = (nop, nop, nop)

shift :: NodeAlt -> NodeAlt
shift = fmap shiftTriplet
  where shiftTriplet :: (N,N,N) -> (N,N,N)
        shiftTriplet (a,b,c) = (nop,a,b)

startNode :: NodeId -> Node
startNode x = [(nothing, horiz x)]

endNode :: NodeId -> Node
endNode = flip . startNode

outLeftSwHoriz  x (y,z) = (horiz x, left  (y,z))
outRightSwHoriz x (y,z) = (horiz x, right (y,z))

outLeftSw  x (y,z) = [ outLeftSwHoriz  x (y,z), rotRight $ outLeftSwHoriz x (y,z)  ]
outRightSw x (y,z) = [ outRightSwHoriz x (y,z), rotLeft  $ outRightSwHoriz x (y,z) ]

inRightSw x (y,z) = flip $ outLeftSw  x (z,y)
inLeftSw  x (y,z) = flip $ outRightSw x (z,y)

outSw x (y,z) = (outLeftSw x (y,z)) ++ (outRightSw x (y,z))
inSw x (y,z) = (inLeftSw x (y,z)) ++ (inRightSw x (y,z))

ex1Nodes = 
  [ startNode 1
  , outLeftSw 0 (2,2)
  , inRightSw 3 (1,1)
  , endNode 2 ]

ex2Nodes = 
  [ startNode  2             -- 0
  , startNode  3             -- 1
  , outLeftSw  0 (4,3)       -- 2
  , inLeftSw   5 (1,2)       -- 3
  , inRightSw  6 (2,5)       -- 4
  , outRightSw 3 (7,4)       -- 5
  , endNode    4             -- 6
  , endNode    5 ]           -- 7

enc2 :: Int -> (Int,Int) -> Int
enc2 w (x,y) = x+w*y

dec2 :: Int -> Int -> (Int,Int)
dec2 w xy = (xy `mod` w, xy `div` w)

data NodeValue = Unused | Linear (NodeId,NodeId) | Node NodeId
  deriving (Show, Eq, Ord)

data Graphics = GNode (Int,Int) NodeId | GLine (Int,Int) (Int,Int) (NodeId,NodeId)
  deriving (Show, Eq, Ord)

data Problem
  = Problem
  { pNodeVals   :: [Val NodeValue]
  , pHorizLines :: [Lit]
  , pDownLines  :: [Lit]
  , pUpLines    :: [Lit]
  } deriving (Show, Eq, Ord)

exactlyOneOr :: Solver -> [Lit] -> [Lit] -> IO ()
exactlyOneOr s pre ls = do
  addClause s (pre ++ ls)
  atMostOneOr s pre ls

withProblem :: [Node] -> Pt -> (Solver -> Problem -> IO a) -> IO a
withProblem nodes (w,h) f = withNewSolver $ \s -> do
  let edges = (Set.toList $ Set.fromList e)
        where e = [(j,i) | (i,alt) <- zip [0..] nodes
                  , ((a1,a2,a3),b) <- alt , j <- catMaybes [a1,a2,a3] ] ++ 
                  [(i,j) | (i,alt) <- zip [0..] nodes
                  , (a,(b1,b2,b3)) <- alt , j <- catMaybes [b1,b2,b3] ]

  -- Sol space: NODES
  nodeVals <- sequence [ do
      nodeVal <- newVal s  (Unused : ([Node i | (i,_) <- zip [0..] nodes ] ++ 
                                      [Linear (i,j) | (i,j) <- edges ]))
      return nodeVal
    | _ <- [(enc2 w (0,0)).. (enc2 w (w-1,h-1))] ]

  -- Sol space: EDGES
  horizLines <- sequence [ newLit s 
                         | _ <- [(enc2 (w-1) (0,0)).. (enc2 (w-1) (w-2, h-1))] ]
  downLines  <- sequence [ newLit s 
                         | _ <- [(enc2 (w-1) (0,0)).. (enc2 (w-1) (w-2, h-2))] ]
  upLines    <- sequence [ newLit s 
                         | _ <- [(enc2 (w-1) (0,0)).. (enc2 (w-1) (w-2, h-2))] ]

  -- No unused edges
  sequence_ [ do addClause s [neg a, neg (v1 .= Unused)] 
                 addClause s [neg a, neg (v2 .= Unused)]
            | (i,a) <- zip [0..] horizLines , let (x,y) = dec2 (w-1) i
            , let v1 = nodeVals !! (enc2 w (x,y)), let v2 = nodeVals !! (enc2 w (x+1,y)) ]
  sequence_ [ do addClause s [neg a, neg (v1 .= Unused)] 
                 addClause s [neg a, neg (v2 .= Unused)]
            | (i,a) <- zip [0..] downLines , let (x,y) = dec2 (w-1) i
            , let v1 = nodeVals !! (enc2 w (x,y)), let v2 = nodeVals !! (enc2 w (x+1,y+1)) ]
  sequence_ [ do addClause s [neg a, neg (v1 .= Unused)] 
                 addClause s [neg a, neg (v2 .= Unused)]
            | (i,a) <- zip [0..] upLines , let (x,y) = dec2 (w-1) i
            , let v1 = nodeVals !! (enc2 w (x,y+1)), let v2 = nodeVals !! (enc2 w (x+1,y)) ]

  -- No crossing edges
  sequence_ [ addClause s [neg x, neg y] | (x,y) <- zip downLines upLines ]

  -- each railway node is at exactly one schematic node
  sequence_ [ exactlyOneOr s [] [ v .= (Node nodeIdx) | v <- nodeVals ]
            | (nodeIdx,_) <- zip [0..] nodes ]

  -- Expect bugs
  let getPrevs :: (Int,Int) -> [Maybe (Val NodeValue, Lit)]
      getPrevs (x,y) = if x == 0 then [Nothing, Nothing, Nothing] else [up,hor,down]
        where up   = if y > 0 then Just (nodeVals !! (enc2 w (x-1,y-1)), 
                                        downLines !! (enc2 (w-1) (x-1,y-1))) else Nothing
              hor  = Just (nodeVals !! (enc2 w (x-1,y)), 
                           horizLines !! (enc2 (w-1) (x-1,y)))
              down = if y < (h-1) then Just (nodeVals !! (enc2 w (x-1,y+1)),
                                           upLines !! (enc2 (w-1) (x-1,y))) else Nothing
      getNexts :: (Int,Int) -> [Maybe (Val NodeValue, Lit)]
      getNexts (x,y) = if x >= (w-1) then [Nothing, Nothing, Nothing] else [up,hor,down]
        where up   = if y > 0 then Just (nodeVals !! (enc2 w (x+1,y-1)),
                                         upLines !! (enc2 (w-1) (x,y-1))) else Nothing
              hor  = Just (nodeVals !! (enc2 w (x+1,y)),
                           horizLines !! (enc2 (w-1) (x,y)))
              down = if y < (h-1) then Just (nodeVals !! (enc2 w (x+1,y+1)),
                                           downLines !! (enc2 (w-1) (x,y))) else Nothing

  forM_ (zip [0..] nodeVals) $ \(i,val) -> do
    let prevs = getPrevs (dec2 w i)
    let nexts = getNexts (dec2 w i)
    
    forM_ (zip [0..] nodes) $ \(idx, node) -> do
      let cond = val .= Node idx
      altLits <- forM node $ \((a1,a2,a3),(b1,b2,b3)) -> do
        prevConds <- forM (zip prevs [a1,a2,a3]) $ \(conn,c) -> do
          case (conn,c) of
            (Nothing, Nothing) -> return []
            (Just (_,line), Nothing) -> return [neg line]
            (Nothing, Just _) -> return [false]
            (Just (val,line),Just x) -> do
              nodeOk <- orl s [val .= Node x, val .= Linear (x,idx)]
              return [line, nodeOk]
        nextConds <- forM (zip nexts [b1,b2,b3]) $ \(conn,c) -> do
          case (conn,c) of
            (Nothing, Nothing) -> return []
            (Just (_,line), Nothing) -> return [neg line]
            (Nothing, Just _) -> return [false]
            (Just (val,line),Just x) -> do
              nodeOk <- orl s [val .= Node x, val .= Linear (idx,x)]
              return [line, nodeOk]
        andl s (join (prevConds ++ nextConds))
      addClause s ([neg cond] ++ altLits)

    forM_ edges $ \(from,to) -> do
      let cond = val .= Linear (from,to)
      -- exactlyOne incoming line is now covered by combos
      -- sequence_ [ exactlyOneOr s [neg cond] [ line | Just (_, line) <- coll ]
      --           | coll <- [prevs, nexts] ]
      let combos = [
                    ((True, False, False),(False ,False, True)),
                    ((False, True, False),(False ,True, False)),
                    ((False, False, True),(True, False, False)),
                    ((True, False, False),(False,True,False)),
                    ((False, True, False),(True,False,False)),
                    ((False, True, False),(False,False,True)),
                    ((False, False, True),(False,True,False))
                    ]
      altLits <- forM combos $ \((a1,a2,a3),(b1,b2,b3)) -> do
        prevConds <- forM (zip prevs [a1,a2,a3]) $ \(conn,c) -> do
          case (conn,c) of
            (Nothing, False) -> return []
            (Nothing, True) -> return [false]
            (Just (_,line), False) -> return [neg line]
            (Just (val,line), True) -> do
              nodeOk <- orl s [val .= Node from, val .= Linear (from,to)]
              return [line, nodeOk]
        nextConds <- forM (zip nexts [b1,b2,b3]) $ \(conn,c) -> do
          case (conn,c) of
            (Nothing, False) -> return []
            (Nothing, True) -> return [false]
            (Just (_,line), False) -> return [neg line]
            (Just (val,line), True) -> do
              nodeOk <- orl s [val .= Node to, val .= Linear (from,to)]
              return [line, nodeOk]
        andl s (join (prevConds ++ nextConds))
      addClause s ([neg cond] ++ altLits)
         
      --  sequence_ [ addClause s [neg cond, neg line, 
      --                           val .= Node from, val .= Linear (from,to) ]
      --            | Just (val, line) <- prevs]
      --  sequence_ [ addClause s [neg cond, neg line, 
      --                            val .= Node to, val .= Linear (from,to) ]
      --             | Just (val, line) <- nexts]
  f s (Problem nodeVals horizLines downLines upLines)

stats :: Solver -> IO String
stats s = do
  vars <- numVars s
  clauses <- numClauses s
  return ("SAT instance with " ++ (show vars) ++ " vars and " ++ (show clauses) ++ " clauses.")


getSolution :: Solver -> Problem -> (Int, Int) -> IO (Maybe [Graphics])
getSolution s p (w,h) = do 
  --log "SAT instance statistics after final"
  --log =<< stats s
  let allnodes x = case x of
        Node x -> [x]
        Linear (x,y) -> [x,y]

  let edgeNodes p1 p2 = do
                        v1 <- SAT.Val.modelValue s ((pNodeVals p) !! (enc2 w p1))
                        v2 <- SAT.Val.modelValue s ((pNodeVals p) !! (enc2 w p2))
                        case nub ((allnodes v1)++(allnodes v2)) of
                          [x,y] -> return (x,y)
                          _ -> error "ambiguous edge"

  nodes <- forM (zip [0..] (pNodeVals p)) $ \(i,val) -> do
    m <- SAT.Val.modelValue s val
    case m of
      Node idx -> do 
        --putStrLn $ "node " ++ (show (dec2 w i)) ++ ", " ++ (show idx)
        return [GNode (dec2 w i) idx]
      _ -> return []

  hor <- forM (zip [0..] (pHorizLines p)) $ \(i,val) -> do
    m <- SAT.modelValue s val
    if m then do 
      let (x0,y0) = dec2 (w-1) i
      let (x1,y1) = (x0+1,y0)
      --putStrLn $ "line " ++ (show (x0,y0)) ++ " " ++ (show (x1,y1))
      nodes <- edgeNodes (x0,y0) (x1,y1)
      return [GLine (x0,y0) (x1,y1) nodes]
    else do return []

  up <- forM (zip [0..] (pUpLines p)) $ \(i,val) -> do
    m <- SAT.modelValue s val
    if m then do 
      let (x0,y0) = (\(x,y) -> (x,y+1)) (dec2 (w-1) i)
      let (x1,y1) = (x0+1,y0-1)
      --putStrLn $ "line " ++ (show (x0,y0)) ++ " " ++ (show (x1,y1))
      nodes <- edgeNodes (x0,y0) (x1,y1)
      return [GLine (x0,y0) (x1,y1) nodes]
    else do return []

  down <- forM (zip [0..] (pDownLines p)) $ \(i,val) -> do
    m <- SAT.modelValue s val
    if m then do 
      let (x0,y0) = dec2 (w-1) i
      let (x1,y1) = (x0+1,y0+1)
      --putStrLn $ "line " ++ (show (x0,y0)) ++ " " ++ (show (x1,y1))
      nodes <- edgeNodes (x0,y0) (x1,y1)
      return [GLine (x0,y0) (x1,y1) nodes]
    else do return []

  return (Just (join (nodes ++ hor ++ up ++ down)))

draw :: [Node] -> Pt -> (String -> IO ()) -> IO (Maybe [Graphics])
draw nodes (w,h) log = do
  log $ "Preparing problem (" ++ (show w) ++ "," ++ (show h) ++ ")"
  withProblem nodes (w,h) $ \s p -> do
    log $ "Solving problem (" ++ (show w) ++ "," ++ (show h) ++ ")"
    log "SAT instance intermediate stage"
    log =<< stats s
    b <- solve s []
    if b then do
      getSolution s p (w,h)
    else do 
      log "no solution"
      return Nothing

minimizeSolution :: [Node] -> Pt -> (String -> IO ()) -> IO (Maybe [Graphics])
minimizeSolution nodes (w,h) log = do
  log $ "Preparing optimization (" ++ (show w) ++ "," ++ (show h) ++ ")"
  withProblem nodes (w,h) $ \s p -> do

    let slant = [ Unary.digit l 
                | l <- (pUpLines p) ++ (pDownLines p) ]
    let horiz = [Unary.digit l | l <- pHorizLines p ]

    numSlants <- Unary.addList s slant
    log $ "Solving optimization"
    b1 <- Optimize.solveOptimize s [] numSlants $ \(a,b) -> do
        log $ "Slant range (" ++ (show a) ++ "--" ++ (show b) ++ ")"
        log "SAT instance intermediate stage"
        log =<< stats s
        return True

    slantValue <- Unary.modelValue s numSlants
    addClause s [numSlants Unary..<= slantValue]

    numHoriz  <- Unary.addList s horiz
    b2 <- Optimize.solveOptimize s [] (Unary.invert numHoriz) $ \(a,b) -> do
        log $ "Horiz range (" ++ (show a) ++ "--" ++ (show b) ++ ")"
        log "SAT instance intermediate stage"
        log =<< stats s
        return True

    sol <- getSolution s p (w,h)
    log "SAT instance final stage"
    log =<< stats s
    return sol
    

-- toJson
--
-- alt1 coords for each node { "nodename": {"x": 0.0, "y": 0.0 } }
-- alt2 path for each edge [ { "n1": "n1", "n2": "n2", "length": 100.0, 
--                             "pts": [ [ 0.0, 0.0 ] , [ 1.0, 1.0 ] ] } ]
-- 

toSvg :: Int -> [Graphics] -> String
toSvg scale g = "<!DOCTYPE HTML><body><style>svg {width:650px; height:250px} .l { stroke-width: 2; stroke: darkred; }</style><svg>" ++ (join $ fmap elem g) ++ "</svg></body>"
  where 
    f x = scale*x + 10
    elem (GNode (x,y) i) = "<circle cx=\"" ++ (show (f x)) ++ "\"\n"   ++
                                   "cy=\"" ++ (show (f y)) ++ "\" r=\"5\" />"
    elem (GLine (x1,y1) (x2,y2) _) = "<line class=\"l\"\n" ++
                                         "x1=\"" ++ (show (f x1)) ++ "\"\n" ++
                                         "x2=\"" ++ (show (f x2)) ++ "\"\n" ++
                                         "y1=\"" ++ (show (f y1)) ++ "\"\n" ++
                                         "y2=\"" ++ (show (f y2)) ++ "\"\n" ++
                                         "/>"

collectEdges :: [Graphics] -> [((NodeId, NodeId), [(Int,Int)])]
collectEdges g = go startSet []
  where
    startSet = [ (start, [p0,p]) | (GNode p0 start) <- g, p <- rightEdges p0 ]

    getNode :: (Int,Int) -> Maybe Int
    getNode p0 = listToMaybe [ x | (GNode p x) <- g, p == p0 ]

    rightEdges :: (Int,Int) -> [(Int,Int)]
    rightEdges p = [ pr | (GLine pl pr _ ) <- g, pl == p ]

    go :: [(NodeId,[(Int,Int)])] -> [((NodeId, NodeId), [(Int,Int)])] -> [((NodeId, NodeId), [(Int,Int)])]
    go [] ended = ended
    go ((s,pts):starts) ended = case getNode (last pts) of 
                                   Just nd -> go starts ((((s,nd),pts)):ended)
                                   Nothing -> go ((s,(pts ++ (rightEdges (last pts)))):starts) ended
