-- Linear-programming-only solver for the 

module Lin where

import Input
import EdgeOrder

import Numeric.Limp.Rep
import Numeric.Limp.Program
import Numeric.Limp.Solvers.Cbc

import qualified Data.Map as Map

import Debug.Trace

data EShape = EUp | EStraight | EDown

succPairs :: [a] -> [(a,a)]
succPairs [] = []
succPairs xs = xs `zip` (tail xs)

layout :: [Node] -> [Edge] -> [Label] -> LayoutOutput
layout nodes edges labels = output
 -- solve:
 --  1. (X) edge levels
 --  2.     label km insertion points (min dist?)
 --  3.     same-layer
 --  4.     node dx contains all symbols.
 where

  -- Representation
  nodeVars = [ (node, "nx" ++ (show i), "ny" ++ (show i)) | (i,node) <- zip [0..] nodes ]
  edgeVars = [ (edge, "ey" ++ (show i)) | (i, edge) <- zip [0..] edges ]

  -- Constraints
  edgeYOrdering   = [ r1 ea .+. con 1 :<= r1 eb   | (eia,eib) <- edgeLt
                    , let (_,ea) = edgeVars !! eia, let (_,eb) = edgeVars !! eib ]
  nodeOrder       = [ r1 nxa :<= r1 nxb           | ((_,nxa,_),(_,nxb,_)) <- succPairs nodeVars ]
  succNodeMinDX   = [ r1 nxa .+. con 1 :<= r1 nxb | ((n1,_),(n2,_)) <- edges
                    , let (_,nxa,_) = nodeVars !! n1, let (_,nxb,_) = nodeVars !! n2 ]
  -- all straight switches in this model (else constraints on dy1 >= 1.0,r dy2 >= 1.0, or similar, apply)
  switchRotation  = [ r1 ey :== r1 ny | (e@((n1,p1),(n2,_)),ey) <- edgeVars,
                      let (node1,_,ny) = nodeVars !! n1, straightOut node1 p1 ] ++ 
                    [ r1 ey :== r1 ny | (e@((n1,_),(n2,p2)),ey) <- edgeVars,
                      let (node2,_,ny) = nodeVars !! n2, straightIn node2 p2 ]
  nodeDXAbsDy     = [ r1 nxa .+. (absy e ey nya nyb) 
                             .+. con (addEdgeLength (shapeUp na p1) (shapeDown nb p2)) :<= r1 nxb  
                    | (e@((n1,p1),(n2,p2)),ey) <- edgeVars
                    , let (na,nxa,nya) = nodeVars !! n1, let (nb,nxb,nyb) = nodeVars !! n2 ]
    where
      absy ((n1,p1),(n2,p2)) ey nya nyb = (r ey  (fa n1 p1) .-. r nya (fa n1 p1)) .+. 
                                          (r nyb (fb n2 p2) .-. r ey  (fb n2 p2))
      fa n1 p1 = (dirFactorOut (nodes !! n1) p1)
      fb n2 p2 = (dirFactorIn  (nodes !! n2) p2)

  --edgeShapeDx     = [ r1 nxa .+. con (addEdgeLength (shapeUp node1 p1) (shapeDown node2 p2))  :<= r1 nxb 
  --                  | ((n1,p1),(n2,p2)) <- edges 
  --                  , let (node1,nxa,_) = nodeVars !! n1, let (node2,nxb,_) = nodeVars !! n2 ]

  shapeUp (SwitchNode SLeft Up) PLeft = EUp
  shapeUp (SwitchNode SLeft Up) PRight = EStraight
  shapeUp (SwitchNode SRight Up) PLeft = EStraight
  shapeUp (SwitchNode SRight Up) PRight = EDown
  shapeUp (SwitchNode _ Down) PTrunk = EStraight
  shapeUp BeginNode PBegin = EStraight
  shapeUp _ _ = error "shapeUp"

  shapeDown (SwitchNode SLeft Down) PLeft = EUp
  shapeDown (SwitchNode SLeft Down) PRight = EStraight
  shapeDown (SwitchNode SRight Down) PLeft = EStraight
  shapeDown (SwitchNode SRight Down) PRight = EDown
  shapeDown (SwitchNode _ Up) PTrunk = EStraight
  shapeDown EndNode PEnd = EStraight
  shapeDown _ _ = error "shapeUp"

  --minEdgeLength :: EShape -> EShape -> Double
  addEdgeLength EUp EDown = 1
  addEdgeLength EDown EUp = 1
  addEdgeLength EStraight _ = 1
  addEdgeLength _ EStraight = 1
  addEdgeLength _ _ = 0


  dirFactorOut (SwitchNode SRight Up) PRight = -1.0
  dirFactorOut _ _ = 1.0
  dirFactorIn  (SwitchNode SRight Down) PRight = -1.0
  dirFactorIn _ _ = 1.0

  straightOut (SwitchNode SRight Up) PRight = False
  straightOut (SwitchNode SLeft Up) PLeft = False
  straightOut _ _ = True

  straightIn (SwitchNode SRight Down) PRight = False
  straightIn (SwitchNode SLeft  Down) PLeft = False
  straightIn _ _ = True
  
  edgeLt = transitiveReduction $ ltRel nodes edges

  allVars = concat [ concat [ [x,y] | (_,x,y) <- nodeVars ],
                     [ ey | (_,ey) <- edgeVars ] ]

  succXDiff = [ (r1 x2 .-. r1 x1) | ((n1,_),(n2,_)) <- edges
              , let (_,x1,_) = nodeVars !! n1, let (_,x2,_) = nodeVars !! n2 ]
  succYDiff = [ (r1 y2 .-. r1 y1) | (a,b) <- edgeLt
              , let (_,y1) = edgeVars !! a, let (_,y2) = edgeVars !! b ]

  lp :: Either Error (Assignment [Char] [Char] IntDouble)
  lp = solve (program Minimise objective constraints bounds)
    where 
      objective = foldl1 (.+.) (succXDiff ++ succYDiff) -- TODO absdy -- TODO distribute labels
      constraints = foldl1 (:&&) (edgeYOrdering ++ nodeDXAbsDy ++ nodeOrder ++ succNodeMinDX ++ switchRotation)
      bounds = [ lowerR 0 x | x <- allVars ]

  assignments = case lp of
    Right (Assignment _ rs) -> rs
    _ -> error $ "LP failed " ++ (show lp)

  output = LayoutOutput outputNodes outputEdges outputLabels
  outputLabels = [] -- TODO
  outputEdges = [ (e, unwrapR $ assignments Map.! ey) | (e,ey) <- edgeVars ]
  outputNodes = [ (n,(unwrapR $ assignments Map.! nx, 
                      unwrapR $ assignments Map.! ny)) | (n,nx,ny) <- nodeVars ]





