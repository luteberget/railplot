module TikzOutput where

import Lin
import Input
import Parser
import Data.List (intercalate)

test :: String -> String -> IO ()
test fn o = do
  x <- parseTopoFile fn
  let i = case x of 
        (Right i) ->  i
        (Left err) -> error err
  let (SolverInput n e _ ) = i 
  let output@(LayoutOutput ln le _) = layout n e []
  let tikz = tikzOutput output
  putStrLn tikz


-- succPairs :: [a] -> [(a,a)]
-- succPairs [] = []
-- succPairs xs = xs `zip` (tail xs)

mkLine :: (Double,Double) -> Double -> (Double,Double) -> [(Double,Double)]
mkLine (x1,y1) y (x2,y2) = concat [[(x1,y1)],
    if absy1 > 1e-5 then [(x1+absy1,y)] else [],
    if absy2 > 1e-5 then [(x2-absy2,y)] else [],[(x2,y2)]]
  where
    absy1 = (abs (y-y1))
    absy2 = (abs (y2-y))

tikzLine :: [(Double,Double)] -> String
tikzLine l = "\\draw[track] " ++ (intercalate "--" (fmap tikzCoord l)) ++ ";"

tikzCoord (x,y) = "(" ++ (s x) ++ "," ++ (s y) ++ ")"
  where s x = (show (( fromIntegral (round (x*100.0)))/100.0))

edgeLine :: [(Node, (Double, Double))] -> (Edge, Double) -> [(Double,Double)]
edgeLine nodes (((n1,p1),(n2,p2)), y) = mkLine (find n1) y (find n2)
  where find n = head [ (x,y) | (i,(_n,(x,y))) <- zip [0..] nodes, i == n ]

tikzOutput :: LayoutOutput -> String
tikzOutput (LayoutOutput nodes edges labels) = showlines ++ "\n" ++ switches
  where 
    lines = fmap (edgeLine nodes) edges
    showlines = intercalate "\n" (fmap tikzLine lines)
    switches = intercalate "\n" (fmap (tikzSwitch edges lines) [ (i, dirFactor dir, x) 
                                                         | (i, ((SwitchNode _ dir),x)) <- zip [0..] nodes ])

dirFactor Up   = 1.0
dirFactor Down = -1.0

tikzSwitch :: [(Edge,Double)] -> [[(Double,Double)]] -> (Int, Double,(Double,Double)) -> String
tikzSwitch forwardEdges forwardLines (ni,f,(x,y)) = "\\fill " ++ (intercalate "--" (fmap tikzCoord points)) ++ ";"
  where
    lines = if f > 0 then forwardLines else fmap reverse forwardLines
    edges = (if f > 0 then fmap (\(e,x) -> fst e) else fmap (\(e,x) -> snd e)) forwardEdges
    findEdge port = head [ lines !! i | (i,(n,p)) <- zip [0..] edges, n == ni, p == port ]
    tangent xs = unitXVector $ head [ (x1-x0,y1-y0) | ((x0,y0),(x1,y1)) <- succPairs xs ]
    unitXVector (x,y) = (x/(abs x), y/(abs x))
    addVector (a,b) (x,y) = (a+0.25*x, b+0.25*y)
    points = [(x,y), addVector (x,y) (tangent (findEdge PLeft)), addVector (x,y) (tangent (findEdge PRight)) ]


