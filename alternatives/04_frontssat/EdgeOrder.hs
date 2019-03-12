module EdgeOrder where

import Input

import qualified Data.Set as Set
import qualified Data.Map as Map
import Data.Set (Set)
import Data.Map (Map)
import Data.Maybe
import Data.List (partition, sortBy)
import Data.Ord (Ordering(..))

relationSort :: Ord a => Set (a,a) -> [a] -> [a]
relationSort rel = sortBy f
  where
    f a b | a == b = EQ
    f a b | (a,b) `Set.member` rel = LT
    f a b | (b,a) `Set.member` rel = GT
    f _ _ = error "Incomplete order relation"

transitiveClosure :: [(Int,Int)] -> [(Int,Int)]
transitiveClosure l = Set.toList (go (Set.fromList l))
  where go s = if (Set.size s) == Set.size (join s) then s else go (join s)
        join s = Set.union s $ Set.fromList [ (a,c) | (a,b) <- Set.toList s
                                                    , (b,c) <- Set.toList (setRangeInclusive s b b)]

transitiveReduction :: [(Int,Int)] -> [(Int,Int)]
transitiveReduction xs = [ (a,c) | (a,c) <- xs, not (reachable c [ b | (_,b) <- next a, b /= c ]) ]
  where
    s = Set.fromList xs
    next x = Set.toList (setRangeInclusive s x x)
    reachable :: Int -> [Int] -> Bool
    reachable goal [] = False
    reachable goal (x:xs) | x == goal = True
    reachable goal (x:xs) = reachable goal ([ b | (_,b) <- next x ] ++ xs)

setRangeInclusive :: Set (Int,Int) -> Int -> Int -> Set (Int,Int)
setRangeInclusive s lo hi = justRight
  where (_, highEnough) = Set.split ((lo-1),2^29-1) s
        (justRight, _ ) = Set.split ((hi+1),-2^29) highEnough



type Prio = Set (Int, EdgeRef)

ltRel :: [Node] -> [Edge] -> [(EdgeRef, EdgeRef)]
ltRel nodes edges = Set.toList $ Set.fromList allLt
  where
    (switchLeftEdge,switchRightEdge,switchTrunkEdge) = ((Map.!) switchLeftEdges, (Map.!) switchRightEdges,
                                                        (Map.!) switchTrunkEdges)
    switchLeftEdges  = Map.fromList [ (ni, ei) | (ei,e) <- zip [0..] edges, (ni, PLeft)  <- [fst e, snd e] ]
    switchRightEdges = Map.fromList [ (ni, ei) | (ei,e) <- zip [0..] edges, (ni, PRight) <- [fst e, snd e] ]
    switchTrunkEdges = Map.fromList [ (ni, ei) | (ei,e) <- zip [0..] edges, (ni, PTrunk) <- [fst e, snd e] ]

    dirFactor :: Dir -> Int
    dirFactor Up = 1
    dirFactor Down = -1

    allLt = [ (under,over) | (i, (SwitchNode side dir)) <- zip [0..] nodes
                           , let (overs,unders) = findIt i dir
                           , over <- Set.toList overs, under <- Set.toList unders ]

    findIt :: Int -> Dir -> (Set EdgeRef, Set EdgeRef)
    findIt n dir = go (Set.singleton (edgeNode (top n)))
                      (Set.singleton (edgeNode (bottom n)))
                      (Set.singleton (f*(edgeNode (top n)), top n))
                      (Set.singleton (f*(edgeNode (bottom n)), bottom n))
                      Set.empty Set.empty

      where
        go :: Set NodeRef -> Set NodeRef -> Prio -> Prio -> Set EdgeRef -> Set EdgeRef -> (Set EdgeRef, Set EdgeRef)
        go oNodes uNodes oQueue uQueue oEdges uEdges | Set.null oQueue || Set.null uQueue || not (Set.null (Set.intersection oNodes uNodes))  = (rest oEdges oQueue, rest uEdges uQueue)
        go oNodes uNodes oQueue uQueue oEdges uEdges = if overPos < underPos then goOver else goUnder
          where
            ((overPos,overEdge),(underPos,underEdge)) = (Set.elemAt 0 oQueue, Set.elemAt 0 uQueue)
            goOver  = go newONodes uNodes newOQueue uQueue (Set.insert overEdge oEdges) uEdges
            goUnder = go oNodes newUNodes oQueue newUQueue oEdges (Set.insert underEdge uEdges)
            newONodes = Set.union oNodes (Set.fromList [ edgeNode e | e <- nextEdges (edgeNode overEdge)  ])
            newUNodes = Set.union uNodes (Set.fromList [ edgeNode e | e <- nextEdges (edgeNode underEdge) ])
            newOQueue = Set.union (Set.deleteAt 0 oQueue)
                          (Set.fromList [ (f*(edgeNode e), e) | e <- nextEdges (edgeNode overEdge)])
            newUQueue = Set.union (Set.deleteAt 0 uQueue)
                          (Set.fromList [ (f*(edgeNode e), e) | e <- nextEdges (edgeNode underEdge)])

        f = dirFactor dir

        top ni = case (dir, (nodes !! ni)) of
                  (Up, SwitchNode _ Up) -> switchLeftEdge ni
                  (Down, SwitchNode _ Down) -> switchRightEdge ni
                  _ -> error "top ni"

        bottom ni = case (dir, (nodes !! ni)) of
                     (Up, SwitchNode _ Up) -> switchRightEdge ni
                     (Down, SwitchNode _ Down) -> switchLeftEdge ni
                     _ -> error "bottom ni"

        edgeNode ei = case dir of
                        Up   -> fst (snd (edges!!ei))
                        Down -> fst (fst (edges!!ei))

        nextEdges ni = case (dir, (nodes !! ni)) of
                              (Up, EndNode) -> []
                              (Up, SwitchNode _ Up) -> [switchLeftEdge ni, switchRightEdge ni]
                              (Up, SwitchNode _ Down) -> [switchTrunkEdge ni]

                              (Down, BeginNode) -> []
                              (Down, SwitchNode _ Down) -> [switchLeftEdge ni, switchRightEdge ni]
                              (Down, SwitchNode _ Up) -> [switchTrunkEdge ni]

                              _ -> error "nextEdges ni"

    rest :: Set EdgeRef -> Prio -> Set EdgeRef
    rest edges queue = Set.union edges (Set.map snd queue)


