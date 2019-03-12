module Input where

--
-- INPUT REPRESENTATION
--
data Side = SLeft | SRight
  deriving (Show)
data Dir = Up | Down
  deriving (Show)
data Port = PLeft | PRight | PTrunk | PBegin | PEnd
  deriving (Show, Eq)

type EdgeRef = Int
type NodeRef = Int
type DNodeRef = Int

type PortRef = (NodeRef, Port)
type Edge = (PortRef, PortRef)
data Node = BeginNode | EndNode | SwitchNode Side Dir
  deriving (Show)

data LAlign = ALeft|ARight|ACenter deriving (Show, Eq, Ord)

data Label = Label {
  labelPos    :: (EdgeRef, NodeRef, Double),
  labelAlign  :: LAlign,
  labelLayer  :: Int,
  labelSymbol :: String
}  deriving (Show)

data LayoutOutput = LayoutOutput {
  lOutputNodes :: [(Node, (Double, Double))],
  lOutputEdges :: [(Edge, Double)], -- TODO: maybe actual line segments, generalizing from just the level.
  lOutputLabels :: [(Label, Double)] -- x coordinate of label insertion point
} deriving (Show)


isSwitch :: Node -> Bool
isSwitch (SwitchNode _ _) = True
isSwitch _ = False

data SolverInput = SolverInput 
  { sNodes :: [Node]
  , sEdges :: [Edge]
  , sEdgeLT :: [(EdgeRef, EdgeRef)]
  } deriving (Show)

example1 = SolverInput n e lt
  where n = [BeginNode, EndNode]
        e = [((0, PBegin),(1, PEnd))]
        lt = []

example2 = SolverInput n e lt
  where  n = [BeginNode, SwitchNode SLeft Up, EndNode, EndNode ]
         e = [((0, PBegin),(1,PTrunk)),
              ((1, PLeft),(2,PEnd)),
              ((1, PRight),(3,PEnd))]
         lt = [(2,1)]

example2b = SolverInput n e lt
  where  n = [BeginNode, SwitchNode SLeft Up, SwitchNode SRight Down, EndNode ]
         e = [((0, PBegin),(1,PTrunk)),
              ((1, PLeft),(2,PRight)),
              ((1, PRight),(2,PLeft)),
              ((2, PTrunk),(3,PEnd)) ]
         lt = [(2,1)]

example3 = SolverInput n e lt
  where  n = [BeginNode, BeginNode, SwitchNode SLeft Up, 
              SwitchNode SLeft Down,
              EndNode, EndNode ]
         e = [((0,PBegin),(2,PTrunk)), -- 0  edges:  1--4
              ((1,PBegin),(3,PRight)), -- 1           2
              ((2,PLeft),(3,PLeft)),   -- 2          0--3
              ((2,PRight),(4,PEnd)),   -- 3  nodes:   1   3 5
              ((3,PTrunk),(5,PEnd))]   -- 4           0 2   4
         lt = [(0, 2), (2, 1), (0, 1), (3,2),(3,4),(2,4)]

example4 = SolverInput n e lt
  where  n = [BeginNode, SwitchNode SLeft Up, SwitchNode SLeft Up, SwitchNode SRight Down, 
                SwitchNode SRight Down, EndNode]
         e = [((0,PBegin),(1,PTrunk)), -- 0
              ((1,PRight),(2,PTrunk)), -- 1
              ((2,PRight),(3,PLeft)), -- 2
              ((3,PTrunk),(4,PLeft)), -- 3
              ((4,PTrunk),(5,PEnd)), -- 4
              ((1,PLeft),(4,PRight)), -- 5
              ((2,PLeft),(3,PRight))] -- 6

---  
--
--        /---5----\
--       /  /-6-\   \
--    -0x-1x--2--x-3-x-4-
-- 15 26 65 35 (25)
--
             
         lt = [(1,5),(2,5),(3,5),(2,6),(5,6)]


example5 = SolverInput n e lt
  where  n = [BeginNode, SwitchNode SLeft Up, SwitchNode SRight Up, SwitchNode SLeft Down, 
                SwitchNode SRight Down, EndNode]
         e = [((0,PBegin),(1,PTrunk)), -- 0
              ((1,PLeft),(2,PTrunk)), -- 1
              ((1,PRight),(4,PLeft)), --2
              ((2,PLeft),(3,PRight)), --3
              ((2,PRight),(3,PLeft)), --4
              ((3,PTrunk),(4,PRight)), --5
              ((4,PTrunk),(5,PEnd))] --6

             
------
--      /-3--
--     ---4--\
--    1      5
-- -0---2-----\ -6
--
-- from 1-2  --> 1 < 2, 1 < 4, 
--
--
         lt = [(2,1),(2,4),(2,3),(2,5),(4,3)]
