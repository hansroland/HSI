{-# Language NamedFieldPuns #-}

module DagTriangle4 where

import Data.Graph.Dag

type Triangle4 = (Bool,Int)

triangle41 :: Dag Triangle4
-- Create a triangle
triangle41 = -- dagLinkParents [3,4] 7
     -- $ dagLinkParents [2,4] 6
     -- $ dagLinkParents [2,3] 5
     -- $ dagLinkSubnodes 1 [2,3,4]
     dagCreateNode 7 [] (True, 700)
     $ dagCreateNode 6 [] (True, 600)
     $ dagCreateNode 5 [] (True, 500)
     $ dagCreateNode 4 [6,7] (False, 400)
     $ dagCreateNode 3 [5,7] (True, 300)
     $ dagCreateNode 2 [5,6] (False, 200)
     $ dagCreateNode 1 [2,3,4] (True, 100)
     $ dagInit 1

nodeDataPred ::  NodePredicate (Bool, Int)
nodeDataPred (Node{nodeData = (bool, _)}) = bool

-- Lift up data from bottom nodes to above nodes
-- type NodeFunction a = (NodeKey, Node a) ->  Dag a -> Dag a

nodeFunct :: NodeFunction (Bool, Int) ()
nodeFunct (nodekey, Node {nodeKids, nodeData}) = do
    dag <- getDag
    let newNode = Node {nodeKids, nodeData = (fst nodeData, 1 + snd nodeData)}
    putDag $  dagUpdateNode dag nodekey newNode


-- Processing on a single
triangle42 :: Dag Triangle4
triangle42 = postOrderSingleFilter nodeFunct nodeDataPred () triangle41

rslt_triangle42 :: String
rslt_triangle42 = "DAG start = 1\n" ++
    "1 => [2,3,4] (True,101)\n" ++
    "2 => [5,6] (False,200)\n" ++
    "3 => [5,7] (True,301)\n" ++
    "4 => [6,7] (False,400)\n" ++
    "5 => [] (True,501)\n" ++
    "6 => [] (True,600)\n" ++
    "7 => [] (True,701)\n"
