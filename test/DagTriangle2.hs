module DagTriangle2 where

import Data.Graph.Dag

type Triangle2 = (Int,[Int])

triangle21 :: Dag Triangle2 ()
-- Create a triangle
triangle21 =
     dagCreateNode 7 [] (700, []) ()
     $ dagCreateNode 6 [] (600, []) ()
     $ dagCreateNode 5 [] (500, []) ()
     $ dagCreateNode 4 [6,7] (400, []) ()
     $ dagCreateNode 3 [5,7] (300, []) ()
     $ dagCreateNode 2 [5,6] (200, []) ()
     $ dagCreateNode 1 [2,3,4] (100, []) ()
     $ dagInit 1


-- Write visiting sequence into node
writeVisitingSequence :: NodeFunction Triangle2 () Int
writeVisitingSequence (key, node) = do
    dag <- getDag
    s <- getUstate
    let (n, ss) = nodeData node
    let s1 = s + 1
        newNode = nodeUpdateData node (n, s1:ss) ()
    putDag $ dagUpdateNode dag key newNode
    putUstate s1

-- Postorder processing with single visit to node
triangle22 :: Dag Triangle2 ()
triangle22 = postOrderSingle writeVisitingSequence 0 triangle21

rslt_triangle22 :: String
rslt_triangle22 = "DAG start = 1\n" ++
    "1 => [2,3,4] (100,[7])  ()\n" ++
    "2 => [5,6] (200,[3])  ()\n" ++
    "3 => [5,7] (300,[5])  ()\n" ++
    "4 => [6,7] (400,[6])  ()\n" ++
    "5 => [] (500,[1])  ()\n" ++
    "6 => [] (600,[2])  ()\n" ++
    "7 => [] (700,[4])  ()\n"


-- Postorder processing with multiple visits to a node
-- Add the visiting sequence to the nodes
triangle23 :: Dag Triangle2 ()
triangle23 = postOrderMultiple writeVisitingSequence 0 triangle21

rslt_triangle23 :: String
rslt_triangle23 = "DAG start = 1\n" ++
    "1 => [2,3,4] (100,[10])  ()\n" ++
    "2 => [5,6] (200,[3])  ()\n" ++
    "3 => [5,7] (300,[6])  ()\n" ++
    "4 => [6,7] (400,[9])  ()\n" ++
    "5 => [] (500,[4,1])  ()\n" ++
    "6 => [] (600,[7,2])  ()\n" ++
    "7 => [] (700,[8,5])  ()\n"
