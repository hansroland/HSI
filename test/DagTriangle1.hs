module DagTriangle1 where

import Data.Graph.Dag

type Triangle1 = (Int, Int, [Int])

triangle11 :: Dag Triangle1
-- Create a triangle
triangle11 = -- dagLinkParents [3,4] 7
     -- $ dagLinkParents [2,4] 6
     -- $ dagLinkParents [2,3] 5
     -- $ dagLinkSubnodes 1 [2,3,4]
     dagCreateNode 7 [] (700, 0, [])
     $ dagCreateNode 6 [] (600, 0, [])
     $ dagCreateNode 5 [] (500, 0, [])
     $ dagCreateNode 4 [6,7] (400, 0, [])
     $ dagCreateNode 3 [5,7] (300, 0, [])
     $ dagCreateNode 2 [5,6] (200, 0, [])
     $ dagCreateNode 1 [2,3,4] (100, 0, [])
     $ dagInit 1

rslt_triangle11 :: String
rslt_triangle11 = "DAG start = 1\n" ++
    "1 => [2,3,4] (100,0,[])\n" ++
    "2 => [5,6] (200,0,[])\n" ++
    "3 => [5,7] (300,0,[])\n" ++
    "4 => [6,7] (400,0,[])\n" ++
    "5 => [] (500,0,[])\n" ++
    "6 => [] (600,0,[])\n" ++
    "7 => [] (700,0,[])\n"


processNode1 :: ((Int, Int, [Int]) ->  [(Int, Int, [Int])] -> ((Int, Int, [Int]), Bool))
processNode1 (n, c, cs) subloades = ( (n + 1, n, cs), True)
    -- copy first component to second
    -- increment first component

grandkids :: [NodeKey]
grandkids = fst <$> dagGrandNodes triangle11 (dagNode triangle11 1)
