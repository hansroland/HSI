{-# Language NamedFieldPuns #-}

module Data.Graph.HSI.Init (mkPyramid, mkTriangle)
    where

import Data.Graph.Dag
import Data.Graph.HSI.Face
import Data.Graph.HSI.RelPos
import Data.Graph.HSI.Halfspace ( HsKey(..), Dim(..), HsMap, hsFromList, hsNormalize )
import Data.Graph.HSI.Polytope

import qualified Data.Vector.Unboxed as VU

import qualified Data.EnumMap.Strict as Map


-- ATTENTION: The HsiKeys to the Faces start with 2!!

sqrt2 :: Double
sqrt2 = sqrt 2

sqrt2h :: Double
sqrt2h = sqrt2 / 2

sqrt2_100 :: Double
sqrt2_100 =sqrt2 * 100

-- -----------------------------------------------------------------
-- 3-dim pyramide
-- -----------------------------------------------------------------

mkPyramid :: HsiPolytope
mkPyramid = Polytope { polyHs = hsPyramid, polyDag = mkPyramidDag }

-- Create the graph structure for the Pyramid
mkPyramidDag :: Dag Face RelPos                          -- where to define type for Dag Face RelPos
mkPyramidDag = addNonvert 1 [2,3,4,5,6] 3 []
     $ addNonvert 2 [ 7,  8,  9]     2 [2]
     $ addNonvert 3 [ 7, 10, 13]     2 [3]
     $ addNonvert 4 [ 9, 10, 11, 12] 2 [1]  -- base
     $ addNonvert 5 [ 8, 11, 14]     2 [5]
     $ addNonvert 6 [12, 13, 14]     2 [4]
     -- 1-dim faces
     $ addNonvert 7 [15, 17]         1 [2,3]
     $ addNonvert 8 [16,17]          1 [2,5]
     $ addNonvert 9 [15,16]          1 [1,2]
     $ addNonvert 10 [15,18]         1 [1,3]
     $ addNonvert 11 [16,19]         1 [1,5]
     $ addNonvert 12 [18,19]         1 [1,4]
     $ addNonvert 13 [17,18]         1 [3,4]
     $ addNonvert 14 [17,19]         1 [5,4]
     -- Vertices
     -- TODO: Remove hs from vertex data structure or add correct data !!
     $ addVertex 15 [1,2,3] [ 300.0,  300.0,  -100.0]
     $ addVertex 16 [1,2,5] [-300.0,  300.0,  -100.0]
     $ addVertex 17 [2,3,4,5] [ 0.0,    0.0,   200.0]
     $ addVertex 18 [1,3,4] [ 300.0, -300.0,  -100.0]
     $ addVertex 19 [1,4,5] [-300.0, -300.0,  -100.0]
     -- $ addVertex 100 [] []                             -- to debug hsi algo
     $ dagInit 1


hsPyramid :: HsMap
hsPyramid = Map.fromAscList $ zip [1..] hsList
     where
          hsList = map hsFromList  [[0,0,1, -100],                        -- 1
                                    [0, -sqrt2h, -sqrt2h, -sqrt2_100],    -- 2
                                    [-sqrt2h, 0, -sqrt2h, -sqrt2_100],    -- 3
                                    [0,  sqrt2h, -sqrt2h, -sqrt2_100],    -- 4
                                    [sqrt2h, 0,  -sqrt2h, -sqrt2_100]]    -- 5

-- -----------------------------------------------------------------
-- 2-dim triangle
-- -----------------------------------------------------------------

mkTriangle :: HsiPolytope
mkTriangle = Polytope { polyHs = hsTri, polyDag = mkTriangleDag }

-- Create the graph structure for a Triangle
mkTriangleDag :: Dag Face RelPos
mkTriangleDag =  addNonvert 1 [2,3,4] 2 []
     $ addNonvert 2 [5,6] 1 [2]
     $ addNonvert 3 [5,7] 1 [3]
     $ addNonvert 4 [7,6] 1 [4]
     $ addVertex 5 [2,3] [ 0,  2]
     $ addVertex 6 [2,4] [-4, -2]
     $ addVertex 7 [3,4] [ 4, -2]
     $ dagInit 1

hsTri :: HsMap
hsTri = Map.fromAscList $ zip [2..] hsList
     where
          hsList = map (hsNormalize . hsFromList )
               [[ sqrt2h,  -sqrt2h, -sqrt2],   --2
               [ -sqrt2h, -sqrt2h, -sqrt2],    --3
               [0, 1, -2] ]                    --4

-- ---------------------------------------------------------------------------
-- Local helper function to add nodes
-- ----------------------------------------------------------------------------
addNonvert :: NodeKey -> [NodeKey] -> Dim -> [HsKey]  -> Dag Face RelPos -> Dag Face RelPos
addNonvert nodeKey subKeys dim hsKeys dag =
     let face = Nonvert dim hsKeys
         node = Node {nodeKids = subKeys, nodeData = face, nodeAttr = mempty}
     in  dagUpdateNode dag nodeKey node

addVertex :: NodeKey -> [HsKey] -> [Double] -> HsiDag -> HsiDag
addVertex nodeKey hsKeys coords dag = dagCreateNode nodeKey [] (mkVertex hsKeys vec) mempty dag
     where vec = VU.fromList coords
