module Data.Graph.HSI.InitCube where

import Data.Graph.Dag
import Data.Graph.HSI.Face
import Data.Graph.HSI.Halfspace
import Data.Graph.HSI.Polytope

import Data.List ( elemIndices )

import qualified Data.EnumMap as Map
import qualified Data.Vector.Unboxed as VU

-- Coefficients for a Halfspace equation for a cube
data Coeff = Minus | Zero | Plus
    deriving (Eq, Show)

-- Constants
coValMax :: Double
coValMax =  10000
coValMin :: Double
coValMin = -1 * coValMax

-- value of a coefficient to calculate the NodeKey of a CFace
coNodekeyVal :: Coeff -> NodeKey
coNodekeyVal Zero  = 0
coNodekeyVal Minus = 2
coNodekeyVal Plus  = 1

-- value of a coefficient to calculate the HsKey of a CFace
-- (only for faces with dim -1 )
coHskeyVal :: Coeff -> HsKey
coHskeyVal Zero  = 0
coHskeyVal Minus = 2
coHskeyVal Plus  = 1

-- Value of a coefficient to calculate the coordinate of a cube vertex
coCoordVal :: Coeff -> Double
coCoordVal Zero  =  0
coCoordVal Minus = coValMax
coCoordVal Plus  = coValMin


-- value of a coefficient to calculate the halfspace
coHsCoeff :: Coeff -> Double
coHsCoeff Zero = 0
coHsCoeff Minus = coValMin
coHsCoeff Plus = coValMax

-- A CubeFace (short CFace) is a list of Coeffs
type CFace = [Coeff]

-- Calculate the dimenstion of a CFace
cfDim::  CFace -> Dim
cfDim = Dim . length . filter (== Zero)

-- Generate all CFaces for a n-dimensional cube
genAllCFaces :: Int -> [CFace]
genAllCFaces  dim  = variateRep dim [Zero, Minus, Plus]

-- Calculate the NodeKey of a CFace
nodekeyVal :: CFace -> NodeKey
nodekeyVal cFace =
    let powers3 :: [NodeKey]
        powers3 = [ 3^n | n <- [(0:: Int)..] ]
    in  foldr (+) 0 $ zipWith (*) (coNodekeyVal <$> cFace)  powers3

-- Calculate the HsKeys of a CFace
hskeyVals :: CFace -> [HsKey]
hskeyVals cf =
    let poks = filter ((/= 0) . snd) $  zip [(0::HsKey)..] $ coHskeyVal <$> cf
        hskey (a,b) = a * (2::HsKey) + b
    in hskey <$> poks

-- create the sub-cFaces for a Zero at position n
-- to get the subfaces of a face we generate for every Zero 2 new
--    CFaces, where we replace the Zero by Plus and Minus
subsAtN :: CFace -> Int -> [CFace]
subsAtN cp n = [replCoeffAtN cp Plus n,  replCoeffAtN cp Minus n]
  where
    replCoeffAtN xs rep n' = take n' xs ++ (rep : drop (n' + 1) xs)

-- Generate the keys of the direct children of a CFace
subs ::  CFace -> [NodeKey]
subs es =
    let ixs = elemIndices Zero es
        cfaces = concat $ subsAtN es <$> ixs
    in  nodekeyVal <$> cfaces

-- make a face
mkFace :: CFace -> Face
mkFace cf =
    let hsKeys = hskeyVals cf
    in  if cfDim  cf== 0
         then Vertex mempty (cfVector cf) hsKeys
         else Nonvert mempty (cfDim cf) hsKeys Hidden

mkNode :: CFace -> Node Face
mkNode cf =
    let face = mkFace cf
        kids = subs cf
    in Node {nodeKids = kids, nodeData = face}


cfVector :: CFace -> VU.Vector Double
cfVector cf = VU.fromList $ (coCoordVal <$> cf)

cfHalfspace :: CFace -> Halfspace
cfHalfspace cf = hsFromList $ (coHsCoeff <$> cf) ++  [coValMin]


cubeFaces :: [CFace]
cubeFaces = genAllCFaces 3                                  -- TODO n-dim

hsmap :: Map.EnumMap HsKey Halfspace
hsmap = Map.fromList $ zip keys hsvects
  where
    dim2s = filter (\cf -> cfDim cf == 2) cubeFaces         -- TODO n-dim
    keys = (head . hskeyVals ) <$> dim2s
    hsvects = cfHalfspace <$> dim2s

dag :: Dag Face
dag =  Dag {start = 0, dagMap = dagmap }
  where
    dagmap = Map.fromList $ zip keys nodes
    keys = nodekeyVal <$> cubeFaces
    nodes = mkNode <$> cubeFaces


cube3 :: Polytope
cube3 = Polytope { polyHs = hsmap, polyDag = dag}

-- -------------------------------------------------------------------
-- Support functions
-- -------------------------------------------------------------------

-- See package `Combinatorics` from Hackage
variateRep :: Int -> [a] -> [[a]]
variateRep n x =
   if n<0 then [] else iter n (\y -> concatMap (\z -> map (z:) y) x) [[]]

{- |
Compositional power of a function,
i.e. apply the function @n@ times to a value.
See: Simon Thompson: \"The Craft of Functional Programming\", page 172
-}
{-# INLINE iter #-}
iter :: Int -> (a -> a) -> a -> a
iter 0 _ x = x
iter n f x = f (iter (n-1) f x)
