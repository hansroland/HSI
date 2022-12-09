module Data.Graph.HSI.InitCube (mkCube) where

import Data.Graph.Dag
import Data.Graph.HSI.Face ( Visibility(Hidden), Face(..) )
import Data.Graph.HSI.Halfspace ( HsKey, hsFromList, Halfspace, Dim(..) )
import Data.Graph.HSI.Polytope ( Polytope(..) )

import Data.List ( elemIndices )
import qualified Data.EnumMap as Map
import qualified Data.Vector.Unboxed as VU

-- --------------------------------------------------------------------
-- Coefficients for a Halfspace equation for a cube
-- --------------------------------------------------------------------
data Coeff = Minus | Zero | Plus
    deriving (Eq, Show)

-- Constants
coValMax, coValMin:: Double
coValMax =  10000
coValMin = negate coValMax

-- Value of a coefficient to calculate the coordinate of a cube vertex
coCoordVal :: Coeff -> Double
coCoordVal Zero  =  0
coCoordVal Minus = coValMax
coCoordVal Plus  = coValMin

-- Value of a coefficient to calculate the halfspace
coHsCoeff :: Coeff -> Double
coHsCoeff Zero = 0
coHsCoeff Minus = coValMin
coHsCoeff Plus = coValMax

-- --------------------------------------------------------------------
-- A CubeFace (short CFace) is a list of Coeffs
-- --------------------------------------------------------------------
type CFace = [Coeff]

-- Calculate the dimenstion of a CFace
cfDim::  CFace -> Dim
cfDim = Dim . length . filter (== Zero)

-- Calculate the NodeKey of a CFace
nodekeyVal :: CFace -> NodeKey
nodekeyVal cFace = foldr (+) 0 $ zipWith (*) (nkeyVal <$> cFace)  powers3
  where
    powers3 :: [NodeKey]
    powers3 = [ 3^n | n <- [(0:: Int)..] ]
     -- Value of a coefficient to calculate the NodeKey of a CFace
    nkeyVal :: Coeff -> NodeKey
    nkeyVal Zero  = 0
    nkeyVal Minus = 2
    nkeyVal Plus  = 1

-- Calculate the HsKeys of a CFace
cfHskeyVals :: CFace -> [HsKey]
cfHskeyVals cf = hskey <$> poks
  where
    poks = filter ((/= 0) . snd) $  zip [(0::HsKey)..] $ hkeyVal <$> cf
    hskey (a,b) = a * (2::HsKey) + b
    hkeyVal Zero  = 0
    hkeyVal Minus = 2
    hkeyVal Plus  = 1

-- Create the sub-CFaces for a Zero at position n
-- to get the sub-CFaces of a CFace we generate for every Zero 2 new
--    CFaces, where we replace the Zero by a Plus and a Minus
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

-- --------------------------------------------------------------------
-- Create a polytope of a given dimension
-- --------------------------------------------------------------------
mkCube :: Dim -> Polytope
mkCube dim = Polytope { polyHs = hsmap, polyDag = dag}
  where
    cubeFaces :: [CFace]
    cubeFaces = genAllCFaces dim

    hsmap :: Map.EnumMap HsKey Halfspace
    hsmap = Map.fromList $ zip keys hsvects
      where
        dim2s = filter (\cf -> cfDim cf == (dim - 1)) cubeFaces
        keys = (head . cfHskeyVals ) <$> dim2s
        hsvects = mkHalfspace <$> dim2s

    dag :: Dag Face
    dag =  Dag {start = 0, dagMap = dagmap }
      where
        dagmap = Map.fromList $ zip keys nodes
        keys = nodekeyVal <$> cubeFaces
        nodes = mkNode <$> cubeFaces

    mkNode :: CFace -> Node Face
    mkNode cf =
        let face = mkFace cf
            kids = subs cf
        in Node {nodeKids = kids, nodeData = face}

    mkFace :: CFace -> Face
    mkFace cf =
        let hsKeys = cfHskeyVals cf
        in  if cfDim  cf== 0
            then Vertex mempty (mkVector cf) hsKeys
            else Nonvert mempty (cfDim cf) hsKeys Hidden

    mkHalfspace :: CFace -> Halfspace
    mkHalfspace cf = hsFromList $ (coHsCoeff <$> cf) ++  [coValMin]

    mkVector :: CFace -> VU.Vector Double
    mkVector cf = VU.fromList $ (coCoordVal <$> cf)

-- -------------------------------------------------------------------
-- Generate all CFaces for a n-dimensional cube
-- -------------------------------------------------------------------
genAllCFaces :: Dim -> [CFace]
genAllCFaces  dim  = variateRep dim [Zero, Minus, Plus]
  where
    -- See package `Combinatorics` from Hackage
    variateRep :: Dim -> [a] -> [[a]]
    variateRep n x =
        if n<0 then [] else iter n (\y -> concatMap (\z -> map (z:) y) x) [[]]
-- Compositional power of a function, i.e. apply the function @n@ times to a value.
    iter :: Dim -> (a -> a) -> a -> a
    iter 0 _ x = x
    iter n f x = f (iter (n-1) f x)
