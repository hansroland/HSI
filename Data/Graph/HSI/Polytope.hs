{-# Language GeneralisedNewtypeDeriving, DerivingStrategies #-}
{-# Language NamedFieldPuns #-}

{-# LANGUAGE BangPatterns #-}     -- TODO Remove this extension

module Data.Graph.HSI.Polytope where

import Data.Graph.HSI.Halfspace
import Data.Graph.HSI.Face
import Data.Graph.HSI.RelPos
import Data.Graph.HSI.Utils
import Data.Graph.Dag

import Data.EnumMap.Strict(EnumMap)
import qualified Data.EnumMap.Strict as Map

import Data.Matrix
import qualified Data.Vector.Unboxed as VU
import Data.Maybe (fromMaybe)
import Data.List ( sortBy )


type HsMap = EnumMap HsKey Halfspace

-- See: https://ti.inf.ethz.ch/ew/courses/Geo20/lecture/gca20-9.pdf
data Polytope = Polytope { polyHs :: HsMap, polyDag :: Dag Face}
    -- deriving (Show)

instance Show Polytope where
  show poly@Polytope {polyHs}  =
       concat ("Halfspaces" : nl : map showHsAssoc (Map.toAscList polyHs)) ++
       concat ("Faces" : (map showNodeAssoc $ sortNodeAssoc $ polyNodeAssocs poly ))
    where
        showHsAssoc ::  (HsKey, Halfspace) -> String
        showHsAssoc (key, hs) = show key ++ " -> " ++ show hs ++ nl
        showNodeAssoc :: (NodeKey, Node Face) -> String
        showNodeAssoc (k,n) =  '\n' : show k ++ "=>" ++ show n
        sortNodeAssoc :: [(NodeKey, Node Face)] -> [(NodeKey, Node Face)]
        sortNodeAssoc = sortBy cmpNode
        cmpNode :: (NodeKey, Node Face) -> (NodeKey, Node Face) -> Ordering
        cmpNode (k1,n1) (k2,n2)
          | nodeDim n1 > nodeDim n2 = LT
          | nodeDim n1 < nodeDim n2 = GT
          | otherwise = compare k1 k2
        nl = ['\n']

-- Return all the node assocs [(NodeKey, Node Face)]
polyNodeAssocs :: Polytope -> [(NodeKey, Node Face)]
polyNodeAssocs = dagNodeAssocs . polyDag

-- Return a list of all faces from a polytope
polyFaces :: Polytope -> [Face]
polyFaces = fmap (nodeData . snd) . dagNodeAssocs . polyDag

-- Return a list of all nodes of a polytope
polyNodes :: Polytope -> [Node Face]
polyNodes = fmap snd . dagNodeAssocs . polyDag

-- Add a new halfspace to the polytope
polyInsertHalfspace :: Halfspace -> Polytope -> (HsKey, Polytope)
polyInsertHalfspace hs poly@Polytope {polyHs} =
    let maxKey = fromMaybe 0 $ fst <$> (Map.lookupMax $ polyHs)
        hsKey = (1 + maxKey)
        newhsmap = Map.insert hsKey hs polyHs
    in  (hsKey, poly {polyHs = newhsmap})

-- Map a list of HsKeys to something
mapHs :: (Halfspace -> b) -> HsMap -> [HsKey] -> [b]
mapHs hsFun hsmap keys = (hsFun . (hsmap Map.!)) <$> keys

-- Get the relative halfspace position of a polytope
polyRelPos :: Polytope -> RelPos
polyRelPos Polytope {polyDag} =
    faceGetRelPos $ nodeData $ dagNode polyDag $ start polyDag

-- Calculate the vertex vector, from halfspace indices.
calculateVertex :: HsMap -> [HsKey] -> VU.Vector Double
calculateVertex hsmap keys =
    let fromRight :: Either String (VU.Vector Double) -> VU.Vector Double
        fromRight (Right b) = b
        fromRight (Left _) = error "Polytope.hs:calculateVertex returned left"
        echelon = rref $ fromLists $  mapHs (VU.toList . hsEquation) hsmap keys
        lastCol = ncols <$> echelon
        -- round and unbox the last column
        eiVertex = asUnboxed . roundVector <$> (getCol <$> lastCol <*> echelon)
    in fromRight eiVertex

-- Return the number of faces for each Dimension
polyStats :: Polytope -> EnumMap Dim Int
polyStats = frequencies . fmap faceDim . polyFaces
  where
    frequencies :: Enum k => [k] -> EnumMap k Int
    frequencies ks = Map.fromListWith (+) $ zip ks (repeat 1)

-- Check whether Eulers formula is valid for the polytope.
-- Note: If the formula is not valid, then there was a bug in the algorithm.
checkFormulaEuler :: Polytope -> String
checkFormulaEuler poly  =
    let dims = Map.elems $ polyStats poly
        alternateSeq = zipWith (*) dims $ cycle [1, -1]
        euler = sum alternateSeq
    in  case euler of
           1 -> "Euler Ok"
           _ -> "EULER VIOLATED"
