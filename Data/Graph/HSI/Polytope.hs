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

-- Type Synonyms
type HsiPolytope = Polytope RelPos              -- A Polytope used during HSI Algorithm

-- See: https://ti.inf.ethz.ch/ew/courses/Geo20/lecture/gca20-9.pdf
data Polytope a = Polytope { polyHs :: !HsMap, polyDag :: !(Dag Face a)}

instance Show a => Show (Polytope a) where
  show poly@Polytope {polyHs}  =
       concat ("Polytope Halfspaces:" : nl : map showHsAssoc (Map.toAscList polyHs)) ++
       concat ("Polytope Faces:" : (map showNodeAssoc $ sortNodeAssoc $ polyNodeAssocs poly ))
    where
        showHsAssoc ::  (HsKey, Halfspace) -> String
        showHsAssoc (key, hs) = show key ++ " -> " ++ show hs ++ nl
        showNodeAssoc :: Show a => (NodeKey, Node Face a) -> String
        showNodeAssoc (k,n) =  '\n' : show k ++ "=>" ++ show n
        sortNodeAssoc :: [(NodeKey, Node Face a)] -> [(NodeKey, Node Face a)]
        sortNodeAssoc = sortBy cmpNode
        cmpNode :: (NodeKey, Node Face a) -> (NodeKey, Node Face a) -> Ordering
        cmpNode (k1,n1) (k2,n2)
          | nodeDim n1 > nodeDim n2 = LT
          | nodeDim n1 < nodeDim n2 = GT
          | otherwise = compare k1 k2
        nl = ['\n']

-- Return all the node assocs [(NodeKey, Node Face)]
polyNodeAssocs :: Polytope a -> [(NodeKey, Node Face a)]
polyNodeAssocs = dagNodeAssocs . polyDag

-- Return a list of all faces from a polytope
polyFaces :: Polytope a -> [Face]
polyFaces = fmap (nodeData . snd) . polyNodeAssocs

-- Get the attribute of the top (first). node of the dag
polyNodeAttr :: Polytope a -> a
polyNodeAttr = nodeAttr . dagStartNode . polyDag

-- Add a new halfspace to the polytope
polyInsertHalfspace :: Halfspace -> Polytope a -> (HsKey, Polytope a)
polyInsertHalfspace hs poly@Polytope {polyHs} =
    let maxKey = fromMaybe 0 $ fst <$> (Map.lookupMax $ polyHs)
        hsKey = (1 + maxKey)
        newhsmap = Map.insert hsKey hs polyHs
    in  (hsKey, poly {polyHs = newhsmap})


-- Calculate the vertex coordinates, from halfspace indices.
-- TODO write own equation solver without fromList / toList conversion !!
calculateVertex :: HsMap -> [HsKey] -> VU.Vector Double
calculateVertex hsmap keys =
    let fromRight :: Either String (VU.Vector Double) -> VU.Vector Double
        fromRight (Right b) = b
        fromRight (Left _) = error "Polytope.hs:calculateVertex returned left"
        echelon = rref $ fromLists $  hsMap (VU.toList . hsEquation) hsmap keys
        lastCol = ncols <$> echelon
        -- round and unbox the last column
        eiVertex = asUnboxed . roundVector <$> (getCol <$> lastCol <*> echelon)
    in fromRight eiVertex

-- Return the number of faces for each Dimension
polyStats :: Polytope a -> EnumMap Dim Int
polyStats = frequencies . fmap faceDim . polyFaces
  where
    frequencies :: Enum k => [k] -> EnumMap k Int
    frequencies ks = Map.fromListWith (+) $ zip ks (repeat 1)

-- Check whether Eulers formula is valid for the polytope.
-- Note: If the formula is not valid, then there was a bug in the algorithm.
checkFormulaEuler :: Polytope a -> String
checkFormulaEuler poly  =
    let dims = Map.elems $ polyStats poly
        alternateSeq = zipWith (*) dims $ cycle [1, -1]
        evenfact | odd $ length dims = 0      -- We have 0 as element too
                 | otherwise          = -1
        euler =  sum alternateSeq + evenfact
    in  case euler of
           0 -> "Euler Ok"
           _ -> "EULER VIOLATED"

-- Get the dimension of a polytope
polyDim :: HsiPolytope -> Dim
polyDim = nodeDim . dagStartNode . polyDag
