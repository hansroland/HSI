{-# Language NamedFieldPuns #-}

module Data.Graph.HSI.Algorithm where

import Data.Graph.HSI.Polytope
import Data.Graph.HSI.Face
import Data.Graph.HSI.Halfspace
import Data.Graph.HSI.RelPos
import Data.Graph.HSI.Utils
import Data.Foldable ( foldlM )

import Data.Graph.Dag

import qualified Data.Vector.Unboxed as VU
import qualified Data.EnumMap as Map
import qualified Data.Set as Set

import Data.Set (Set)
import Data.List ( (\\) )
import Data.Maybe (fromJust)
import Control.Monad.State.Strict ( State, unless, when )

-- Calculate polytope from halfspaces
hsiPoly :: HsiPolytope -> [Halfspace] -> Either String HsiPolytope
hsiPoly poly hss = foldlM hsiStep poly hss

-- Apply one single halfspace to the polytope
hsiStep :: HsiPolytope -> Halfspace -> Either String HsiPolytope
hsiStep poly hs = do
  let poly1 = hsiRelPosPoly hs poly
      relHs = nodeAttr $ dagStartNode $ polyDag poly1
  checkRelHs relHs hs poly1

checkRelHs :: RelPos -> Halfspace -> HsiPolytope -> Either String HsiPolytope
checkRelHs relPos hs poly
      | relPos == relPos0  || relPos == relPosM0  = Left "Polytope is degraded"
      | relPos == relPosM                         = Left ("Polytope is empty" ++ show poly)
      | relPos == relPosP  || relPos == relPosP0  = pure poly   -- Halfspace is redundant
      | relPos == relPosMP || relPos == relPosM0P =
        pure $ hsiRed
             $ hsiIntersectH0
             $ polyInsertHalfspace hs
             $ hsiIntersectHMin poly
      | otherwise = Left ("checkRelHs got strange relPos:" ++ show relPos)

-- Calculate the RelPos for a polytope
hsiRelPosPoly :: Halfspace -> HsiPolytope -> HsiPolytope
hsiRelPosPoly hs poly@Polytope {polyDag = dag0 } =
    poly {polyDag = newDag }
  where
    newDag = postOrder Single relPosNode () dag0

    -- Calculate the relPos for a single node
    relPosNode :: NodeFunction Face RelPos ()
    relPosNode (key, node) = do
      dag <- getDag
      putDag $ dagUpdateNode dag key node {nodeAttr = relPosFace node dag}
    -- Calculate the relative position for a vertex
    relPosFace :: HsiNode -> HsiDag -> RelPos
    relPosFace Node{nodeData = Vertex vec _ } _ = calcRelPosVertex vec
    -- Calculate the relative position for a nonVertex
    relPosFace node@Node{nodeData = Nonvert _ _ } dag =
        mconcat $ (nodeAttr . dagNode dag) <$> nodeKids node
    -- Calculate relPos for a Vertex
    calcRelPosVertex :: VU.Vector Double -> RelPos
    calcRelPosVertex vertex = relPos $ roundDouble $ distance hs vertex
      where
        relPos :: Double -> RelPos
        relPos x
            | x < -eps = relPosM
            | x >  eps = relPosP
            | otherwise = relPos0
        eps = 0.01

-- Remove Nodes, that are now outside the new halfspace
hsiIntersectHMin :: HsiPolytope -> HsiPolytope
hsiIntersectHMin poly@Polytope {polyDag} =
    poly {polyDag = postOrder Single nodeRemove Set.empty polyDag}
  where
    nodeRemove :: NodeFunction  Face RelPos (Set NodeKey)
    nodeRemove (key, node@Node{nodeKids, nodeAttr}) = do
      dag@Dag {dagNodes} <- getDag
      deleted <- getUstate
      let (newDag, newDeleted) =
            if nodeAttr == relPosM || nodeAttr == relPosM0
              then (dag {dagNodes = Map.delete key dagNodes}, Set.insert key deleted )
              else
                let newNode = node {nodeKids = nodeKids \\ (Set.toList deleted)}
                in (dagUpdateNode dag key newNode, deleted)
      putDag newDag
      putUstate newDeleted

-- Intersect with H0.
-- Calculate and add new faces.
hsiIntersectH0 :: (HsKey, HsiPolytope) -> HsiPolytope
hsiIntersectH0 (hskey, poly@Polytope{polyHs, polyDag}) = poly{polyDag = newDag}
  where
    newDag = postOrderFilter Multiple (processNode  polyHs) isToProcess () polyDag
    processNode :: HsMap -> NodeFunction Face RelPos ()
    processNode hsmap (key,node) = do
      dag <- getDag
      unless (hasH0Kids dag node) $ do
          -- get grandchildren of parent that are in H0
          let grandKids = dagGrandNodes dag node
              grandKidsKeys = fst <$>
                filter (\gk -> relPos0 == nodeAttr (snd gk)) grandKids
          newKey <- mkNewNode hsmap grandKidsKeys node
          linkToParent newKey key
      pure ()

    -- Make new new Node with a face: insert it into the dag
    -- and return both, key and node.
    -- TODO Make a HsiDagAlgo type synonym
    mkNewNode :: HsMap -> [NodeKey] -> HsiNode -> State (DagAlgoData Face RelPos u) NodeKey
    mkNewNode hsmap kids node = do
      dag <- getDag
      let dim = nodeDim node
          (newSubs, newFace) =
            if dim == 1
                then let hsKeys = nodeHsKeys node ++ [hskey]
                         vec = mkVert hsKeys hsmap
                     in  ([], Vertex vec hsKeys )
                else let hsKeys = hskey : nodeHsKeys node
                     in  (kids, Nonvert (dim -1) hsKeys)
          newnode = node{ nodeKids = newSubs, nodeData = newFace, nodeAttr = relPos0 }
          (newKey, dag1) = dagInsertNode newnode dag
      putDag dag1
      pure newKey

    -- Return true, if this node has points on both sides of the halfspace
    isToProcess :: NodePredicate Face RelPos
    isToProcess node =
      let relPos = nodeAttr node
      in  relPos == relPosMP || relPos == relPosM0P

    -- Calculate a new vertex
    mkVert :: [HsKey] -> HsMap -> VU.Vector Double
    mkVert hsKeys hsmap = calculateVertex hsmap hsKeys
    -- Note: `calculateVertex` may throw an error, when the linear
    --       equation system has no solution. This should not occur,
    --       because we calculate only if we have a point on each
    --       side of the halfspace!

    -- Connect a node to a parent node
    linkToParent :: NodeKey -> NodeKey -> State (DagAlgoData Face RelPos u) ()
    linkToParent newSubkey parentKey = do
      dag <- getDag
      let parentNode = dagNode dag parentKey
          updParent = nodeAddKey newSubkey parentNode
      putDag $ dagUpdateNode dag parentKey updParent

    -- Has the node any kids in H0
    hasH0Kids :: HsiDag -> HsiNode -> Bool
    hasH0Kids dag node =
      let isH0Key :: NodeKey -> Bool
          isH0Key = (== relPos0) . nodeAttr . dagNode dag
      in any isH0Key $ nodeKids node

-- Redundancy processing:
hsiRed :: HsiPolytope -> HsiPolytope
hsiRed poly@Polytope {polyDag, polyHs} = poly {polyDag = newDag, polyHs = newHs}
  where
    facetDim = polyDim poly - 1
    newDag = preOrder Multiple addHs  Nothing $
               preOrder Single clearHss () polyDag
    newHs = consHss newDag polyHs
    -- Clear out the HsKeys for all faces with a dimension below the facet-dim
    clearHss :: NodeFunction Face RelPos ()
    clearHss (key,node@Node{nodeData}) = do
      dag <- getDag
      when ((nodeDim node) < facetDim) $ do
              putDag $ dagUpdateNode dag key
                $  nodeUpdateData (faceSetHsKeys [] nodeData) node
      pure ()
    addHs :: NodeFunction Face RelPos (Maybe HsKey)
    addHs (key,node@Node{nodeData})
        -- All faces other than polytope and facets. (In 3D: vertices and edges)
        -- Add the HsKey from the supporting facet to all decendants.
      | (faceDim nodeData) < facetDim = do
         dag <- getDag
         ustate <- getUstate
         let newNode = node{ nodeData = addHsKey nodeData $ fromJust ustate }
         putDag $ dagUpdateNode dag key newNode
         -- The facet: Store the HsKey of the facet in the user-state
      | (nodeDim node) == facetDim = do
         putUstate $ Just $ head $ nodeHsKeys node
         -- The polytope
      | otherwise = pure ()

    consHss :: Dag Face RelPos -> HsMap -> HsMap
    consHss dag hsmap =
      let facetKeys = nodeKids $ dagStartNode dag
          faces = (nodeData . dagNode dag) <$> facetKeys
          usedKeys = concat $ faceHsKeys <$> faces
          nonused = Map.keys hsmap \\ usedKeys
      in  foldr (Map.delete) hsmap nonused
