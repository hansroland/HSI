{-# Language NamedFieldPuns #-}
{-# Language OverloadedStrings #-}

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

import           Data.Text(Text)
import qualified Data.Text              as T

import Data.Set (Set)
import Data.List ( (\\) )
import Control.Monad.State.Strict ( State, unless )

-- Calculate polytope from halfspaces
hsiPoly :: HsiPolytope -> [Halfspace] -> Either Text HsiPolytope
hsiPoly poly hss = foldlM hsiStep poly hss >>= hsiCleanHs

-- Apply one single halfspace to the polytope
hsiStep :: HsiPolytope -> Halfspace -> Either Text HsiPolytope
hsiStep poly hs = do
    let poly1 = hsiRelPosPoly hs poly
        relPosPoly = nodeAttr $ dagStartNode $ polyDag poly1
    poly2 <- checkRelPosPoly relPosPoly poly1
    pure $ hsiIntersectH0 . polyInsertHalfspace hs . hsiIntersectHMin $ poly2
  where
    -- Check the realtive position of a polytope
    checkRelPosPoly :: RelPos -> HsiPolytope -> Either Text HsiPolytope
    checkRelPosPoly relPos poly1
      | relPos == relPos0  || relPos == relPosM0  = Left "Polytope is degraded"
      | relPos == relPosM                         = Left "Polytope is empty"
      | relPos == relPosP  || relPos == relPosP0  = pure poly1   -- Halfspace is redundant
      | relPos == relPosMP || relPos == relPosM0P = pure poly1
      | otherwise = Left $ T.pack ("checkRelHs got strange relPos:" <> show relPos)

-- Calculate the RelPos for a polytope
hsiRelPosPoly :: Halfspace -> HsiPolytope -> HsiPolytope
hsiRelPosPoly hs poly@Polytope {polyDag = dag0 } =
    poly {polyDag = newDag }
  where
    newDag = dsDag $ postOrder Single relPosNode () dag0
    -- Calculate the relPos for a single node
    relPosNode :: NodeFunction Face RelPos ()
    relPosNode (key, node) = do
      dag <- getDag
      putDag $ dagUpdateNode dag key node {nodeAttr = relPosFace node dag}
    relPosFace :: HsiNode -> HsiDag -> RelPos
    -- Calculate the relative position for a vertex
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

-- Intersect the current polytope with the H- part of a Halfspace.
-- Remove Nodes, that are now outside the new halfspace
hsiIntersectHMin :: HsiPolytope -> HsiPolytope
hsiIntersectHMin poly@Polytope {polyDag} =
    poly {polyDag = dsDag $ postOrder Single nodeRemove Set.empty polyDag}
  where
    nodeRemove :: NodeFunction  Face RelPos (Set NodeKey)
    nodeRemove (key, node@Node{nodeKids, nodeAttr}) = do
      dag@Dag {dagNodes} <- getDag
      deleted <- getClState
      let (newDag, newDeleted) =
            if nodeAttr == relPosM || nodeAttr == relPosM0
              then (dag {dagNodes = Map.delete key dagNodes}, Set.insert key deleted )
              else
                let newNode = node {nodeKids = nodeKids \\ (Set.toList deleted)}
                in (dagUpdateNode dag key newNode, deleted)
      putDag newDag
      putClState newDeleted

-- Intersect with H0.
-- Intersect all faces that belong to both sides of the halfspace with H0
hsiIntersectH0 :: (HsKey, HsiPolytope) -> HsiPolytope
hsiIntersectH0 (hskey, poly@Polytope{polyHs, polyDag}) = poly{polyDag = newDag}
  where
    newDag = dsDag $ postOrderFilter Multiple (processNode  polyHs) onBothSides () polyDag
    processNode :: HsMap -> NodeFunction Face RelPos ()
    processNode hsmap (key,node) = do
      dag <- getDag
      unless (hasH0Kids dag node) $ do
          -- The grandchildren in H0 of the current halfspace become the children
          -- of the new face.
          let grandKids = dagGrandNodes dag node
              grandKidsKeys = fst <$>
                filter (\gk -> relPos0 == nodeAttr (snd gk)) grandKids
          newKey <- mkNewNode hsmap grandKidsKeys node
          linkToParent newKey key
      pure ()

    -- Make a new Node with a face: insert it into the dag
    -- and return both, key and node.
    mkNewNode :: HsMap -> [NodeKey] -> HsiNode -> State (DagAlgoData Face RelPos c) NodeKey
    mkNewNode hsmap kids node = do
      dag <- getDag
      let dim = nodeDim node
          (newSubs, newFace) =
            if dim == 1
                then let hsKeys = nodeHsKeys node ++ [hskey]
                         vec = calculateVertex hsmap hsKeys
                     in  ([], Vertex vec hsKeys )
                -- Note: `calculateVertex` may throw an error, when the linear
                --       equation system has no solution. This should not occur,
                --       because we calculate only if we have a point on each
                 --       side of the halfspace!
              else let hsKeys = hskey : nodeHsKeys node
                     in  (kids, Nonvert (dim -1) hsKeys)
          newnode = node{ nodeKids = newSubs, nodeData = newFace, nodeAttr = relPos0 }
          (newKey, dag1) = dagInsertNode newnode dag
      putDag dag1
      pure newKey

    -- Return True, if this face has points on both sides of the halfspace
    onBothSides :: NodePredicate Face RelPos
    onBothSides node =
      let relPos = nodeAttr node
      in  relPos == relPosMP || relPos == relPosM0P

    -- Connect a node to a parent node
    linkToParent :: NodeKey -> NodeKey -> State (DagAlgoData Face RelPos u) ()
    linkToParent newSubkey parentKey = do
      dag <- getDag
      let parentNode = dagNode dag parentKey
          updParent = nodeAddKey newSubkey parentNode
      putDag $ dagUpdateNode dag parentKey updParent

    -- Has the face any subfaces in H0
    hasH0Kids :: HsiDag -> HsiNode -> Bool
    hasH0Kids dag node =
      let isH0Key :: NodeKey -> Bool
          isH0Key = (== relPos0) . nodeAttr . dagNode dag
      in any isH0Key $ nodeKids node

-- Remove all unused half-spaces from the HsList
-- Note: This is not a full redundandency processing
hsiCleanHs :: HsiPolytope -> Either Text HsiPolytope
hsiCleanHs poly@Polytope{polyHs, polyDag} = Right $ poly{ polyHs = newHs}
  where
    rsltDagAlgo = postOrder Single hsCollect Set.empty polyDag
    usedHs = dsClState rsltDagAlgo
    unusedHs = Set.difference (Set.fromList (Map.keys polyHs)) usedHs
    newHs = Set.foldr Map.delete polyHs unusedHs
    hsCollect :: NodeFunction  Face RelPos (Set HsKey)
    hsCollect (_, Node{nodeData}) = do
      usedHsk <- getClState
      putClState $ foldr Set.insert usedHsk $ faceHsKeys nodeData

{-
-- Redundancy processing:
hsiRed :: HsiPolytope -> Either Text HsiPolytope
hsiRed poly@Polytope {polyDag, polyHs} = pure $ poly {polyDag = newDag, polyHs = newHs}
  where
    polydim = polyDim poly
    facetDim = polydim - 1
    newDag = preOrder Multiple addHs  Nothing $
               preOrder Single clearHss () polyDag
    newHs = consHss newDag polyHs
    -- Clear out the HsKeys for all faces with a dimension below the facet-dim
    clearHss :: NodeFunction Face RelPos ()
    clearHss (key,node@Node{nodeData}) = do
      dag <- getDag
      when ((nodeDim node) < facetDim) $ do
              putDag $ dagUpdateNode dag key
                $  nodeSetData (faceSetHsKeys [] nodeData) node
      pure ()
    addHs :: NodeFunction Face RelPos (Maybe HsKey)
    addHs (key,node@Node{nodeData})
        -- All faces other than polytope and facets. (In 3D: vertices and edges)
        -- When we haven't yet enough supporting halfspace for the dimension of
        --   this node, we add the HsKey from the supporting facet to all decendants.
      | (faceDim nodeData) < facetDim = do
          when (Dim (length (faceHsKeys nodeData)) < (polydim - faceDim nodeData)) $ do
            dag <- getDag
            ustate <- getClState
            let newNode = node{ nodeData = addHsKey nodeData $ fromJust ustate }
            putDag $ dagUpdateNode dag key newNode
          pure ()
         -- The facet: Store the HsKey of the facet in the user-state
      | (nodeDim node) == facetDim = do
         putClState $ Just $ head $ nodeHsKeys node
         -- The polytope
      | otherwise = pure ()

    consHss :: Dag Face RelPos -> HsMap -> HsMap
    consHss dag hsmap =
      let facetKeys = nodeKids $ dagStartNode dag
          faces = (nodeData . dagNode dag) <$> facetKeys
          usedKeys = concat $ faceHsKeys <$> faces
          nonused = Map.keys hsmap \\ usedKeys
      in  foldr (Map.delete) hsmap nonused
-}