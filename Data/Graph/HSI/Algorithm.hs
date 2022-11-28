{-# Language NamedFieldPuns #-}

module Data.Graph.HSI.Algorithm where

import Data.Graph.HSI.Polytope
import Data.Graph.HSI.Face
import Data.Graph.HSI.Halfspace
import Data.Graph.HSI.RelHsPos
import Data.Graph.HSI.Utils

import Data.Graph.Dag

import qualified Data.Vector.Unboxed as VU

import qualified Data.EnumMap as Map

import Data.Set (Set)
import qualified Data.Set as Set
import Data.List ( partition, (\\) )
import Control.Monad

import Debug.Trace ( trace )

-- Apply one single halfspace to the polytope
hsiStep :: Polytope -> Halfspace -> Either String Polytope
hsiStep poly hs = do
  let poly1 = trace ("Process halfspace:" ++ show hs) $ relHsPosPoly hs poly
      dag = polyDag poly1
      relHs = faceGetRelHsPos $ nodeData $ dagNode dag $ start dag
  checkRelHs relHs hs poly1


checkRelHs :: RelHsPos -> Halfspace -> Polytope -> Either String Polytope
checkRelHs relPos0 hs0 poly0  = go (wipeOutOnEdge relPos0) hs0 poly0
  where
    go relPos hs poly
      | relPos == mempty    = Left "Polytope is degraded"
      | relPos == outside   = Left ("Polytope is empty" ++ show poly)
      | relPos == inside    = pure poly
      | relPos == bothside  =
        pure $ hsiIntersectH0
          $ polyInsertHalfspace hs
          $ hsiIntersectHMin poly
      | otherwise = Left ("checkRelHs got strange relPos:" ++ show relPos)

-- Calculate the relHsPos for a polytope
relHsPosPoly :: Halfspace -> Polytope -> Polytope
relHsPosPoly hs poly@Polytope {polyDag = dag0 } =
    poly {polyDag = newDag }
  where
    -- First process the leaves
    dag1 = dagUpdateLeafs  (setHsPosLeaf hs) dag0
    -- then process all the non-leaf faces
    newDag = postOrderSingleState relHsPosNode () dag1

    -- Set the newly calculated relHsPos to a vertex node
    setHsPosLeaf :: Halfspace -> Face -> Face
    setHsPosLeaf _ edge@(Edge _ _ _ _) = edge
    setHsPosLeaf hs0 (Vertex _ vec hskeys) =
      Vertex (relHsPosVertex hs0 vec) vec hskeys
    -- Calculate the relHsPos for a single node
    relHsPosNode :: NodeFunctionState Face ()
    relHsPosNode (key, node@Node{nodeData = pLoad}) = do
      dag <- getDag
      let pairsKeysRelpos = dagMapSubnodesKey (faceGetRelHsPos . nodeData) dag node
          relHsPos = mconcat $ fmap snd pairsKeysRelpos
          newKids = sortOnEdge pairsKeysRelpos
          newNode = node{nodeKids = newKids, nodeData = relHsPosEdge relHsPos pLoad}
      putDag $ dagUpdateNode dag key newNode
    -- Sort: Move the onEdge nodes at the beginning of the kids list
    sortOnEdge :: [(NodeKey, RelHsPos)] -> [NodeKey]
    sortOnEdge keysRelpos =
      let (onEdgies, others) = partition (isOnEdge . snd) keysRelpos
      in  (fst <$> onEdgies) ++ (fst <$> others)
    -- Set the relHsPos in a non vertex edge
    relHsPosEdge :: RelHsPos -> Face -> Face
    relHsPosEdge rpos (Edge _ dim hskeys vis) = Edge rpos dim hskeys vis
    relHsPosEdge _ vertex@(Vertex _ _ _)       = vertex
    -- Calculate relHsPos for a Vertex
    relHsPosVertex :: Halfspace -> VU.Vector Double -> RelHsPos
    relHsPosVertex (Halfspace vs) vertex = relHsPos $ roundDouble $ dotp - VU.last vs
      where
        dotp = VU.sum $ VU.zipWith (*) (VU.init vs) vertex
        relHsPos :: Double -> RelHsPos
        relHsPos x
            | x < -eps = outside
            | x >  eps = inside
            | otherwise = onEdge
        eps = 0.01

-- Remove Nodes, that are now outside the new halfspace
hsiIntersectHMin :: Polytope -> Polytope
hsiIntersectHMin poly@Polytope {polyDag} =
    poly {polyDag = postOrderSingleState nodeRemove Set.empty polyDag}
  where
    nodeRemove :: NodeFunctionState  Face (Set NodeKey)
    nodeRemove (key, node@Node{nodeKids, nodeData}) = do
      dag@Dag {dagMap} <- getDag
      deleted <- getUstate
      let relPos = faceGetRelHsPos $ nodeData
          (newDag, newDeleted) =
            if relPos == outside || relPos == outsideEdge
              then (dag {dagMap = Map.delete key dagMap}, Set.insert key deleted )
              else
                let newNode = node {nodeKids = nodeKids \\ (Set.toList deleted)}
                in (dagUpdateNode dag key newNode, deleted)
      putDag newDag
      putUstate newDeleted

-- ------------------------------------------------------------------------------------------

-- Add the necessary new faces to the polytope
hsiIntersectH0 :: (HsKey, Polytope) -> Polytope
hsiIntersectH0 (hskey, poly@Polytope{polyHs, polyDag}) = poly{polyDag = newDag}
  where
    newDag = postOrderMultipleStateFilter (processNode  polyHs) isToProcess () polyDag

    processNode :: HsMap -> NodeFunctionState Face ()
    processNode hsmap (key,node) = do
      dag <- getDag
      let fstKey = head $ nodeKids node
          -- get grandchildren of parent that are in H0
          grandKids = dagGrandNodes dag node
          grandKidsKeys = fst <$>
              filter (\gk -> onEdge == faceGetRelHsPos (nodeData (snd gk) )) grandKids

          fstNode = dagNode dag $ fstKey
          fstNodeData = nodeData fstNode
          xrelHsPos = faceGetRelHsPos $ nodeData node

      -- TODO: Clean up this code, do we really need to check again for bothside or allside ??
      unless (faceGetRelHsPos fstNodeData == onEdge) $ do
              let dag2 = if (xrelHsPos == bothside || xrelHsPos == allside)
                            then let (newKey, dag1) = mkNewNode hsmap grandKidsKeys dag node
                                 in  linkToParent newKey key dag1
                            else trace  ("ATTENTION Algorithms hit code that was regarded as dead code") $ dag     -- This is the case, if the node is in H+
              putDag dag2
      pure ()

    -- Make new new Node with a face: insert it into the dag
    -- and return both, key and node.
    -- TODO: Try to change this function to state monad
    mkNewNode :: HsMap -> [NodeKey] -> Dag Face -> Node Face -> (NodeKey, Dag Face)
    mkNewNode hsmap kids dag node =
      let dim = nodeDim node
          (newSubs, newFace) =
            if dim == 1
                then let hsKeys = nodeHsKeys node ++ [hskey]
                         vec = mkVert hsKeys hsmap
                     in  ([], Vertex onEdge vec hsKeys )
                else let hsKeys = hskey : nodeHsKeys node
                     in  (kids, Edge onEdge (dim -1) hsKeys Hidden)
          newnode = node{ nodeKids = newSubs, nodeData = newFace}
          (newKey, dag1) = dagInsertNode newnode dag
      in  (newKey, dag1)

    -- Return true, if this node has points on both sides of the halfspace
    isToProcess :: NodePredicate Face
    isToProcess node =
      let relPos = (faceGetRelHsPos . nodeData) node
      in  isBothside relPos || isAllside relPos

    -- Calculate a new vertex
    mkVert :: [HsKey] -> HsMap -> VU.Vector Double
    mkVert hsKeys hsmap = calculateVertex hsmap hsKeys
    -- Note: `calculateVertex` may throw an error, when the linear
    --       equation system has no solution. This should not occur,
    --       because we calculate only if we have a point on each
    --       side of the halfspace!

    -- Connect a node to a parent node
    -- TODO: Try to change this function to state monad
    linkToParent :: NodeKey -> NodeKey -> Dag Face -> Dag Face
    linkToParent newSubkey parentKey dag =
          let parentNodeFromDag = dagNode dag parentKey
              updParent = nodeAddKey newSubkey parentNodeFromDag
          in  dagUpdateNode dag parentKey updParent
