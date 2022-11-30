{-# Language NamedFieldPuns #-}

module Data.Graph.HSI.Algorithm where

import Data.Graph.HSI.Polytope
import Data.Graph.HSI.Face
import Data.Graph.HSI.Halfspace
import Data.Graph.HSI.RelPos
import Data.Graph.HSI.Utils

import Data.Graph.Dag

import qualified Data.Vector.Unboxed as VU
import qualified Data.EnumMap as Map
import qualified Data.Set as Set

import Data.Set (Set)
import Data.List ( partition, (\\) )
import Control.Monad.State.Strict ( State, unless, when )

import Debug.Trace ( trace )

-- Apply one single halfspace to the polytope
hsiStep :: Polytope -> Halfspace -> Either String Polytope
hsiStep poly hs = do
  let poly1 = trace ("Process halfspace:" ++ show hs) $ relPosPoly hs poly
      dag = polyDag poly1
      relHs = faceGetRelPos $ nodeData $ dagNode dag $ start dag
  checkRelHs relHs hs poly1


checkRelHs :: RelPos -> Halfspace -> Polytope -> Either String Polytope
checkRelHs relPos' hs0 poly0  = go (wipeOut0 relPos') hs0 poly0
  where
    go relPos hs poly
      | relPos == mempty    = Left "Polytope is degraded"
      | relPos == relPosM   = Left ("Polytope is empty" ++ show poly)
      | relPos == relPosP    = pure poly
      | relPos == relPosMP  =                 --TODO Check for M0P ??
        pure $ hsiIntersectH0
          $ polyInsertHalfspace hs
          $ hsiIntersectHMin poly
      | otherwise = Left ("checkRelHs got strange relPos:" ++ show relPos)

-- Calculate the RelPos for a polytope
-- TODO: Avoid preprocessing of the vertices, so we can save dagUpdateLeafs
relPosPoly :: Halfspace -> Polytope -> Polytope
relPosPoly hs poly@Polytope {polyDag = dag0 } =
    poly {polyDag = newDag }
  where
    -- First process the leaves
    dag1 = dagUpdateLeafs  (setHsPosLeaf hs) dag0
    -- then process all the non-leaf faces
    newDag = postOrderSingle relPosNode () dag1

    -- Set the newly calculated relPos to a vertex node
    setHsPosLeaf :: Halfspace -> Face -> Face
    setHsPosLeaf _ edge@(Edge _ _ _ _) = edge
    setHsPosLeaf hs0 (Vertex _ vec hskeys) =
      Vertex (relPosVertex hs0 vec) vec hskeys
    -- Calculate the relPos for a single node
    relPosNode :: NodeFunction Face ()
    relPosNode (key, node@Node{nodeData = pLoad}) = do
      dag <- getDag
      let pairsKeysRelpos = dagMapSubnodesKey (faceGetRelPos . nodeData) dag node
          relPos = mconcat $ fmap snd pairsKeysRelpos
          newKids = sortOnEdge pairsKeysRelpos
          newNode = node{nodeKids = newKids, nodeData = relPosEdge relPos pLoad}
      putDag $ dagUpdateNode dag key newNode
    -- Sort: Move the onEdge nodes at the beginning of the kids list
    sortOnEdge :: [(NodeKey, RelPos)] -> [NodeKey]
    sortOnEdge keysRelpos =
      let (onEdgies, others) = partition ((relPos0 ==) . snd) keysRelpos
      in  (fst <$> onEdgies) ++ (fst <$> others)
    -- Set the relPos in a non vertex edge
    relPosEdge :: RelPos -> Face -> Face
    relPosEdge rpos (Edge _ dim hskeys vis) = Edge rpos dim hskeys vis
    relPosEdge _ vertex@(Vertex _ _ _)       = vertex
    -- Calculate relPos for a Vertex
    relPosVertex :: Halfspace -> VU.Vector Double -> RelPos
    relPosVertex (Halfspace vs) vertex = relPos $ roundDouble $ dotp - VU.last vs
      where
        dotp = VU.sum $ VU.zipWith (*) (VU.init vs) vertex
        relPos :: Double -> RelPos
        relPos x
            | x < -eps = relPosM
            | x >  eps = relPosP
            | otherwise = relPos0
        eps = 0.01

-- Remove Nodes, that are now outside the new halfspace
hsiIntersectHMin :: Polytope -> Polytope
hsiIntersectHMin poly@Polytope {polyDag} =
    poly {polyDag = postOrderSingle nodeRemove Set.empty polyDag}
  where
    nodeRemove :: NodeFunction  Face (Set NodeKey)
    nodeRemove (key, node@Node{nodeKids, nodeData}) = do
      dag@Dag {dagMap} <- getDag
      deleted <- getUstate
      let relPos = faceGetRelPos $ nodeData
          (newDag, newDeleted) =
            if relPos == relPosM || relPos == relPosM0
              then (dag {dagMap = Map.delete key dagMap}, Set.insert key deleted )
              else
                let newNode = node {nodeKids = nodeKids \\ (Set.toList deleted)}
                in (dagUpdateNode dag key newNode, deleted)
      putDag newDag
      putUstate newDeleted

-- Intersect with H0.
-- Calculate and add new faces.
hsiIntersectH0 :: (HsKey, Polytope) -> Polytope
hsiIntersectH0 (hskey, poly@Polytope{polyHs, polyDag}) = poly{polyDag = newDag}
  where
    newDag = postOrderMultipleFilter (processNode  polyHs) isToProcess () polyDag

    processNode :: HsMap -> NodeFunction Face ()
    processNode hsmap (key,node) = do
      dag <- getDag
      let fstKey = head $ nodeKids node
          -- get grandchildren of parent that are in H0
          grandKids = dagGrandNodes dag node
          grandKidsKeys = fst <$>
              filter (\gk -> relPos0 == faceGetRelPos (nodeData (snd gk) )) grandKids

          fstNode = dagNode dag $ fstKey
          fstNodeData = nodeData fstNode
          relPos = faceGetRelPos $ nodeData node

      -- TODO: Clean up this code, do we really need to check again for bothside or allside ??
      unless (faceGetRelPos fstNodeData == relPos0) $ do
          when (relPos == relPosMP || relPos == relPosM0P) $ do
              newKey <- mkNewNode hsmap grandKidsKeys node
              linkToParent newKey key
      pure ()

    -- Make new new Node with a face: insert it into the dag
    -- and return both, key and node.
    mkNewNode :: HsMap -> [NodeKey] -> Node Face -> State (DagAlgoData Face u) NodeKey
    mkNewNode hsmap kids node = do
      dag <- getDag
      let dim = nodeDim node
          (newSubs, newFace) =
            if dim == 1
                then let hsKeys = nodeHsKeys node ++ [hskey]
                         vec = mkVert hsKeys hsmap
                     in  ([], Vertex relPos0 vec hsKeys )
                else let hsKeys = hskey : nodeHsKeys node
                     in  (kids, Edge relPos0 (dim -1) hsKeys Hidden)   -- TODO Check relPos !!!
          newnode = node{ nodeKids = newSubs, nodeData = newFace}
          (newKey, dag1) = dagInsertNode newnode dag
      putDag dag1
      pure newKey

    -- Return true, if this node has points on both sides of the halfspace
    isToProcess :: NodePredicate Face
    isToProcess node =
      let relPos = (faceGetRelPos . nodeData) node
      in  relPos == relPosMP || relPos == relPosM0P

    -- Calculate a new vertex
    mkVert :: [HsKey] -> HsMap -> VU.Vector Double
    mkVert hsKeys hsmap = calculateVertex hsmap hsKeys
    -- Note: `calculateVertex` may throw an error, when the linear
    --       equation system has no solution. This should not occur,
    --       because we calculate only if we have a point on each
    --       side of the halfspace!

    -- Connect a node to a parent node
    linkToParent :: NodeKey -> NodeKey -> State (DagAlgoData Face u) ()
    linkToParent newSubkey parentKey = do
      dag <- getDag
      let parentNode = dagNode dag parentKey
          updParent = nodeAddKey newSubkey parentNode
      putDag $ dagUpdateNode dag parentKey updParent
