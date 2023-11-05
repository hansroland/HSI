{-# Language DerivingStrategies #-}

module Data.Graph.HSI.Face where

import Data.Graph.HSI.Halfspace ( HsKey, Dim )
import Data.Graph.HSI.RelPos ( RelPos )
import Data.Graph.Dag

import Data.Vector.Unboxed (Vector)
import Data.Containers.ListUtils(nubOrd)

-- A Face is and vertex, edge, hyperplane or side of a polytope
data Face = Nonvert !Dim ![HsKey]                   -- A Non-Vertex stores its dimension
                                                    -- and the keys of the supporting Halfspaces
          | Vertex !(Vector Double) ![HsKey]        -- A Vertex stores its coordiantes
    deriving (Show)                                 -- and the keys of the supporting Halfspaces

type HsiNode = Node Face RelPos
type HsiDag = Dag Face RelPos

-- Check, whether a this face is a vertex or not
isVertex :: Face -> Bool
isVertex (Vertex _ _ ) = True
isVertex _             = False

-- Make a new Face representing a vertex
mkVertex :: [HsKey] -> Vector Double -> Face
mkVertex hsKeys v = Vertex v hsKeys

-- Add a new Halfspace key to a Face
addHsKey :: Face -> HsKey -> Face
addHsKey (Nonvert dim keys) k = Nonvert dim $ nubOrd $ k : keys
addHsKey (Vertex vect keys) k = Vertex vect $ nubOrd $ k : keys

-- Set the HsKeys into a Face
faceSetHsKeys :: [HsKey] -> Face -> Face
faceSetHsKeys keys (Nonvert dim _) = Nonvert dim keys
faceSetHsKeys keys (Vertex  vec _) = Vertex  vec keys

-- Get the List of the HsKeys
faceHsKeys :: Face -> [HsKey]
faceHsKeys (Nonvert _ hsKeys) = hsKeys
faceHsKeys (Vertex  _ hsKeys) = hsKeys

-- Get the dimension
faceDim :: Face -> Dim
faceDim (Nonvert  dim _ ) = dim
faceDim (Vertex _ _ ) = 0

-- ----------------------------------------------------------------
-- Face accessor function for nodes
-- ----------------------------------------------------------------
-- Return the dimension of a Face
nodeDim :: Node Face a -> Dim
nodeDim = faceDim . nodeData

-- Return the keys to the Halfspace map
nodeHsKeys :: Node Face a -> [HsKey]
nodeHsKeys = faceHsKeys . nodeData

-- Set the attribute of a node
nodeSetAttr :: a -> Node Face a -> Node Face a
nodeSetAttr attr node@Node{} = node { nodeAttr = attr}
