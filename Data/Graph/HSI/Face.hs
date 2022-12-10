{-# Language GeneralisedNewtypeDeriving, DerivingStrategies #-}
{-# Language NamedFieldPuns #-}

module Data.Graph.HSI.Face where

import Data.Graph.HSI.Halfspace ( HsKey, Dim )
import Data.Graph.HSI.RelPos ( RelPos )
import Data.Graph.Dag

import Data.Vector.Unboxed (Vector)

-- A Face is and vertex, edge, hyperplane or side of a polytope
data Face = Nonvert !Dim ![HsKey]
          | Vertex !(Vector Double) ![HsKey]
     deriving (Show)

type HsiNode = Node Face RelPos       -- TODO Move to a better position !!!
type HsiDag = Dag Face RelPos

type VisNode = Node Face Visibility
type VisDag = Dag Face Visibility

-- Datatype for visibility of faces   --TODO Move to the visible module !!!!
data Visibility = Visible | Hidden
    deriving (Show, Eq)

-- Check, whether a this face is a vertex
isVertex :: Face -> Bool
isVertex (Vertex _ _ ) = True
isVertex _                = False

-- Make a new Face representing a vertex
mkVertex :: [HsKey] -> (Vector Double) -> Face
mkVertex hsKeys v = Vertex v hsKeys

-- Add an new Halfspace index to a Face
addHsKey :: Face -> HsKey -> Face
addHsKey (Nonvert dim keys) k = Nonvert dim (k : keys)
addHsKey edge _ = edge

-- Get the List of the HsKeys
faceHsKeys :: Face -> [HsKey]
faceHsKeys (Nonvert _   hsKeys) = hsKeys
faceHsKeys (Vertex _ hsKeys)   = hsKeys

-- Get the dimension
faceDim :: Face -> Dim
faceDim (Nonvert  dim _ ) = dim
faceDim (Vertex _ _ ) = 0

-- ----------------------------------------------------------------
-- Face accessor function for nodes
-- ----------------------------------------------------------------
-- return the dimension of a Face
nodeDim :: Node Face a -> Dim
nodeDim = faceDim . nodeData

-- return the keys to the Halfspace map
nodeHsKeys :: Node Face a -> [HsKey]
nodeHsKeys = faceHsKeys . nodeData

-- Set the attribute of a node
nodeSetAttr :: a -> Node Face a -> Node Face a
nodeSetAttr attr node@Node{} = node { nodeAttr = attr}
