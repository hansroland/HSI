{-# Language GeneralisedNewtypeDeriving, DerivingStrategies #-}
{-# Language NamedFieldPuns #-}

module Data.Graph.HSI.Face where

import Data.Graph.HSI.Halfspace ( HsKey )
import Data.Graph.HSI.RelHsPos ( RelHsPos )
import Data.Graph.Dag

import Data.Vector.Unboxed (Vector)

-- A Face is and vertex, edge, hyperplane or side of a polytope
data Face = Edge !RelHsPos !Dim ![HsKey] !Visibility
          | Vertex !RelHsPos !(Vector Double) ![HsKey]
     deriving (Show)

-- Datatype to store the dimension of a face
newtype Dim = Dim Int
    deriving (Eq, Ord, Num, Enum)
    deriving newtype (Show)

-- Datatype for visibility of faces
data Visibility = Visible | Hidden
    deriving (Show, Eq)

-- Check, whether a this face is a vertex
isVertex :: Face -> Bool
isVertex (Vertex _ _ _ ) = True
isVertex _                = False

-- Make a new Face representing a vertex
mkVertex :: [HsKey] -> (Vector Double) -> Face
mkVertex hsKeys v = Vertex mempty v hsKeys

-- Add an new Halfspace index to a Face
addHsKey :: Face -> HsKey -> Face
addHsKey (Edge relHsPos dim keys vis) k = Edge relHsPos dim (k : keys) vis
addHsKey edge _ = edge

-- Get the relative Halfspace position
faceGetRelHsPos :: Face -> RelHsPos
faceGetRelHsPos (Edge relHsPos _ _ _) = relHsPos
faceGetRelHsPos (Vertex relHsPos _ _ ) = relHsPos

-- Get the List of the HsKeys
faceHsKeys :: Face -> [HsKey]
faceHsKeys (Edge _ _   hsKeys _) = hsKeys
faceHsKeys (Vertex _ _ hsKeys)   = hsKeys

-- Get the dimension
faceDim :: Face -> Dim
faceDim (Edge _  dim _ _) = dim
faceDim (Vertex _ _ _ ) = 0

faceVis :: Face -> Visibility
faceVis (Vertex _ _ _ ) = Hidden
faceVis (Edge _ _ _ vis) = vis

faceSetVis :: Face -> Visibility -> Face
faceSetVis v@(Vertex _ _ _) _ = v
faceSetVis (Edge relHsPos dim hskeys  _) vis = Edge relHsPos dim hskeys vis

-- ----------------------------------------------------------------
-- Face accessor function for nodes
-- ----------------------------------------------------------------
-- return the dimension of a Face
nodeDim :: Node Face -> Dim
nodeDim = faceDim . nodeData

-- return the visibility of a Face
nodeVis :: Node Face -> Visibility
nodeVis = faceVis . nodeData

nodeHsKeys :: Node Face -> [HsKey]
nodeHsKeys = faceHsKeys . nodeData

-- Face setters for nodes
nodeSetVis :: Visibility -> Node Face -> Node Face
nodeSetVis vis node =
    let face = nodeData node
        newFace = faceSetVis face vis
    in  nodeUpdateData node newFace
