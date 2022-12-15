module Data.Graph.HSI (
    Polytope(..),
    HsiPolytope, VisPolytope,
    NodeKey,
    Node,
    Face(..), VisDag, VisNode, HsiNode, HsiDag,
    Visibility(..),
    Halfspace(..),
    isVertex,
    polyNodeAssocs,
    nodeDim,
    nodeAttr,
    hsFromList,
    visPoly
)

where

import Data.Graph.Dag (Node(), NodeKey, nodeAttr )
import Data.Graph.HSI.Polytope
import Data.Graph.HSI.Halfspace
import Data.Graph.HSI.Face
import Data.Graph.HSI.Visibility

