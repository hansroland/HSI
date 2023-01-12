module Data.Graph.HSI (
    Polytope(..),
    HsiPolytope,
    NodeKey,
    Node,
    Face(..), Dim(..), HsiNode, HsiDag,
    Halfspace(..),
    hsiPoly,
    checkFormulaEuler,
    polyStats,
    isVertex,
    polyNodeAssocs,
    polyDim,
    nodeDim,
    nodeAttr,
    nodeHsKeys,
    nodeSetAttr,
    hsFromList,
    hsFromVector,
    hsEquation,
    sp,
    normalize
)

where

import Data.Graph.Dag (Node(), NodeKey, nodeAttr )
import Data.Graph.HSI.Algorithm(hsiPoly)
import Data.Graph.HSI.Polytope
import Data.Graph.HSI.Halfspace
import Data.Graph.HSI.Face
import Data.Graph.HSI.Utils
