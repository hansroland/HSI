-- A module that exports functions, to be used for tests

module Data.Graph.HSI.Test (
    -- Init
    mkPyramid,
    mkTriangle,
    -- Halfspace
    Halfspace(..),
    hsFromList,
    normalize,
    -- Face
    faceGetRelHsPos,
    -- Polytope
    Polytope(..),
    polyInsertHalfspace,
    polyRelHsPos,
    polyStats,
    checkFormulaEuler,
    visPoly,
    -- HSI Algorithm
    hsiStep,
    relHsPosPoly,
    wipeOutOnEdge,
    hsiIntersectHMin,
    hsiIntersectH0

 ) where

import Data.Graph.HSI.Polytope
import Data.Graph.HSI.Halfspace
import Data.Graph.HSI.Face
import Data.Graph.HSI.RelHsPos
import Data.Graph.HSI.Init
import Data.Graph.HSI.Algorithm
import Data.Graph.HSI.Visibility
