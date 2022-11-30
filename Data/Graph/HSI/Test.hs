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
    faceGetRelPos,
    -- Polytope
    Polytope(..),
    polyInsertHalfspace,
    polyRelPos,
    polyStats,
    checkFormulaEuler,
    visPoly,
    -- HSI Algorithm
    hsiStep,
    relPosPoly,
    wipeOut0,
    hsiIntersectHMin,
    hsiIntersectH0

 ) where

import Data.Graph.HSI.Polytope
import Data.Graph.HSI.Halfspace
import Data.Graph.HSI.Face
import Data.Graph.HSI.RelPos
import Data.Graph.HSI.Init
import Data.Graph.HSI.Algorithm
import Data.Graph.HSI.Visibility
