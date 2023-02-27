{-# Language OverloadedStrings #-}

module Data.Graph.HSI.LinearEquationSolver (solve) where

import qualified Data.Text as T
import qualified Data.Vector as V
import qualified Data.Vector.Unboxed as VU


type Equation = VU.Vector Double
type Matrix = V.Vector Equation

cLIMIT :: Double
cLIMIT = 0.001

cNONSOLVABLE:: T.Text
cNONSOLVABLE = "Attempt to solve a non-solvable equation system"

solve :: Matrix -> Either T.Text (VU.Vector Double)
solve mat = checkMatrix mat >>= calcTriangle >>= backInsert

-- -------------------------------------------------------------------
-- Check input
-- -------------------------------------------------------------------

checkMatrix :: Matrix -> Either T.Text Matrix
checkMatrix mat = checkMatrixSameLength mat >>= checkRowsCols

checkMatrixSameLength :: Matrix -> Either T.Text Matrix
checkMatrixSameLength mat = do
    let length1 = VU.length $ V.head mat
    if all (\r -> (VU.length r == length1)) mat
        then Right mat
        else Left "Not all equations have the same length"

checkRowsCols :: Matrix -> Either T.Text Matrix
checkRowsCols mat = do
    let numRows = V.length mat
        numCols = VU.length $ V.head mat
    if numRows + 1 == numCols
        then Right mat
        else Left "Number of rows is not compatible with number of columns"

-- -------------------------------------------------------------------
-- Calculation
-- -------------------------------------------------------------------
calcTriangle :: Matrix -> Either T.Text (V.Vector Equation)
calcTriangle mat= V.unfoldrExactNM (V.length mat) pivotStep mat

pivotStep :: Matrix -> Either T.Text (Equation, Matrix)
pivotStep  mat =
    if abs negPivot < cLIMIT
      then Left cNONSOLVABLE
      else Right $ (rowp, V.map newRow newMat)
  where
    (rowp, newMat) = getNextPivot mat
    tailrowp = VU.tail rowp
    negPivot = negate $ VU.head rowp
    -- Apply the pivot to a row
    newRow :: Equation -> Equation
    newRow row = VU.zipWith (+)
                     (applyPivot (VU.head row))
                     (VU.tail row)
    applyPivot :: Double -> Equation
    applyPivot hdRow = VU.map (hdRow / negPivot *) tailrowp

-- Find biggest pivot in first column.
-- Remove row with pivot from matrix
-- Return pivot row and new matrix
getNextPivot :: Matrix -> (Equation, Matrix)
getNextPivot mat
    | len > 1 = (rowp, strtRows <> V.tail endRows)
    | otherwise = (V.head mat, V.empty)
  where
    len = V.length mat
    -- Get the index of the row with biggest pivot
    -- Versions with elemIndex or maxIndexBy are much slower
    -- absvect = V.map (abs . VU.head) mat
    -- ixabs = V.map swap $ V.indexed absvect
    -- ixrow = snd $ V.maximum ixabs
    -- ixrow = V.maxIndexBy (\x y -> compare (abshead x) (abshead y)) mat
    -- abshead = abs . VU.head
    ixrow = snd $ V.maximum $ V.zip (V.map (abs . VU.head) mat) (V.enumFromN 0 len)
    (strtRows, endRows) = V.splitAt ixrow mat
    rowp = V.head endRows

backInsert :: V.Vector Equation -> Either T.Text (VU.Vector Double)
backInsert eqs = Right $ V.foldr stepInsert VU.empty eqs
  where
    stepInsert :: Equation -> VU.Vector Double -> VU.Vector Double
    stepInsert equat xs =
        let piv = VU.head equat
            as = (VU.tail . VU.init) equat
            s = VU.last equat - (VU.sum $ VU.zipWith (*) as xs)
        in VU.cons (s/piv)  xs
