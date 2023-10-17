{-# Language OverloadedStrings #-}

module Data.Graph.HSI.LinearEquationSolver (solve) where

import qualified Data.Text as T
import qualified Data.Vector as V
import qualified Data.Vector.Unboxed as VU
import qualified VectorBuilder.Builder as B
import qualified VectorBuilder.Vector as C

type Equation = VU.Vector Double
type Matrix = V.Vector Equation

cLIMIT :: Double
cLIMIT = 0.001

cNONSOLVABLE:: T.Text
cNONSOLVABLE = "Attempt to solve a non-solvable equation system"

solve :: Matrix -> Either T.Text (VU.Vector Double)
solve mat = calcTriangle mat >>= backInsert

-- -------------------------------------------------------------------
-- Calculation
-- -------------------------------------------------------------------
calcTriangle :: Matrix -> Either T.Text (V.Vector Equation)
calcTriangle mat0 = V.unfoldrExactNM (V.length mat0) pivotStep mat0
  where
    pivotStep :: Matrix -> Either T.Text (Equation, Matrix)
    pivotStep mat =
      if abs negPivot < cLIMIT
          then Left cNONSOLVABLE
          else Right (pivotrow, V.map newRow newMat)

     where
          ixprow = V.maxIndexBy (\x y -> compare (abshead x) (abshead y)) mat
          abshead = abs . VU.head
          pivotrow = (V.!) mat ixprow
          newMat = V.imapMaybe ixFilter mat   -- This is faster than V.ifilter !!
            where
            ixFilter :: Int -> a -> Maybe a
            ixFilter ix v
                | ix == ixprow = Nothing
                | otherwise  = Just v
          -- Apply the pivot to a row
          newRow :: Equation -> Equation
          newRow row = VU.zipWith (+)
                     (applyPivot (VU.head row))
                     (VU.tail row)
          applyPivot :: Double -> Equation
          applyPivot hdRow = VU.map (hdRow / negPivot *) tailprow
          -- The next 2 values do not change between rows in applyPivot!
          tailprow = VU.tail pivotrow
          negPivot = negate $ VU.head pivotrow

backInsert :: V.Vector Equation -> Either T.Text (VU.Vector Double)
backInsert eqs = V.foldM' stepInsert VU.empty (V.reverse eqs)
  -- Here we miss Vector.foldrM, therefore we have to reverse the equations !!
  where
    stepInsert :: VU.Vector Double -> Equation -> Either T.Text (VU.Vector Double)
    stepInsert xs equat =
        let piv = VU.head equat
            as = (VU.tail . VU.init) equat
            s = VU.last equat - VU.sum (VU.zipWith (*) as xs)
        in  if abs piv < cLIMIT
            then Left cNONSOLVABLE
            else Right $ cons (s/piv) xs
    cons :: Double -> VU.Vector Double -> VU.Vector Double
    cons el vect = C.build builder
      where
        builder = B.singleton el <> B.vector vect

