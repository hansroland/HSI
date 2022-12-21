module Data.Graph.HSI.Utils where

import qualified Data.Vector as V
import qualified Data.Vector.Unboxed as VU
import qualified Data.Vector.Generic as VG

-- Rounding
factor :: Double
factor = 100000

-- round a double
roundDouble :: Double -> Double
roundDouble x = (fromInteger (round (factor * x))) / factor

-- round a vector
roundVector :: V.Vector Double -> V.Vector Double
roundVector vec = roundDouble <$> vec

-- scalarproduct of 2 vectors
sp :: VU.Vector Double -> VU.Vector Double -> Double
sp = (VU.sum .) . VU.zipWith (*)
-- TODO: Use this function as a central scalarprocuct function !!!!! search on zipWith

-- Normalize a vector
normalize :: VU.Vector Double -> VU.Vector Double
normalize vs =
    let d = sqrt $ sp vs vs
    in  VU.map (/ d) vs

-- Conversions from Boxed to Unboxed vectors and vice versa
-- Should be deleted, when we have a matirx with unboxed elements
asUnboxed :: V.Vector Double -> VU.Vector Double
asUnboxed boxed = VG.convert boxed

asBoxed :: VU.Vector Double -> V.Vector Double
asBoxed unboxed = VG.convert unboxed
