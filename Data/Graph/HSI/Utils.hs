module Data.Graph.HSI.Utils where

import qualified Data.Vector.Unboxed as VU

-- Rounding
factor :: Double
factor = 100000

-- round a double
roundDouble :: Double -> Double
roundDouble x = (fromInteger (round (factor * x))) / factor

-- round a vector
roundVector :: VU.Vector Double -> VU.Vector Double
roundVector vec = VU.map roundDouble vec

-- scalarproduct of 2 vectors
sp :: VU.Vector Double -> VU.Vector Double -> Double
sp = (VU.sum .) . VU.zipWith (*)

-- Normalize a vector
normalize :: VU.Vector Double -> VU.Vector Double
normalize vs =
    let d = sqrt $ sp vs vs
    in  VU.map (/ d) vs
