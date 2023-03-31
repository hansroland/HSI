{-# OPTIONS_GHC -Wno-orphans #-}

module Data.Graph.Display.Vect2 (
    P2,
    point2,
    maxXy,
    minXy,
    divBy,
    multWith,
    x, y)

where

import Prelude hiding (div)
import qualified Data.Vector.Unboxed as VU

-- Drawing point and drawng vectors
type P2 = VU.Vector Double

-- This should be defined in some simple linear algebra package
instance (Num a, VU.Unbox a) => Num (VU.Vector a) where
    (+)= VU.zipWith (+)
    (*) = VU.zipWith (*)
    negate = VU.map negate
    abs _    =  error "P2 dosen't support fromInteger"
    fromInteger _             = error "P2 dosen't support fromInteger"
    signum _                  = error "P2 dosen't support signum"

-- Constructor
point2 :: Double -> Double -> P2
point2 x1 x2 = VU.fromList [x1,x2]

divBy :: P2 -> Double -> P2
divBy v d = VU.map (/d) v

multWith :: Double -> P2 -> P2
multWith s = VU.map (s *)

-- Accessor function: x is the first coordinate
x :: P2  -> Double
x v = v VU.! 0

-- Accessor function: y is the second coordinate
y :: P2 -> Double
y v = v VU.! 1

-- Get the maximum of the x and y coordinates
-- ATTENTION: This is a non standard function, it does the maximum by coordinates!!
maxXy :: P2 -> P2 -> P2
maxXy = VU.zipWith max

-- Get the minimum of the x and y coordinates
-- ATTENTION: This is a non standard function, it does the minimum by coordinates!!
minXy :: P2 -> P2 -> P2
minXy = VU.zipWith min
