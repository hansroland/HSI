{-# GHC_OPTIONS -fno-warn-orphans #-}

module Data.Graph.Display.Vect2 (
    P2,
    point2,
    max_xy,
    min_xy,
    divBy,
    multWith,
    x, y)

where

import Prelude hiding (div)
import qualified Data.Vector.Unboxed as VU

type P2 = VU.Vector Double

instance (Num a, VU.Unbox a) => Num (VU.Vector a) where
    (+) v1 v2 = VU.zipWith (+) v1 v2
    (*) v1 v2 = VU.zipWith (*) v1 v2
    negate v = VU.map negate v
    abs _    =  error "P2 dosen't support fromInteger"
    fromInteger _             = error "P2 dosen't support fromInteger"
    signum _                  = error "P2 dosen't support signum"

-- Constructor
point2 :: Double -> Double -> P2
point2 x1 x2 = VU.fromList [x1,x2]

divBy :: P2 -> Double -> P2
divBy v d = VU.map (/d) v

multWith :: Double -> P2 -> P2
multWith s v = VU.map (s *) v

-- Accessor functions
x :: P2  -> Double
x v = v VU.! 0

y :: P2 -> Double
y v = v VU.! 1

-- Get the maximum of the x and y coordinates
-- ATTENTION: This is a non standard function, it does the maximum by coordinates!!
max_xy :: P2 -> P2 -> P2
max_xy v1 v2 = VU.zipWith (max) v1 v2

-- Get the minimum of the x and y coordinates
-- ATTENTION: This is a non standard function, it does the minimum by coordinates!!
min_xy :: P2 -> P2 -> P2
min_xy v1 v2 = VU.zipWith (min) v1 v2
{-# OPTIONS_GHC -Wno-orphans #-}

module Data.Graph.Display.Vect2 (
    P2,
    point2,
    max_xy,
    min_xy,
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
    (+) v1 v2 = VU.zipWith (+) v1 v2
    (*) v1 v2 = VU.zipWith (*) v1 v2
    negate v = VU.map negate v
    abs _    =  error "P2 dosen't support fromInteger"
    fromInteger _             = error "P2 dosen't support fromInteger"
    signum _                  = error "P2 dosen't support signum"

-- Constructor
point2 :: Double -> Double -> P2
point2 x1 x2 = VU.fromList [x1,x2]

divBy :: P2 -> Double -> P2
divBy v d = VU.map (/d) v

multWith :: Double -> P2 -> P2
multWith s v = VU.map (s *) v

-- Accessor function: x is the first coordinate
x :: P2  -> Double
x v = v VU.! 0

-- Accessor function: y is the second coordinate
y :: P2 -> Double
y v = v VU.! 1

-- Get the maximum of the x and y coordinates
-- ATTENTION: This is a non standard function, it does the maximum by coordinates!!
max_xy :: P2 -> P2 -> P2
max_xy v1 v2 = VU.zipWith (max) v1 v2

-- Get the minimum of the x and y coordinates
-- ATTENTION: This is a non standard function, it does the minimum by coordinates!!
min_xy :: P2 -> P2 -> P2
min_xy v1 v2 = VU.zipWith (min) v1 v2
