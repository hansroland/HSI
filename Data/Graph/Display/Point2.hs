module Data.Graph.Display.Point2 (
    P2(..),
    point2,
    max_xy,
    min_xy,
    divBy,
    multWith,
    x, y)

where

import Prelude hiding (div)

data P2 = P2 Double Double
   deriving (Show)

instance Num P2 where
    (+) (P2 x1 y1) (P2 x2 y2) = P2 (x1 + x2) (y1 + y2)
    (*) (P2 x1 y1) (P2 x2 y2) = P2 (x1 * x2) (y1 * y2)
    negate (P2 x1 y1)           = P2 (negate x1) (negate y1)
    abs (P2 x1 y1)              = P2 (abs x1) (abs y1)
    fromInteger _             = error "P2 dosen't support fromInteger"
    signum _                  = error "P2 dosen't support signum"

-- Constructor
point2 :: Double -> Double -> P2
point2 x1 y1 = P2 x1 y1

divBy :: P2 -> Double -> P2
divBy (P2 x1 y1) d = P2 (x1/d) (y1/d)

multWith :: Double -> P2 -> P2
multWith s (P2 x1 y1)  = P2 (x1*s) (y1*s)

-- Accessor functions

x :: P2  -> Double
x (P2 x1 _) = x1

y :: P2 -> Double
y (P2 _ y1) = y1

-- Get the maximum of the x and y coordinates
-- ATTENTION: This is a non standard function, it does the maximum by coordinates!!
max_xy :: P2 -> P2 -> P2
max_xy (P2 x1 y1) (P2 x2 y2) = P2 (max x1 x2) (max y1 y2)

-- Get the minimum of the x and y coordinates
-- ATTENTION: This is a non standard function, it does the maximum by coordinates!!
min_xy :: P2 -> P2 -> P2
min_xy (P2 x1 y1) (P2 x2 y2) = P2 (min x1 x2) (min y1 y2)
