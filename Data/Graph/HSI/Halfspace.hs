{-# Language GeneralisedNewtypeDeriving, DerivingStrategies #-}
{-# Language NamedFieldPuns #-}

-- {-# Language GeneralisedNewtypeDeriving, DeriveFunctor, DeriveFoldable, DeriveTraversable, DerivingStrategies #-}
module Data.Graph.HSI.Halfspace where

import Data.Vector.Unboxed (Vector)
import qualified Data.Vector.Unboxed as VU
import Data.EnumMap.Strict(EnumMap)
import qualified Data.EnumMap.Strict as Map

-- Data type to store a Halfspace
-- A halfspace ax + by + cz + d >= 0 is stored as Vector [a,b,c,d]
newtype Halfspace = Halfspace (Vector Double)
    deriving (Show)

-- Datatype to store the dimension of a geometric object
newtype Dim = Dim Int
    deriving (Eq, Ord, Num, Enum)
    deriving newtype (Show)

-- Datatype to store the keys / indexes of Halfspaces in an IntMap.
newtype HsKey = HsKey Int
    deriving (Eq, Ord, Num)
    deriving newtype (Read, Show, Enum, Real, Integral)

type HsMap = EnumMap HsKey Halfspace

-- Create a Halfspace from a list of doubles.
-- Checking the length of the list is the task of the input program
-- Note: Here we assume, that the length of the list has been checked!
hsFromList :: [Double] -> Halfspace
hsFromList cs = Halfspace $ VU.fromList cs

-- Get the equation vector out of a Halfspace.
hsEquation :: Halfspace -> Vector Double
hsEquation (Halfspace vs) = vs

-- Map a list of HsKeys to something
hsMap :: (Halfspace -> b) -> HsMap -> [HsKey] -> [b]
hsMap hsFun hsmap keys = (hsFun . (hsmap Map.!)) <$> keys

toVector :: Halfspace -> Vector Double
toVector (Halfspace vs) = vs

-- Normalize the vector of a Halfspace
normalize :: Halfspace -> Halfspace
normalize (Halfspace vs) =
    let ivs = VU.init vs
        d = sqrt (VU.sum $ VU.zipWith (*) ivs ivs)
        norm = VU.map (/ d) ivs
    in Halfspace $ VU.snoc norm $ VU.last vs

-- Distance of a Point from a halfspace
distance :: Halfspace -> Vector Double -> Double
distance hs point =
    let hsv = toVector $ normalize hs
        dotp = VU.sum $ VU.zipWith (*) (VU.init hsv) point
    in  dotp - VU.last point

-- Dimension of a halfspace
hsDim :: Halfspace -> Dim
hsDim (Halfspace v) = Dim (VU.length v) -1
