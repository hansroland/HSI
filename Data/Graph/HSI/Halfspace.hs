{-# Language GeneralisedNewtypeDeriving, DerivingStrategies #-}
{-# Language NamedFieldPuns #-}

-- {-# Language GeneralisedNewtypeDeriving, DeriveFunctor, DeriveFoldable, DeriveTraversable, DerivingStrategies #-}
module Data.Graph.HSI.Halfspace where

import Data.Vector.Unboxed (Vector)
import qualified Data.Vector.Unboxed as VU
import Data.EnumMap.Strict(EnumMap)
import qualified Data.EnumMap.Strict as Map
import Data.Graph.HSI.Utils

-- Data type to store a Halfspace
-- A halfspace ax + by + cz + d >= 0 is stored as Halfspace {hsV =[a,b,c], hsD = d}
data Halfspace = Halfspace {hsV :: !(Vector Double), hsD :: !Double}
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

mkHs :: Vector Double -> Double -> Halfspace
mkHs v d = Halfspace {hsV = v, hsD= d}

-- Create a Halfspace from a list of doubles.
-- Checking the length of the list is the task of the input program
-- So, here we assume, that the length of the list has been checked!
hsFromList :: [Double] -> Halfspace
hsFromList cs = Halfspace {hsV = VU.fromList (init cs), hsD = last cs}

-- Create a Halfspace from a vector of Doubles
hsFromVector :: Vector Double -> Halfspace
hsFromVector v = Halfspace {hsV = VU.init v, hsD = VU.last v}

-- Get the equation vector out of a Halfspace.
hsEquation :: Halfspace -> Vector Double
hsEquation hs = VU.snoc (hsV hs) (hsD hs)

-- Map a list of HsKeys to something
hsMap :: (Halfspace -> b) -> HsMap -> [HsKey] -> [b]
hsMap hsFun hsmap keys = (hsFun . (hsmap Map.!)) <$> keys

-- Normalize the vector of a Halfspace
hsNormalize :: Halfspace -> Halfspace
hsNormalize hs@Halfspace{ hsV } = hs {hsV = normalize hsV}

-- Distance of a Point from a halfspace
distance :: Halfspace -> Vector Double -> Double
distance hs point =
    let nv = normalize $ hsV hs
    in  sp nv point - hsD hs

-- Dimension of a halfspace
hsDim :: Halfspace -> Dim
hsDim hs = Dim $ VU.length $ hsV hs
