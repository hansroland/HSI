{-# Language GeneralisedNewtypeDeriving, DerivingStrategies #-}
{-# Language NamedFieldPuns #-}
{-# OPTIONS_GHC -fno-warn-overlapping-patterns #-}

module Data.Graph.HSI.RelHsPos where

import Data.Word ( Word8 )
import Data.Bits ( Bits((.&.), (.|.)) )

-- The location of a point or edge relative to a Halfspace
newtype RelHsPos = RelHsPos Word8
     deriving Eq

instance Semigroup RelHsPos where
     (RelHsPos x) <> (RelHsPos y) = RelHsPos (x .|. y)

instance Monoid RelHsPos where
     mempty = RelHsPos 0x00

instance Show RelHsPos where
    show x
        | x == mempty = "empty"
        | x == inside = "inside"
        | x == onEdge = "onEdge"
        | x == outside = "outside"
        | x == insideEdge = "insideEdge"
        | x == outsideEdge = "outsideEdge"
        | x == bothside = "bothside"
        | x == allside = "allside"
    show (RelHsPos v) = "RelHsPos is wrong " ++ show v
-- Possible position of a face relative to the Halfspace
inside :: RelHsPos
inside = RelHsPos 0x10

onEdge :: RelHsPos
onEdge = RelHsPos 0x20

outside :: RelHsPos
outside = RelHsPos 0x40

insideEdge :: RelHsPos
insideEdge = RelHsPos 0x30

outsideEdge :: RelHsPos
outsideEdge = RelHsPos 0x60

bothside :: RelHsPos
bothside = RelHsPos 0x50

allside :: RelHsPos
allside = RelHsPos 0x70

wipeOutOnEdge :: RelHsPos -> RelHsPos
wipeOutOnEdge (RelHsPos x) = RelHsPos ( x .&. 0x50)

isBothside :: RelHsPos -> Bool
isBothside  = (== bothside)

isAllside :: RelHsPos -> Bool
isAllside = (== allside)

isOnEdge :: RelHsPos -> Bool
isOnEdge = (== onEdge)
