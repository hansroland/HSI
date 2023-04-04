{-# Language GeneralisedNewtypeDeriving, DerivingStrategies #-}
{-# Language NamedFieldPuns #-}
{-# OPTIONS_GHC -fno-warn-overlapping-patterns #-}

module Data.Graph.HSI.RelPos
(RelPos,
relPosP,  relPos0, relPosM,
relPosP0, relPosM0,
relPosMP, relPosM0P
)
where

import Data.Word ( Word8 )
import Data.Bits ( Bits( (.|.) ) )

-- The location of a face relative to a Halfspace
-- The relative position for a vertex is either H+, H-, H0.
-- The relative position of a non-vertex is the combination of the
-- relative positions of it's sub-faces.
newtype RelPos = RelPos Word8
     deriving Eq

instance Semigroup RelPos where
     (RelPos x) <> (RelPos y) = RelPos (x .|. y)

instance Monoid RelPos where
     mempty = RelPos 0x00

instance Show RelPos where
    show x
        | x == mempty = "empty"
        | x == relPosP = "relPosP"
        | x == relPos0 = "relPos0"
        | x == relPosM = "relPosM"
        | x == relPosP0 = "relPosP0"
        | x == relPosM0 = "relPosM0"
        | x == relPosMP = "relPosMP"
        | x == relPosM0P = "relPosM0P"
    show (RelPos v) = "RelPos is wrong " ++ show v

-- Possible position of a face relative to the Halfspace
relPosP :: RelPos
relPosP = RelPos 0x10

relPos0 :: RelPos
relPos0 = RelPos 0x20

relPosM :: RelPos
relPosM = RelPos 0x40

relPosP0 :: RelPos
relPosP0 = RelPos 0x30

relPosM0 :: RelPos
relPosM0 = RelPos 0x60

relPosMP :: RelPos
relPosMP = RelPos 0x50

relPosM0P :: RelPos
relPosM0P = RelPos 0x70
