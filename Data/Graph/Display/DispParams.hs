{-# Language NamedFieldPuns #-}

module Data.Graph.Display.DispParams where

import Data.Graph.Display.Point2

import Data.Vector.Unboxed (Vector)
import qualified Data.Vector.Unboxed as VU


data DispParams = DispParams {
    dpSize      :: P2,                     -- Size (in html points) of the whole drawing
    dpPdir    :: Vector Double,            -- Projection direction
    dpFilepath :: FilePath                 -- The FilePath to write the HTML file
    -- dpJproj :: Int,
    -- dpAuge  :: [Double]                  -- Decide Vector or list ??
    } deriving (Show)

dpInit :: DispParams
dpInit = DispParams
    { dpSize = point2 400 300,
      dpPdir = VU.fromList [3,1,-1],
      dpFilepath = "HSI00.html"              -- TODO: Take name from polytope !!
    }

-- Set a new value to the dpSize field
dpSetSize :: P2 -> DispParams -> DispParams
dpSetSize point dparms = dparms{ dpSize = point }

dpSetPdir :: Vector Double -> DispParams -> DispParams
dpSetPdir pdir dparms = dparms { dpPdir = pdir }
