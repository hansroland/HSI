{-# LANGUAGE StrictData #-}

module Data.Graph.Display.DispParams where

import Data.Graph.Display.Vect2

import Data.Vector.Unboxed (Vector)
import qualified Data.Vector.Unboxed as VU


data DispParams = DispParams {
    dpSize      :: !P2,                     -- Size (in html points) of the whole drawing
    dpPdir      :: Vector Double,          -- Projection direction
    dpZaxis     :: Vector Double,          -- The z-axis for correct orientation
    dpIndir     :: !FilePath,               -- Filepath to the input directory
    dpOutdir    :: !FilePath,               -- Filepath to the outpur directory
    dpFilename  :: !FilePath,               -- The FilePath to write the HTML file
    dpHidden    :: !Bool,                   -- True = Show hidden lines
    dpAuto      :: !Bool                    -- True = calculate automatically
    -- dpJproj :: Int,
    -- dpAuge  :: Vector Double
    } deriving (Show)

dpInit :: DispParams
dpInit = DispParams
    { dpSize = point2 400 300,
      dpPdir = VU.fromList [3,1,-1],
      dpIndir = ".",
      dpOutdir = "./",
      dpFilename = "poly",
      dpZaxis = VU.fromList [0,0,1],
      dpHidden = True,
      dpAuto = True
    }

-- Set a new value to the dpSize field
dpSetSize :: P2 -> DispParams -> DispParams
dpSetSize point dparms = dparms{ dpSize = point }

dpSetPdir :: Vector Double -> DispParams -> DispParams
dpSetPdir pdir dparms = dparms { dpPdir = pdir }

dpSetHidden :: Bool -> DispParams -> DispParams
dpSetHidden b dparms = dparms { dpHidden = b}

dpSetIndir :: FilePath -> DispParams -> DispParams
dpSetIndir dir dparms = dparms { dpIndir = dir}

dpSetOutdir :: FilePath -> DispParams -> DispParams
dpSetOutdir dir dparms = dparms { dpOutdir = dir}

dpSetFilename :: FilePath -> DispParams -> DispParams
dpSetFilename name dparms = dparms {dpFilename = name}

dpSetAuto :: Bool ->  DispParams -> DispParams
dpSetAuto b dparms = dparms { dpAuto = b}
