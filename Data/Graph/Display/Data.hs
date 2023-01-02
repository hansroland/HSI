module Data.Graph.Display.Data where

import Data.Graph.Dag
import Data.Graph.HSI

-- Datatype for visibility of faces
data Visibility = Visible | Hidden
    deriving (Show, Eq)

type VisPolytope = Polytope Visibility          -- A Polytope used during drawing
type VisNode = Node Face Visibility
type VisDag = Dag Face Visibility

-- The style of an output line
data Style = Normal
           | Dotted

-- A line to be converted to a SVG Line
data Line = Line { lix1 :: !Int,
                lix2 :: !Int,
                liy1 :: !Int,
                liy2 :: !Int,
                liStyle :: !Style}

