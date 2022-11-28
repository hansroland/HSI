module Data.Graph.Display.Data where

data Style = Normal
           | Dotted

data Line = Line { lix1 :: Int,
                lix2 :: Int,
                liy1 :: Int,
                liy2 :: Int,
                liStyle :: Style}

