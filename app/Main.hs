module Main where

import UI

import Data.Graph.HSI
import Control.Monad.State.Strict

main :: IO ()
main = do
   _ <- execStateT uiLoop uiInitState
   return ()

cut_base :: [Halfspace]
cut_base = hsFromList <$> [[0,0,1, 50] ]
cut_top :: [Halfspace]
cut_top =  hsFromList <$> [[0,0,-1, -50] ]


hslist_simple1 :: [Halfspace]
hslist_simple1 = hsFromList <$> [
    [0,0,1, -10],
    [0,0,-1, -10],
    [0,1,0, -10],
    [0,-1,0, -10]
    ]


hslist_check :: [Halfspace]
hslist_check =  hsFromList <$> [[0,0,1, -100] ]


hslist_okta :: [Halfspace]
hslist_okta = hsFromList <$> [
  [ 1, 1, 1,   -1],
  [-1, 1, 1,   -1],
  [ 1,-1, 1,   -1],
  [-1,-1, 1,   -1],
  [ 1, 1,-1,   -1],
  [ 1,-1,-1,   -1],
  [-1, 1,-1,   -1],
  [-1,-1,-1,   -1]
  ]


hslist_cube :: [Halfspace]
hslist_cube = hsFromList <$> [
    [ 1, 0, 0,   -1],
    [-1, 0, 0,   -1],
    [ 0, 1, 0,   -1],
    [ 0,-1, 0,   -1],
    [ 0, 0, 1,   -1],
    [ 0, 0,-1,   -1]
   ]

hslist_tetra :: [Halfspace]
hslist_tetra = hsFromList <$> [
    [ 1,  1,  1,   -1],
    [-1, -1,  1,   -1],
    [-1,  1, -1,   -1],
    [ 1, -1, -1,   -1]
  ]
