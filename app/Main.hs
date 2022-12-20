-- A simple main program to calculate and draw polytopes
-- from halfspaces
module Main where

import UI ( uiLoop, uiInitState )
import Control.Monad.State.Strict ( execStateT )

main :: IO ()
main = do
   _ <- execStateT uiLoop uiInitState
   return ()
