-- A simple main program to calculate and draw polytopes
-- from halfspaces
module Main where

import UI ( uiLoop, uiInitState )
import Control.Monad.State.Strict ( execStateT )
import System.Console.Haskeline

main :: IO ()
main = do
      _ <- execStateT (runInputT defaultSettings uiLoop) uiInitState
      return ()
