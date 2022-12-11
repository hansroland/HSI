{-# Language NamedFieldPuns #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}   -- used for mtl functions
{-# LANGUAGE OverloadedStrings #-}

module UI where

import Data.Graph.HSI

import Data.Graph.Display.DispParams
import Data.Graph.Display.Point2

import Data.Graph.HSI.Algorithm
import Data.Graph.HSI.Polytope
import Data.Graph.HSI.InitCube
import Data.Graph.Display.Display (display)

import Validation (Validation (..))

import Text.Read(readMaybe)

import Data.Foldable
import Control.Monad.State.Strict
import System.Directory
import System.IO

import qualified Data.Vector.Unboxed as VU
-- import Data.Vector.Internal.Check (checkLength)
-- import Data.Graph.HSI (mkPyramid)

-- import Debug.Trace

-- Application state of the UI
-- All the data maintained and updated during the UI program
data UiState = UiState {
    uiPoly :: HsiPolytope,
    uiDparms :: DispParams,
    uiHss     :: [Halfspace]
    }

-- Store a new poly into the UiState
putPoly :: MonadState UiState m => HsiPolytope -> m ()
putPoly newpoly = do
  uiState <- get
  put $ uiState {uiPoly = newpoly}

-- Store new Halfspace data
putHss :: MonadState UiState m => [Halfspace] -> m ()
putHss newhss = do
  uiState <- get
  put $ uiState {uiHss = newhss}

-- Store new DisplayParameters
putDparm :: MonadState UiState m => DispParams -> m ()
putDparm dparms = do
  uiState <- get
  put $ uiState {uiDparms = dparms}

-- Initialize the application state
uiInitState :: UiState
uiInitState = UiState { uiPoly = mkCube 3, uiHss = [], uiDparms = dpInit}


uiLoop :: StateT UiState IO ()
uiLoop = do
    strDir <- liftIO $ getCurrentDirectory
    liftIO $ putStr ("hsi: " <> strDir <> ": ")
    liftIO $ hFlush stdout
    line <- liftIO $ getLine
    let tokens = words line
    if null tokens
      then do
        liftIO $ putStrLn "No input received"
        uiLoop
       else do
        let cmd = head tokens
            params = tail tokens
        case cmd of
          "end" -> return ()
          _ -> do
              uiCommands cmd params
              uiLoop

-- Process the diffenent commands.
uiCommands :: String -> [String] -> StateT UiState IO ()
uiCommands cmd params = do
    case cmd of
      "input" -> uiReadHss params
      "clear" -> uiClear
      "hsi"   -> uiHsi
      "draw"  -> uiDisplay
      "size"  -> uiSize params
      "pdir"  -> uiPdir params
      "list"  -> uiList
      "cd"    -> uiCd params
      _       -> liftIO $ putStrLn ("incorrect input `" <> cmd <> "`")
      -- The `end`command is processed in the uiLoop Function above.

-- Change directory for reading files
uiCd :: MonadIO m => [String] -> m()
uiCd toks = do
  let valid = (validateLength "cd" 1 toks)
  doIfValid valid (setCurrentDirectory . head)

-- TODO Improve the whole validation mess !! (More Haskell less C#)
-- TODO do it only in IO and return Halfspace list
-- Read the halfspaces from a file
uiReadHss :: [String] -> StateT UiState IO ()
uiReadHss parms = do
    if null parms
      then do
        liftIO $ putStrLn "missing filename"
      else do
        let path = head parms
        bFileExists <- liftIO $ doesFileExist path
        if bFileExists
          then do
            liftIO $ putStrLn ("Reading file " <> path)
            contents <- liftIO $ readFile path
            let lns = lines contents
            liftIO $ mapM_ putStrLn lns
            let linToks = map words lns
                linValues :: [[Double]] = map (map read ) linToks
                hss = map hsFromList linValues
                strHss = map show hss
            liftIO $ mapM_ putStrLn $ strHss
            putHss hss
          else do
            liftIO $ putStrLn ("File " <> path <> " does not exist")

-- Run the HSI algorithm on the halfspaces into the halfspace store
uiHsi :: (MonadState UiState m, MonadIO m) => m()
uiHsi = do
  hslist <- gets uiHss
  poly0 <- gets uiPoly
  let rslt = foldlM hsiStep poly0 hslist
  case rslt of
    (Left msg)   -> liftIO $ putStrLn msg
    (Right poly) -> do
      -- let poly = polyShowPreorder poly0
      liftIO $ putStrLn $ show $ polyStats poly
      liftIO $ putStrLn $ show $ checkFormulaEuler poly
      -- liftIO $ putStrLn $ show $ getVertMap poly
      -- update state: remove halfspaces, add polytope
      putHss []
      putPoly poly

-- List the current polytope in the UI State
uiList :: (MonadState UiState m, MonadIO m) => m()
uiList = do
    poly <- gets uiPoly
    liftIO $ putStrLn $ show poly
    liftIO $ putStrLn $ show $ polyStats poly
    liftIO $ putStrLn $ show $ checkFormulaEuler poly


-- Reset the application state
-- Reset the polytope and the halfspace store
uiClear ::  (MonadState UiState m) => m()
uiClear = get >>= uiReset
  where
    uiReset :: (MonadState UiState m) => UiState -> m()
    uiReset uiState = put $ uiState{ uiPoly = (mkCube 3), uiHss = [] }

-- Display the current polytope with the current display parameters
uiDisplay :: (MonadState UiState m, MonadIO m) => m()
uiDisplay = do
    dparms <- gets uiDparms
    poly   <- gets uiPoly
    -- Transform a HsiPolytope into a VisPolytope
    liftIO $ display dparms $ polyHsi2Vis poly

-- Process the `size` input
uiSize :: (MonadState UiState m, MonadIO m) => [String] -> m()
uiSize inps = updateIfValid fupd $ validateSize inps
  where
    validateSize :: [String] -> Validation [String] [Double]
    validateSize toks = validateReads toks <* (validateLength "size" 2 toks)
    fupd :: ([Double] -> DispParams -> DispParams)
    fupd lst = dpSetSize (P2 (head lst) (last lst))

-- Set the projection direction
uiPdir :: (MonadState UiState m, MonadIO m) => [String] -> m()
uiPdir inps = updateIfValid fupd $ validatePdir inps
  where
    validatePdir :: [String] -> Validation [String] [Double]
    validatePdir toks = validateReads toks <* (validateLength "pdir" 3 toks)
    fupd :: ([Double] -> DispParams -> DispParams)
    fupd = dpSetPdir . VU.fromList

-- Validate halfspaces: Are there any halfspaces and have all the same dimension
-- validateHss

-- ---------------------------------------------------------------------------------
-- General validation functions
-- ---------------------------------------------------------------------------------

-- validate whether the input string can be read
validateRead :: Read a => String -> Validation [String] a
validateRead str =
  case readMaybe str of
    Just d  -> Success d
    Nothing -> Failure [str <> " is not numeric"]

-- validate whether all elements of a list can be read
validateReads :: Read a => [String] -> Validation [String] [a]
validateReads = sequenceA . fmap validateRead

-- validate the length of the input tokens
validateLength :: String -> Int -> [a] -> Validation [String] [a]
validateLength  msg len toks =
    if length toks == len
        then Success toks
        else Failure ([ msg <> " has wrong number of tokens" ])

--
updateIfValid :: (MonadState UiState m, MonadIO m) =>
      ([a] -> DispParams -> DispParams)
      -> Validation [String] [a]
      -> m()
updateIfValid fupd (Success rslt) = do
      dparms <- gets uiDparms
      putDparm $ fupd rslt dparms
updateIfValid _ (Failure msgs)  =
      mapM_ (liftIO . putStrLn) msgs

doIfValid:: MonadIO m => Validation [String] [String]
      -> ([String] -> IO())
      -> m ()
doIfValid (Success parms) f = liftIO $ f parms
doIfValid (Failure msgs) _ = mapM_ (liftIO . putStrLn) msgs
