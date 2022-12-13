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

import Validation (Validation (..), failure, isSuccess)
import Data.List.NonEmpty (NonEmpty(..))

import Text.Read(readMaybe)

import Data.Foldable
import Control.Monad.State.Strict
import System.Directory
import System.IO

import qualified Data.Vector.Unboxed as VU
-- import Data.Vector.Internal.Check (checkLength)
-- import Data.Graph.HSI (mkPyramid)

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
      "load"  -> uiLoadHss params
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
uiCd :: (MonadState UiState m, MonadIO m) => [String] -> m ()
uiCd toks = do
    dirValid <- whenS toks (validateLength "cd" 1) (validateDirExists)
    doIfValid dirValid (liftIO . setCurrentDirectory . head)


-- TODO Needs still a lot of improvments !!!!!
--      Validation of readability of numeric data
--      Same dimension for all hss entries etc etc
-- Read the halfspaces from a file
uiLoadHss :: [String] -> StateT UiState IO ()
uiLoadHss toks = do
    fnValid <- whenS toks (validateLength "cd" 1) (validateFileExists)
    when (isSuccess fnValid) $ load $ head toks
    informIfFailure fnValid
    pure ()
    where
      load :: String -> StateT UiState IO ()
      load path = do
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
    liftIO $ putStrLn "Unprocessed Halfspaces:"
    liftIO $ putStrLn "-----------------------"
    hslist <- gets uiHss
    if (null hslist)
        then liftIO $ putStrLn "(empty)"
        else liftIO $ mapM_ putStrLn $ map show hslist
    --
    liftIO $ putStrLn "\nPolytope"
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
    validateSize :: [String] -> Validate [Double]
    validateSize toks = validateReads toks <* (validateLength "size" 2 toks)
    fupd :: ([Double] -> DispParams -> DispParams)
    fupd lst = dpSetSize (P2 (head lst) (last lst))

-- Set the projection direction
uiPdir :: (MonadState UiState m, MonadIO m) => [String] -> m()
uiPdir inps = updateIfValid fupd $ validatePdir inps
  where
    validatePdir :: [String] -> Validate [Double]
    validatePdir toks = validateReads toks <* (validateLength "pdir" 3 toks)
    fupd :: ([Double] -> DispParams -> DispParams)
    fupd = dpSetPdir . VU.fromList

-- Validate halfspaces: Are there any halfspaces and have all the same dimension
-- validateHss

-- ---------------------------------------------------------------------------------
-- General validation functions
-- ---------------------------------------------------------------------------------
type Validate a = Validation (NonEmpty String) a

-- Function to run a validation in IO, when a first valiation succeeds!
whenS:: MonadIO m => a
                  -> (a -> Validate a)                -- a precondition for the IO valiation
                  -> (a -> IO (Validate a))           -- The IO validation
                 -> m (Validate a)
whenS as v1 v2 = do
    let val1 = v1 as
    if (isSuccess val1)
        then liftIO $ v2 as
        else pure val1

-- validate if a file existS
validateFileExists :: MonadIO m => [String] -> m (Validate [String])
validateFileExists strs = do
    let strDir = head strs
    bExits <- liftIO $ doesFileExist strDir
    if bExits
       then return $ Success strs
       else return $ failure $ "File `" <> strDir <> "` doesn't exist"

-- validate if a directory existS
validateDirExists :: MonadIO m => [String] -> m (Validate [String])
validateDirExists strs = do
    let strDir = head strs
    bExits <- liftIO $ doesDirectoryExist strDir
    if bExits
       then return $ Success strs
       else return $ failure $ "Directory `" <> strDir <> "` doesn't exist"

-- validate whether the input string can be read
validateRead :: Read a => String -> Validate  a
validateRead str =
  case readMaybe str of
    Just d  -> Success d
    Nothing -> failure $ str <> " is not numeric"

-- validate whether all elements of a list can be read
validateReads :: Read a => [String] -> Validate  [a]
validateReads = sequenceA . fmap validateRead

-- validate only one input token
-- Check for 2 as the command is included
-- checkLengthEq1 :: String -> [String] -> Validate Bool
-- checkLengthEq1  msg toks = trace ("CheckLength1:" ++ show toks ++ " " ++ show (length toks)) $ Success $ length toks == 2
-- TODO: Take the error message out of client code
 --   if
 --       then Success toks
 --       else Failure ([ "`" <> msg <> "` needs one parameter" ])

-- validate the length of the input tokens
validateLength :: String -> Int -> [a] -> Validate [a]
validateLength  msg len toks =
    if length toks == len
        then Success toks
        else failure $ msg <> " needs " <> show len <> " parameters"

--
updateIfValid :: (MonadState UiState m, MonadIO m) =>
      ([a] -> DispParams -> DispParams)
      -> Validate[a]
      -> m()
updateIfValid fupd (Success rslt) = do
      dparms <- gets uiDparms
      putDparm $ fupd rslt dparms
updateIfValid _ (Failure msgs)  =
      mapM_ (liftIO . putStrLn) msgs

doIfValid:: MonadIO m => Validate [String]
      -> ([String] -> IO ())
      -> m ()
doIfValid (Success parms) f = liftIO $ f parms
doIfValid (Failure msgs) _ = mapM_ (liftIO . putStrLn) msgs

informIfFailure :: MonadIO m => Validate [String] -> m()
informIfFailure (Failure msgs) = mapM_ (liftIO . putStrLn) msgs
informIfFailure _              = pure ()
