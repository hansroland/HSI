{-# Language NamedFieldPuns #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}   -- used for mtl functions
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE BangPatterns #-}

module UI where

import Data.Graph.HSI

import Data.Graph.Display.DispParams
import Data.Graph.Display.Point2

import Data.Graph.HSI.Algorithm
import Data.Graph.HSI.Polytope
import Data.Graph.HSI.InitCube
import Data.Graph.Display.Display (display)

import Validation (Validation (..), failure)
import Data.List.NonEmpty (NonEmpty(..))

import qualified Data.Text      as T
import qualified Data.Text.IO   as T
import qualified Data.Text.Read as T
import qualified Data.Vector    as V
import qualified Data.Vector.Unboxed    as VU
import qualified Data.Vector.Generic    as VG
import Data.Char (isSpace)
import Data.Maybe (fromJust, isNothing)

import Text.Read(readMaybe)
import Data.Foldable
import Control.Monad.State.Strict
import System.Directory
import System.IO
import Control.Monad.Except

-- Application state of the UI
-- All the data maintained and updated during the UI program
data UiState = UiState {
    uiPoly   :: HsiPolytope,
    uiDparms :: DispParams,
    uiHss    :: [Halfspace]
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
          "exit" -> return ()
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
      "red"   -> uiRed
      _       -> liftIO $ putStrLn ("incorrect input `" <> cmd <> "`")
      -- The `end`command is processed in the uiLoop Function above.

-- Change directory for reading files
-- uiCd :: (MonadState UiState m, MonadIO m) => [String] -> m ()
uiCd :: [String] -> StateT UiState IO ()
uiCd parms = (liftIO $ cdVerify parms) >>= report
  where
    cdVerify :: [FilePath] -> IO (Either String FilePath)
    cdVerify toks =
      runExceptT $ verifyLength1 "cd" toks >>= verifyDirExists
    report :: (MonadState UiState m, MonadIO m) => Either String FilePath -> m ()
    report (Right fp) = liftIO $ setCurrentDirectory fp
    report (Left e)   = liftIO $ putStrLn $ "*FAILURE: " <> e

-- TODO Needs still a lot of improvments !!!!!
--      Validation of readability of numeric data
--      Same dimension for all hss entries etc etc
-- Read the halfspaces from a file
uiLoadHss :: [String] -> StateT UiState IO ()
uiLoadHss parms = (liftIO $ load parms) >>= report
  where
    load :: [FilePath] -> IO (Either String [Halfspace])
    load toks = runExceptT $
        verifyLength1 "load" toks >>=
          verifyFileExists >>=
          verifyLoad
    report :: (MonadState UiState m, MonadIO m) => Either String [Halfspace] -> m ()
    report (Right hss) = do
      liftIO $ mapM_ putStrLn $ map show hss
      putHss hss
    report (Left e) = liftIO $ putStrLn $ "*FAILURE: " <> e
    verifyLoad :: String -> ExceptT String IO [Halfspace]
    verifyLoad path = do
      liftIO $ putStrLn ("Reading file " <> path)
      contents <- liftIO $ T.readFile path
      let vecs = parse <$> T.lines contents
      verifyVecs vecs
    parse :: T.Text -> V.Vector (Maybe Double)   -- The outer Maybe is for unfoldr
    parse l = V.unfoldr step (skipBlanks l)      -- The inner for error handling!
      where
        step :: T.Text -> Maybe (Maybe Double, T.Text)
        step "" = Nothing
        step !s = case T.double s of
            Left _ -> Just $ (Nothing , "")
            Right (!d, !t) -> Just $ (Just d, skipBlanks t)
        skipBlanks :: T.Text -> T.Text
        skipBlanks = T.dropWhile (isSpace)


    verifyVecs :: [V.Vector (Maybe Double)] -> ExceptT String IO [Halfspace]
    verifyVecs vecs =
      let toUnboxed :: V.Vector (Maybe Double) -> VU.Vector Double
          toUnboxed = VG.convert . V.map fromJust
          hasNothing :: V.Vector (Maybe Double) -> Bool
          hasNothing v = not $ V.null $  V.filter isNothing v
          errs  = filter (hasNothing . snd) $ zip ([1..]::[Int]) vecs
          in if null errs
                then return $ (Halfspace . toUnboxed) <$> vecs
                else throwError ("Input errors in lines " ++ show (map fst errs))


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

uiRed :: (MonadState UiState m) => m()
uiRed = do
    poly <- gets uiPoly
    putPoly $ hsiRed poly

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

-- TODO: Validate halfspaces: Are there any halfspaces and have all the same dimension
-- validateHss

-- ---------------------------------------------------------------------------------
-- General validation functions
-- ---------------------------------------------------------------------------------
type Validate a = Validation (NonEmpty String) a

-- Verify whether a file exists
verifyFileExists :: (MonadIO m, MonadError String m) => String -> m FilePath
verifyFileExists fn = do
    bExits <- liftIO $ doesFileExist fn
    if bExits
       then return fn
       else throwError $ "File `" <> fn <> "` doesn't exist"

-- Verify whether a directory exists
verifyDirExists :: (MonadIO m, MonadError String m) => String -> m FilePath
verifyDirExists dn = do
    bExits <- liftIO $ doesDirectoryExist dn
    if bExits
       then return dn
       else throwError $ "Directory `" <> dn <> "` doesn't exist"


-- validate whether the input string can be read
validateRead :: Read a => String -> Validate  a
validateRead str =
  case readMaybe str of
    Just d  -> Success d
    Nothing -> failure $ str <> " is not numeric"



-- validate whether all elements of a list can be read
validateReads :: Read a => [String] -> Validate  [a]
validateReads = sequenceA . fmap validateRead

-- validate the length of the input tokens
validateLength :: String -> Int -> [a] -> Validate [a]
validateLength  msg len toks =
    if length toks == len
        then Success toks
        else failure $ msg <> " needs " <> show len <> " parameters"

verifyLength1 :: String -> [a] -> ExceptT String IO  (a)
verifyLength1  msg toks =
    if length toks == 1
        then return $ head toks
        else throwError $ msg <> " expects exact 1 parameter"

verifyLength :: String -> Int -> [a] -> ExceptT String IO [a]
verifyLength  msg len toks =
    if length toks == len
        then return toks
        else throwError $ msg <> " needs " <> show len <> " parameters"


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
