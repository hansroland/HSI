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

import qualified Data.Text      as T
import qualified Data.Text.IO   as T
import qualified Data.Text.Read as T
import qualified Data.Vector    as V
import qualified Data.Vector.Unboxed    as VU
import qualified Data.Vector.Generic    as VG
import Data.List(partition)
import Data.Char (isSpace)
import Data.Maybe (fromJust, isNothing)

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

-- Loop over input till an `exit` command
uiLoop :: StateT UiState IO ()
uiLoop = do
    strDir <- liftIO $ getCurrentDirectory
    liftIO $ putStr ("hsi: " <> strDir <> ": ")
    liftIO $ hFlush stdout
    line <- liftIO $ T.getLine
    let tokens = T.words line
    if null tokens
      then do
        liftIO $ T.putStrLn "No input received"
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
uiCommands :: T.Text -> [T.Text] -> StateT UiState IO ()
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
      _       -> liftIO $ T.putStrLn ("incorrect input `" <> cmd <> "`")
      -- The `end`command is processed in the uiLoop Function above.

-- Change directory for reading files
-- uiCd :: (MonadState UiState m, MonadIO m) => [T.Text] -> m ()
uiCd :: [T.Text] -> StateT UiState IO ()
uiCd parms = (liftIO $ cdVerify parms) >>= report
  where
    cdVerify :: [T.Text] -> IO (Either T.Text FilePath)
    cdVerify toks =
      runExceptT $ verifyLength1 "cd" (T.unpack <$>toks) >>= verifyDirExists
    report :: (MonadState UiState m, MonadIO m) => Either T.Text  FilePath -> m ()
    report (Right fp) = liftIO $ setCurrentDirectory fp
    report (Left e)   = liftIO $ T.putStrLn  e

-- TODO Needs still a lot of improvments !!!!!
--      Validation of readability of numeric data
--      Same dimension for all hss entries etc etc
-- Read the halfspaces from a file
uiLoadHss :: [T.Text] -> StateT UiState IO ()
uiLoadHss parms = (liftIO $ load parms) >>= report
  where
    load :: [T.Text] -> IO (Either T.Text [VU.Vector Double])
    load toks = runExceptT $
        verifyLength1 "load" toks >>=
          verifyFileExists >>=
          verifyLoad
    report :: (MonadState UiState m, MonadIO m) => Either T.Text [VU.Vector Double] -> m ()
    report (Right vecs) = do
      let hss = Halfspace <$> vecs
      liftIO $ mapM_ putStrLn $ map show hss
      putHss hss
    report (Left e) = liftIO $ T.putStrLn e
    verifyLoad :: FilePath -> ExceptT T.Text IO [VU.Vector Double]
    verifyLoad path = do
      liftIO $ putStrLn ("Reading file " <> path)
      contents <- liftIO $ T.readFile path
      verifyVectors $ parseLine <$> T.lines contents
    parseLine :: T.Text -> V.Vector (Maybe Double)     -- The outer Maybe is for unfoldr
    parseLine l = V.unfoldr step (skipBlanks l)        -- The inner for error handling!
      where
        step :: T.Text -> Maybe (Maybe Double, T.Text)
        step "" = Nothing
        step !s = case T.double s of
            Left _ -> Just $ (Nothing , "")
            Right (!d, !t) -> Just $ (Just d, skipBlanks t)
        skipBlanks :: T.Text -> T.Text
        skipBlanks = T.dropWhile (isSpace)
    verifyVectors :: [V.Vector (Maybe Double)] -> ExceptT T.Text IO [VU.Vector Double]
    verifyVectors vecs = do
        let toUnboxed :: V.Vector (Maybe Double) -> VU.Vector Double
            toUnboxed = VG.convert . V.map fromJust
            hasNothing :: V.Vector (Maybe Double) -> Bool
            hasNothing v = not $ V.null $  V.filter isNothing v
            errs  = filter (hasNothing . snd) $ zip ([1..]::[Int]) vecs
        if null errs
              then return $ toUnboxed <$> vecs
              else throwError $ T.pack ("Input errors in lines " ++ show (map fst errs))

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
uiSize :: (MonadState UiState m, MonadIO m) => [T.Text] -> m()
uiSize params = (liftIO $ verifySize params) >>= report
  where
    verifySize :: [T.Text] -> IO (Either T.Text (VU.Vector Double))
    verifySize toks =
       runExceptT $ (verifyLength "size" 2 toks) >>= verifyNumTokens
    report :: (MonadState UiState m, MonadIO m) => Either T.Text (VU.Vector Double) -> m ()
    report (Right vec) = do
        dparms <- gets uiDparms
        putDparm $ fupd vec dparms
      where
        fupd :: VU.Vector Double -> DispParams -> DispParams
        fupd v = dpSetSize (P2 (VU.head v) (VU.last v))
    report (Left e) = liftIO $ T.putStrLn e


-- Set the projection direction
uiPdir :: (MonadState UiState m, MonadIO m) => [T.Text] -> m()
uiPdir params = (liftIO $ verifyPdir params) >>= report
  where
    verifyPdir :: [T.Text] -> IO (Either T.Text (VU.Vector Double))
    verifyPdir toks =
        runExceptT $ (verifyLength "size" 2 toks) >>= verifyNumTokens
    report :: (MonadState UiState m, MonadIO m) => Either T.Text (VU.Vector Double) -> m ()
    report (Right vec) = do
        dparms <- gets uiDparms
        putDparm $ dpSetPdir vec dparms
    report (Left e) = liftIO $ T.putStrLn e


-- TODO: Validate halfspaces: Are there any halfspaces and have all the same dimension
-- validateHss

-- ---------------------------------------------------------------------------------
-- General validation functions
-- ---------------------------------------------------------------------------------
-- Verify whether a file exists
verifyFileExists :: (MonadIO m, MonadError T.Text m) => T.Text -> m FilePath
verifyFileExists fn = do
    bExits <- liftIO $ doesFileExist $ T.unpack fn
    if bExits
       then return $ T.unpack fn
       else throwError $ "File `" <> fn <> "` doesn't exist"

-- Verify whether a directory exists
verifyDirExists :: (MonadIO m, MonadError T.Text m) => FilePath -> m FilePath
verifyDirExists dn = do
    bExits <- liftIO $ doesDirectoryExist dn
    if bExits
       then return dn
       else throwError $ "Directory `" <> T.pack  dn <> "` doesn't exist"


verifyNumTokens :: [T.Text] -> ExceptT T.Text IO  (VU.Vector Double)
verifyNumTokens toks = do
    let part = partition isNothing $ parse <$> toks
    if null (fst part)
         then return $ VU.fromList $ fromJust <$> snd part
         else throwError $ "Not all tokens are numeric"
  where
    parse :: T.Text -> Maybe Double
    parse tok = case T.double tok of
        Left _        -> Nothing
        Right (!d, _) -> Just d

verifyLength1 :: T.Text -> [a] -> ExceptT T.Text IO  (a)
verifyLength1  msg toks =
    if length toks == 1
        then return $ head toks
        else throwError $ msg <> " expects 1 parameter"

verifyLength :: T.Text -> Int -> [a] -> ExceptT T.Text  IO [a]
verifyLength  msg len toks =
    if length toks == len
        then return toks
        else throwError $ msg <> T.pack (" expects " <> show len <> " parameters")
