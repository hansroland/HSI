{-# Language NamedFieldPuns #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}   -- used for mtl functions
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE BangPatterns #-}

module UI where

import Data.Graph.HSI

import Data.Graph.Display.DispParams
import Data.Graph.Display.Vect2
import Data.Graph.HSI.InitCube
import Data.Graph.Display.Display (display, polyHsi2Vis)

import           Data.Text(Text)
import qualified Data.Text              as T
import qualified Data.Text.IO           as T
import qualified Data.Text.Read         as T
import qualified Data.Vector            as V
import qualified Data.Vector.Unboxed    as VU
import qualified Data.Vector.Generic    as VG
import Data.List(partition, nub)
import Data.Char (isSpace)
import Data.Maybe (fromJust, isNothing)
import System.FilePath

import Control.Monad.State.Strict
import System.Directory
import Control.Monad.Except
import Control.Error (handleExceptT)
import Control.Monad.Catch
import System.Console.Haskeline hiding (display)

-- Application state of the UI
-- All the data maintained and updated during the UI program
data UiState = UiState {
    uiPoly   :: Maybe HsiPolytope,
    uiDparms :: DispParams,
    uiHss    :: [Halfspace]
    }

type UiMonad a = StateT UiState IO a

-- Store a new poly into the UiState
putPoly :: MonadState UiState m => Maybe HsiPolytope -> m ()
putPoly mbNewpoly = do
  uiState <- get
  put $ uiState {uiPoly = mbNewpoly}

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
uiInitState = UiState { uiPoly = Nothing, uiHss = [], uiDparms = dpInit}

-- Loop over input till an `exit` command
uiLoop :: InputT (StateT UiState IO) ()
uiLoop = do
    strDir <- liftIO $ getCurrentDirectory
    strInp <- getInputLine  ("hsi: " <> strDir <> ": ")
    when (isNothing strInp) $ uiLoop
    let tokens = (T.words . T.pack . fromJust) strInp
    if null tokens
      then do
        outputStrLn "No input received"
        uiLoop
       else do
        let cmd = head tokens
            params = tail tokens
        case cmd of
          "exit" -> return ()
          _ -> do
              lift $ uiCommands cmd params
              uiLoop

-- Process the diffenent commands.
uiCommands :: Text -> [Text] -> UiMonad ()
uiCommands cmd params = do
    case cmd of
      "auto"   -> uiAuto params
      "draw"   -> uiDisplay params
      "hidden" -> uiHidden params
      "hsi"    -> uiHsi params
      "indir"  -> uiIndir params
      "list"   -> uiList
      "load"   -> uiLoadHss params
      "outdir" -> uiOutdir params
      "pdir"   -> uiPdir params
      "size"   -> uiSize params
      _        -> liftIO $ T.putStrLn ("incorrect input `" <> cmd <> "`")
      -- The `end`command is processed in the uiLoop Function above.

-- General cheap error handler
handler :: SomeException -> Text
handler e = T.pack $ show e

-- Change the automatic hi calculation mode
uiAuto :: [Text] -> UiMonad ()
uiAuto params = runExceptT
    (verifyLength1 "auto" params >>= verifyBoolean "auto" >>= doAuto)
     >>= report
  where
    doAuto :: (MonadIO m, MonadState UiState m) => Bool -> ExceptT Text m ()
    doAuto auto = gets uiDparms >>= putDparm . dpSetAuto auto

-- Change the input directory for reading files
uiIndir :: [Text] -> UiMonad ()
uiIndir parms = runExceptT (
    (verifyLength1 "indir" (T.unpack <$> parms)) >>= verifyDirExists >>= doIndir)
      >>= report
  where
    doIndir :: (MonadIO m, MonadState UiState m ) => FilePath -> ExceptT Text m ()
    doIndir dir = gets uiDparms >>= putDparm . dpSetIndir dir

-- Change the output directory for reading files
uiOutdir :: [Text] -> UiMonad ()
uiOutdir parms = runExceptT (
    (verifyLength1 "outir" (T.unpack <$> parms)) >>= verifyDirExists >>= doOutdir)
      >>= report
  where
    doOutdir :: (MonadIO m, MonadState UiState m ) => FilePath -> ExceptT Text m ()
    doOutdir dir = gets uiDparms >>= putDparm . dpSetIndir dir

-- Read the halfspaces from a file and calculate the polytope
uiLoadHss :: [Text] -> UiMonad ()
uiLoadHss parms =
  (runExceptT $ do
    poly <- verifyLength1 "load" parms >>= verifyLoad >>= verifyDims >>= doLoad
    dparms <- gets uiDparms
    case dpAuto dparms of
      True  -> doHsi poly
      False -> pure ()
  ) >>= report
  where
    doLoad  :: (MonadIO m, MonadState UiState m ) => [VU.Vector Double] -> ExceptT Text m HsiPolytope
    doLoad vecs = do
      let dim = 3
          initialCube = mkCube dim
          hss = hsFromVector <$> vecs
          phs = zip ([1..]::[Int]) vecs
      liftIO $ putStrLn $ show (length phs) <>  " halfspaces: "
      putHss hss
      putPoly $ Just initialCube
      pure initialCube
    verifyLoad :: (MonadIO m, MonadState UiState m, MonadCatch m) =>
                  Text -> ExceptT Text m [VU.Vector Double]
    verifyLoad textpath = do
      dparms <- gets uiDparms
      let path = (dpIndir dparms) </> T.unpack textpath
      -- Read the data
      liftIO $ putStrLn ("Reading file " <> path)
      contents <- handleExceptT handler $ liftIO $ T.readFile path
      -- Write the filename into the UIState
      putDparm $ dpSetFilename (takeBaseName path) dparms
      -- Verify the data
      (verifyVectors $ parseLine <$> T.lines contents) >>= verifyNonZeroVecs >>= verifyDims
    parseLine :: Text -> V.Vector (Maybe Double)     -- The outer Maybe is for unfoldr
    parseLine l = V.unfoldr step (skipBlanks l)      -- The inner for error handling!
      where
        step :: Text -> Maybe (Maybe Double, Text)
        step "" = Nothing
        step !s = case T.double s of
            Left _ -> Just $ (Nothing , "")
            Right (!d, !t) -> Just $ (Just d, skipBlanks t)
        skipBlanks :: Text -> Text
        skipBlanks = T.dropWhile (isSpace)
    verifyVectors :: (MonadIO m) => [V.Vector (Maybe Double)] -> ExceptT Text m [VU.Vector Double]
    verifyVectors vecs = do
        let toUnboxed :: V.Vector (Maybe Double) -> VU.Vector Double
            toUnboxed = VG.convert . V.map fromJust
            hasNothing :: V.Vector (Maybe Double) -> Bool
            hasNothing v = not $ V.null $  V.filter isNothing v
            errs  = filter (hasNothing . snd) $ zip ([1..]::[Int]) vecs
        if null errs
              then return $ toUnboxed <$> vecs
              else throwError $ T.pack ("Input errors in lines " ++ show (map fst errs))
    verifyDims :: (MonadIO m) => [VU.Vector Double] -> ExceptT Text m [VU.Vector Double]
    verifyDims vecs = do
        -- All input equations must have the same dimension
        let dims = nub $ VU.length <$> vecs
        case length dims of
          0 -> throwError "No half-space definitions found"
          1 -> return vecs
          _ -> throwError $ T.pack ("Not all input equations have the same dimension " ++ show dims)
    verifyNonZeroVecs :: (MonadIO m) => [VU.Vector Double] -> ExceptT Text m [VU.Vector Double]
    verifyNonZeroVecs vecs = do
        let vs = (VU.init <$> vecs)              -- only coeffs
            vsa = fmap (VU.map abs) vs           -- take them positive
            vss = VU.sum <$> vsa                 --  sum
            ps = zip ([1..]::[Int]) vss            -- pair them with line numbers
            zeros = filter ((< 0.001) . snd) ps
        if null zeros
          then return vecs
          else throwError $ T.pack ("Input lines with invalid halfspace equations " ++ show (fst <$> zeros))

uiHsi :: [Text] -> UiMonad ()
uiHsi params = do
  runExceptT
      (verifyLength "hsi" 0 params >> verifyPoly >>= doHsi)
      >>= report

-- Run the HSI algorithm on the halfspaces into the halfspace store
doHsi :: (MonadIO m, MonadState UiState m ) => HsiPolytope -> ExceptT Text m ()
doHsi poly = do
    hss <- gets uiHss
    case hsiPoly poly hss of
      (Left msg)   -> do
        liftIO $ putStrLn msg
        putPoly Nothing
      (Right newPoly) -> do
        liftIO $ do
          putStrLn $ show $ polyStats newPoly
          putStrLn $ show $ checkFormulaEuler newPoly
        -- update state: remove halfspaces, add polytope
        putPoly $ Just newPoly
    putHss []

-- List the current polytope in the UI State
uiList :: (MonadState UiState m, MonadIO m) => m()
uiList = do
    liftIO $ putStrLn "Unprocessed Halfspaces:"
    liftIO $ putStrLn "-----------------------"
    hslist <- gets uiHss
    if (null hslist)
        then liftIO $ putStrLn "(empty)"
        else liftIO $ mapM_ putStrLn $ map show hslist
    runExceptT
      (verifyPoly >>= doListPoly)
      >>= report
  where
    doListPoly :: MonadIO m => HsiPolytope -> ExceptT Text m ()
    doListPoly poly = liftIO $ do
      putStrLn "\nPolytope"
      putStrLn $ show poly
      putStrLn $ show $ polyStats poly
      putStrLn $ show $ checkFormulaEuler poly

-- Display the current polytope with the current display parameters
uiDisplay :: (MonadState UiState m, MonadIO m) => [Text] -> m()
uiDisplay params = do
    runExceptT
      (verifyLength "draw" 0 params >> verifyPoly >>= verify3DimPoly >>= doDisplay)
      >>= report
  where
    -- Verify that there exists a polytope in the UIState
    verify3DimPoly :: (MonadState UiState m, MonadIO m) => HsiPolytope -> ExceptT Text m HsiPolytope
    verify3DimPoly poly
      | polyDim poly == 3 = return poly
      | otherwise         = throwError "For drawing, the dimension must be 3"
    doDisplay :: (MonadState UiState m, MonadIO m) => HsiPolytope -> ExceptT Text m ()
    doDisplay poly = do
      dparms <- gets uiDparms
      -- Transform a HsiPolytope into a VisPolytope
      liftIO $ display dparms $ polyHsi2Vis poly

-- Process the `size` input
uiSize :: [Text] -> UiMonad ()
uiSize params =
       runExceptT (verifyLength "size" 2 params >>= verifyNumTokens >>= doSize) >>= report
  where
    doSize :: (MonadIO m, MonadState UiState m) => VU.Vector Double -> ExceptT Text m ()
    doSize vec = gets uiDparms >>= putDparm . fupd vec
    fupd :: VU.Vector Double -> DispParams -> DispParams
    fupd v = dpSetSize (point2 (VU.head v) (VU.last v))

-- Set the projection direction
uiPdir :: [Text] -> UiMonad ()
uiPdir params = runExceptT
        (verifyLength "pdir" 3 params >>= verifyNumTokens >>= doPdir)
         >>= report
  where
    doPdir :: (MonadIO m, MonadState UiState m) => VU.Vector Double -> ExceptT Text m ()
    doPdir vec = gets uiDparms >>= putDparm . dpSetPdir vec

uiHidden :: [Text] -> UiMonad ()
uiHidden params = runExceptT
    (verifyLength1 "hidden" params >>= verifyBoolean "hidden" >>= doHidden)
     >>= report
  where
    doHidden :: (MonadIO m, MonadState UiState m) => Bool -> ExceptT Text m ()
    doHidden showHidden = gets uiDparms >>= putDparm . dpSetHidden showHidden

-- TODO: Validate halfspaces: Are there any halfspaces and have all the same dimension
-- validateHss

-- ---------------------------------------------------------------------------------
-- General validation functions
-- ---------------------------------------------------------------------------------

-- report the verification errors
report :: (MonadIO m) => Either Text () -> m ()
report (Left e) = liftIO $ T.putStrLn $ "*** " <> e
report (Right _) = return ()

-- Verify that the tookens are numeric, and collect them in a Double vector
verifyNumTokens :: (MonadIO m) => [Text] -> ExceptT Text m (VU.Vector Double)
verifyNumTokens toks = do
    let part = partition isNothing $ parse <$> toks
    if null (fst part)
         then return $ VU.fromList $ fromJust <$> snd part
         else throwError $ "Not all tokens are numeric"
  where
    parse :: Text -> Maybe Double
    parse tok = case T.double tok of
        Left _        -> Nothing
        Right (!d, _) -> Just d

verifyLength1 :: (MonadIO m) => Text -> [a] -> ExceptT Text m a
verifyLength1  msg toks =
    if length toks == 1
        then return $ head toks
        else throwError $ msg <> " expects 1 parameter"

verifyLength :: (MonadIO m) => Text -> Int -> [a] -> ExceptT Text m [a]
verifyLength  msg len toks =
    if length toks == len
        then return toks
        else throwError $ msg <> T.pack (" expects " <> show len <> " parameter(s")

-- Verify that there exists a polytope in the UIState
verifyPoly :: (MonadState UiState m, MonadIO m) => ExceptT Text m HsiPolytope
verifyPoly = do
    mbPoly <- gets uiPoly
    go mbPoly
  where
    go :: (MonadIO m) => Maybe HsiPolytope -> ExceptT Text m HsiPolytope
    go (Just poly) = return poly
    go  Nothing     = throwError ("No polytope found")

verifyBoolean :: (MonadState UiState m, MonadIO m) => Text -> Text -> ExceptT Text m Bool
verifyBoolean _  "0" = return False
verifyBoolean _  "1" = return True
verifyBoolean msg _   = throwError $ msg <> " expects '0' or '1'"

-- Verify whether a directory exists
verifyDirExists :: (MonadIO m) => FilePath -> ExceptT T.Text m FilePath
verifyDirExists dir = do
    bExits <- liftIO $ doesDirectoryExist dir
    if bExits
       then return dir
       else throwError $ "Directory `" <> T.pack  dir <> "` doesn't exist"
