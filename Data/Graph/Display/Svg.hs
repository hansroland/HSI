{-# LANGUAGE OverloadedStrings #-}
{-# Language NamedFieldPuns #-}

module Data.Graph.Display.Svg where

import Data.Graph.Display.Data
import Data.Graph.Display.DispParams
import Data.Graph.Display.Vect2

import Graphics.Svg
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Lazy.IO as TLIO

import Control.Monad.IO.Class (MonadIO (..))
import Control.Monad.Except
import Control.Error (handleExceptT)
import Control.Monad.Catch
import Web.Browser(openBrowser)
import System.FilePath

-- General cheap error handler
handler :: SomeException -> Text
handler e = T.pack $ show e

-- Plot the 2-dim edges of the polytope as a svg to a file
-- Here we also change the y-coordinate.
-- Normally the origin is botton left, however in svg the
-- origin is top left. Here we change.
plot :: (MonadIO m, MonadCatch m ) => DispParams ->  [Line] -> ExceptT Text m ()
plot dparms lns = do
    let maxy = round $ y $ dpSize dparms
        svgLines = map (lineToSvg maxy) lns
        outdir = dpOutdir dparms
        filename = dpFilename dparms
        svgFile = outdir </> filename <.> "svg"
        svgdata = svgDoc dparms $ svgHdr <> mconcat svgLines
        reportBrowser :: Bool -> IO ()
        reportBrowser True  = putStrLn "Browser Ok"
        reportBrowser False = putStrLn "Browser failed"

    handleExceptT handler $ liftIO $ do
        TLIO.writeFile svgFile $ renderText svgdata
        putStrLn $ "File written: " <> svgFile
        openBrowser svgFile >>= reportBrowser

-- Create the Svg document
svgDoc :: DispParams -> Element -> Element
svgDoc dparms content =
    let size = dpSize dparms
        w = toV $ round $ x size
        h = toV $ round $ y size
    in  doctype
        <> with (svg11_ content) [Version_ <<- "1.1", Width_ <<- w, Height_ <<- h]

-- The SVG header consists of a base rectangle with a white background
svgHdr :: Element
svgHdr = rect_ [Width_ <<- "100%", Height_ <<- "100%", Fill_ <<- "white"]

-- Convert a Line to a Svg Element
lineToSvg :: Int -> Line -> Element
lineToSvg maxy (Line {lix1, lix2, liy1, liy2, liStyle }) =
    let vy1 = toV $ maxy - liy1
        vy2 = toV $ maxy - liy2
        attrs = [X1_ <<- toV lix1, Y1_ <<- vy1,
                 X2_ <<- toV lix2, Y2_ <<- vy2,
                 Stroke_ <<- "black", Stroke_width_ <<- "1"]
    in  case liStyle of
          Normal -> line_ attrs
          Dotted -> line_ ([Stroke_dasharray_ <<- "3, 3"] <>  attrs)

-- Little helper function to create svg attribute values
toV :: Int -> Text
toV = T.pack . show
