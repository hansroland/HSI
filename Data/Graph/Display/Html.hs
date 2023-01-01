{-# LANGUAGE OverloadedStrings #-}
{-# Language NamedFieldPuns #-}

module Data.Graph.Display.Html(plot) where

import Data.Graph.Display.Data

import Data.Graph.Display.DispParams
import Data.Graph.Display.Vect2

import Data.Text (Text)
import qualified Data.Text as T
import Data.Char
import System.FilePath

-- imports for SVG
import Text.Blaze.Svg11 ( line, svg, Svg, (!) )
import Text.Blaze.Svg11.Attributes
    ( stroke,
      strokeWidth,
      x1, y1, x2, y2,
      strokeDasharray,
      height,
      width )
import Text.Blaze.Html5
    ( h1, body, title, head, docTypeHtml, Html, toValue, toHtml)
import Text.Blaze.Html.Renderer.Text ( renderHtml )
-- import qualified Text.Blaze.Html.Renderer.Pretty as Pretty
import qualified Data.Text.Lazy as TL
import qualified Data.Text.Lazy.IO as TIO
import Web.Browser(openBrowser)

import Control.Monad.Except
import Control.Error (handleExceptT)
import Control.Monad.Catch

-- General cheap error handler
handler :: SomeException -> Text
handler e = T.pack $ show e


-- Write the lines of the drawing as a HTML/svg to a file
-- Here we also change the y-coordinate.
-- Normally the origin is botton left, however in html the
-- origin is top left. Here we change.
plot :: (MonadIO m, MonadCatch m ) => DispParams ->  [Line] -> ExceptT Text m ()
plot dparms lns = do
    let maxy = round $ y $ dpSize dparms
        svgLines = map (lineToSvg maxy) lns
        htmlPage = page dparms svgLines
        outdir = dpOutdir dparms
        filename = dpFilename dparms
        htmlFile = outdir </> filename <.> "html"
        svgFile = outdir </> filename <.> "svg"
        svgdata = svgDoc dparms $ mconcat svgLines
    handleExceptT handler $ liftIO $ TIO.writeFile htmlFile $ renderHtml $ htmlPage
    handleExceptT handler $ liftIO $ TIO.writeFile svgFile $ addXmlns $ renderHtml $ toHtml svgdata
    handleExceptT handler $ liftIO $ openBrowser htmlFile >>= print


--  SEE: /home/roland/Projekte/HaskellTutorials/svg/svgbuilder/Main.hs how to do it!
-- Convert a Line to a Svg action
lineToSvg :: Int -> Line -> Svg
lineToSvg maxy (Line {lix1, lix2, liy1, liy2, liStyle }) =
    let vx1 = toValue lix1
        vx2 = toValue lix2
        vy1 = toValue $ maxy - liy1
        vy2 = toValue $ maxy - liy2
        ln = line ! x1 vx1 ! y1 vy1 ! x2 vx2 !  y2 vy2 ! strokeWidth "1" ! stroke "black"
    in  case liStyle of
          Normal -> ln
          Dotted -> ln ! strokeDasharray "3, 3"

-- Create a Html page to embed the drawing
page :: DispParams -> [Svg] -> Html
page dparms svgs = docTypeHtml $ do
    let fn = dpFilename dparms
        tit = toHtml $ cap fn
    Text.Blaze.Html5.head $ title $ tit
    body $ do
        h1 tit
        svgDoc dparms $ mconcat svgs
  where
    cap :: String -> String
    cap "" = ""
    cap (c:cs) = toUpper c : map toLower cs

-- create a Svg document
svgDoc :: DispParams -> Svg -> Svg
svgDoc dparms =
    let w = toValue $ x $ dpSize dparms
        h = toValue $ y $ dpSize dparms
    in  svg ! width w ! height h

-- Standalone svg files need a xmlns specification.
-- When missing, browsers don't display the image.
-- TODO: Remove this wart !
addXmlns :: TL.Text -> TL.Text
addXmlns xml =
    let tag = TL.take 4 xml
        rest = TL.drop 4 xml
    in tag `TL.append` " xmlns=\"http://www.w3.org/2000/svg\" " `TL.append` rest
