{-# LANGUAGE OverloadedStrings #-}
{-# Language NamedFieldPuns #-}

module Data.Graph.Display.Html(plot) where

import Data.Graph.Display.Data

import Data.Graph.Display.DispParams
import Data.Graph.Display.Vect2

import Data.Text (Text)
import qualified Data.Text as T

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
    ( h1, body, title, head, docTypeHtml, Html, toValue )
import Text.Blaze.Html.Renderer.Text ( renderHtml )
-- import qualified Text.Blaze.Html.Renderer.Pretty as Pretty
import qualified Data.Text.Lazy.IO as TIO
import Web.Browser(openBrowser)

import Control.Monad.Except
import Control.Error (handleExceptT)
import Control.Monad.Catch

-- General cheap error handler
handler :: SomeException -> Text
handler e = T.pack $ show e


-- write the lines of the drawing as an HTML/svg to a file
-- Here we also change the y-coordinate.
-- Normally the origin is botton left, however in html the
-- origin is top left. Here we change.
plot :: (MonadIO m, MonadCatch m ) => DispParams ->  [Line] -> ExceptT Text m ()
plot dparms lns = do
    let maxy = round $ y $ dpSize dparms
        svgLines = map (lineToSvg maxy) lns
        htmlPage = page dparms svgLines
        filePath = dpFilepath dparms
    handleExceptT handler $ liftIO $ TIO.writeFile filePath $ renderHtml $ htmlPage
    handleExceptT handler $ liftIO $ openBrowser filePath >>= print

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
  Text.Blaze.Html5.head $ title "Works"
  body $ do
    h1 "Test to Display"
    svgDoc dparms $ mconcat svgs

-- create a Svg document
svgDoc :: DispParams -> Svg -> Svg
svgDoc dparms =
    let w = toValue $ x $ dpSize dparms
        h = toValue $ y $ dpSize dparms
    in  svg ! width w ! height h
