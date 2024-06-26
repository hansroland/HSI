{-# Language NamedFieldPuns #-}

module Data.Graph.Display.Display where

import Data.Graph.Dag
import Data.Graph.HSI

import Data.Graph.Display.DispParams
import Data.Graph.Display.Vect2
import Data.Graph.Display.Data
import Data.Graph.Display.Svg
import Data.Graph.Display.Visibility

import Data.Vector.Unboxed (Vector)
import qualified Data.Vector.Unboxed as VU
import qualified Data.Vector as V
import Data.EnumMap.Strict (EnumMap)
import qualified Data.EnumMap.Strict as Map
import Data.Text (Text)
import qualified Data.Text.IO as T
import Control.Monad.Except
import Control.Monad.IO.Class (MonadIO (..))
import Data.Maybe(mapMaybe)

-- An IndexedEdge is an edge with indexes to a vertice map.
-- A verice map contains vertices indexed by NodeKeys.
data IndexedEdge = IndexedEdge {
            segKey1 :: !NodeKey,
            segKey2 :: !NodeKey,
            segVis :: !Visibility
            }
        deriving (Show)

-- A DrawObj contains edges with indexes to a vertice map.
--  This allows to do calculations with the vertices (eg center the drawing)
--  with using every vertex once.
data DrawObj = DrawObj {drEdges :: ![IndexedEdge], drVerts :: !PointMap}

type Matrix = V.Vector (VU.Vector Double)
type VertMap = EnumMap NodeKey (Vector Double)
type PointMap = EnumMap NodeKey P2

-- Convert HsiPolytope to VisPol<tope
polyHsi2Vis :: HsiPolytope -> VisPolytope
polyHsi2Vis poly = poly{ polyDag = newDag}
 where
    dag = polyDag poly
    newNds = Map.map hsi2vis . dagNodes . polyDag
    newDag = dag {dagNodes = newNds poly}
    hsi2vis :: HsiNode -> VisNode
    hsi2vis node = node {nodeAttr = Hidden}


-- Display a polytope
display :: DispParams -> VisPolytope -> IO ()
display dparms poly = do
    let dim = dpSize dparms
        pdir = dpPdir dparms
        showHidden = dpHidden dparms
        dag = visPoly pdir poly
        linesPoly = drawObjToLines showHidden $ drawObjCenter dim $ mkDrawObj dparms dag
        linesRect = drawObjToLines True $ mkFrameRect dparms
    runExceptT (plot dparms (linesRect <> linesPoly)) >>= report
  where
    report :: (MonadIO m) => Either Text () -> m ()
    report (Left e) = liftIO $ T.putStrLn e
    report (Right _) = return ()

-- Transform a Dag into aDrawObj
-- Project from 3-D to 2-D.
mkDrawObj :: DispParams -> VisDag -> DrawObj
mkDrawObj dparms dag =
    let pdir = dpPdir dparms
        -- calculate the rotation matrix
        rotmat = drehma pdir
        -- rotate and project the vertexes
        drawMap = Map.map (project . multvect rotmat) $ getVertMap dag
        -- rotate and project the z-axis
        zaxis = multvect rotmat $ dpZaxis dparms
        z2axis = (project . multvect rotmat) zaxis
        rot2Map = adjustOrientation2 z2axis drawMap
    in  DrawObj {drEdges = getEdges dag, drVerts = rot2Map}

-- Return an [Enum]Map with the 3-d vectors transfered with the diplay transformation of the vertices
-- Use NodeKey as key.
getVertMap :: VisDag -> VertMap
getVertMap dag =
    let getVec :: Face -> Vector Double
        getVec (Vertex vec _) = vec
        getVec _ = error "Severe error in Display.hs:getVertMap" --we use `isVertex`
        vertexAssocs = filter (isVertex . nodeData. snd) $ dagNodeAssocs dag
        rotVertAssocs = fmap (getVec . nodeData) <$> vertexAssocs
    in  Map.fromList rotVertAssocs

-- Create the edges to draw
getEdges :: VisDag -> [IndexedEdge]
getEdges dag =
    let node2seg :: VisNode -> IndexedEdge
        node2seg node =
            let kids = nodeKids node
            in  IndexedEdge {
                  segKey1 = Prelude.head kids,
                  segKey2 = Prelude.last kids,
                  segVis  = nodeAttr node
                  }
    in  fmap node2seg $ filter (\n -> nodeDim n == 1) $ Map.elems $ dagNodes dag

-- Make visible edges
mkVisEdge :: Int -> Int -> IndexedEdge
mkVisEdge k1 k2 = IndexedEdge (fromIntegral  k1) (fromIntegral k2) Visible

-- Convert a IndexedEdge to a Line
segToLine :: Bool -> PointMap -> IndexedEdge -> Maybe Line
segToLine showHidden pmap seg =
    let p1 = pmap Map.! segKey1 seg
        p2 = pmap Map.! segKey2 seg
        vx1 = round $ x p1
        vy1 = round $ y p1
        vx2 = round $ x p2
        vy2 = round $ y p2
        vis = segVis seg
        style = case vis of
            Visible -> Normal
            Hidden  -> Dotted
    in if showHidden  || vis == Visible
         then Just Line {lix1= vx1, lix2=vx2, liy1=vy1, liy2=vy2, liStyle = style}
         else Nothing

-- Create a rectangle to frame the drawing area.
mkFrameRect :: DispParams -> DrawObj
mkFrameRect dpParms =
    let size = dpSize dpParms
        px1 = 1
        py1 = 1
        px2 = x size - 1
        py2 = y size - 1
    in mkRect px1 px2 py1 py2

-- A helper function to create a rectangle
mkRect :: Double -> Double -> Double -> Double -> DrawObj
mkRect x0 x1 y0 y1 =
    let pmap = Map.fromList [
            (1, point2 x0 y0), (2, point2 x1 y0),
            (3, point2 x0 y1), (4 ,point2 x1 y1) ]
        segs = [mkVisEdge 1 2,  mkVisEdge 1 3,
                mkVisEdge 2 4,  mkVisEdge 3 4]
    in DrawObj segs pmap

-- convert a DrawObject to a list of Svgs
drawObjToLines :: Bool -> DrawObj -> [Line]
drawObjToLines showHidden (DrawObj edges pmap) = mapMaybe (segToLine showHidden pmap) edges

-- Transform the drawObj so it fits on the drawing area
--   1. Move the center of the drawing to the origin.
--   2. Scale the drawing up or down to 85% of the size of the drawing board.
--   3. Move the drawing into the center of the drawing board.
drawObjCenter :: P2 -> DrawObj -> DrawObj
drawObjCenter extBoard drawObj@DrawObj{drVerts} =
    let points = Map.elems drVerts
        maxxy = foldr maxXy (point2 (-1E10) (-1E10)) points    -- the point with the biggest coordinae
        minxy = foldr minXy (point2 1E10 1E10) points          -- the point with the smallest coord
        centImg = divBy (maxxy + minxy) 2                       -- The center of the points
        centBoard = divBy extBoard 2                            -- The center of the board
        extImg = maxxy - minxy                                  -- The size of the image
        moveToOrig = flip (-) centImg                           -- Move the center of image to the origin
        scale = multWith $ scaleFactor (multWith 0.85 extBoard) extImg
        center = (+) centBoard                                -- Move the origin to the center of the board
    in  drawObj{drVerts = Map.map (center . scale . moveToOrig) drVerts}

scaleFactor :: P2 -> P2 -> Double
scaleFactor v1 v2 =
    let check 0 = 1
        check d = d
    in  min (x v1 / check (x v2)) (y v1 / check (y v2))

-- Adjust the drawing points, so the z-axis always points upwards.
adjustOrientation2 :: Vector Double -> PointMap -> PointMap
adjustOrientation2 zAxis pointMap =
    let normZ = normalize zAxis
        sina = normZ VU.! 0
        cosa = normZ VU.! 1
        rotMat = V.fromList [
            VU.fromList [-cosa, sina],
            VU.fromList [-sina, -cosa] ]
    in Map.map (multvect rotMat) $ pointMap

-- calculate a matrix that transforms the input vector to 1 0 0.
drehma :: VU.Vector Double -> Matrix
drehma tv0 =
    let tv = normalize tv0
        tv1 = tv VU.! 0
        tv2 = tv VU.! 1
        tv3 = tv VU.! 2
        tvt = VU.tail tv         -- tv(2), tv(3)
        d  = sqrt $ sp tvt tvt
        cosa = if d == 0 then 1 else  tv3 / d
        sina = if d == 0 then 0 else -tv2 / d
        cosb = tv1
        sinb = -sina* tv2 + cosa * tv3
        tm1 = VU.fromList [cosb, -sina*sinb, cosa*sinb]
        tm2 = VU.fromList [0, cosa, sina]
        tm3 = VU.fromList [-sinb, -sina*cosb, cosb*cosa]
    in  V.fromList [tm1,tm2,tm3]

multvect :: Matrix -> VU.Vector Double -> VU.Vector Double
multvect mat vec = V.convert $ V.map (sp vec) mat

-- project a point from the 3-dim space to the 2-dim plane
-- For parallel projection to 1 0 0, this is just using the y and z components
-- For zentral proj, we need a little bit more.
-- TODO Zentralproj
project :: VU.Vector Double -> P2
project vect3 =
    let v2 = VU.tail vect3
    in point2 (VU.head v2) (VU.last v2)
