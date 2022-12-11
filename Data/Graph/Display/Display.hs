{-# Language NamedFieldPuns #-}

module Data.Graph.Display.Display where

import Data.Graph.Dag

import Data.Graph.HSI
import Data.Graph.HSI.Utils

import Data.Graph.Display.DispParams
import Data.Graph.Display.Point2
import Data.Graph.Display.Data

import Data.Graph.Display.Html

import Data.Vector.Unboxed (Vector)
import qualified Data.Vector.Unboxed as VU

import qualified Data.Vector as V

import Data.EnumMap.Strict (EnumMap)
import qualified Data.EnumMap.Strict as Map

-- IndexedEdge are edges with indexes to a vertice map
data IndexedEdge = IndexedEdge {
            segKey1 :: !NodeKey,
            segKey2 :: !NodeKey,
            segVis :: !Visibility
            }
        deriving (Show)

-- A DrawObj contains edges with indexes to the vertices.
--  This allows to do calculations with the vertices (eg center the drawing)
--  with using every vertex once.
data DrawObj = DrawObj {doEdges :: ![IndexedEdge], doVerts :: !PointMap}

type VertMap = EnumMap NodeKey (Vector Double)
type PointMap = EnumMap NodeKey P2

-- Display a polytope
display :: DispParams -> VisPolytope -> IO ()
display dparms poly = do
    let dim = dpSize dparms
        pdir = dpPdir dparms
        dag = visPoly pdir poly
        linesPoly = drawObjToLines $ drawObjCenter dim $ mkDrawObj dparms dag
        linesRect = drawObjToLines $ mkFrameRect dparms
    plot dparms (linesRect <> linesPoly)


-- Create from the DAG aDrawObj
mkDrawObj :: DispParams -> VisDag -> DrawObj
mkDrawObj dparms dag =
    let pdir = dpPdir dparms
        -- calculate the rotation matrix
        rotmat = drehma pdir
        -- rotate and project the vertexes
        drawMap = Map.map (project dparms . multvect rotmat ) $ getVertMap dag
    in DrawObj {doEdges = getEdges dag, doVerts = drawMap}

-- Return an [Enum]Map with the 3-d vectors transfered with the diplay transformation of the vertices
-- Use NodeKey as key.
getVertMap :: VisDag -> VertMap
getVertMap dag =
    let getVec :: Face -> Vector Double
        getVec (Vertex vec _) = vec
        getVec _ = error "Severe error in Display.hs:getVertMap" --we use `isVertex`
        vertexAssocs = filter (isVertex . nodeData. snd) $ dagNodeAssocs dag
        rotVertAssocs = (fmap (getVec . nodeData)) <$> vertexAssocs
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
segToLine :: PointMap -> IndexedEdge -> Line
segToLine pmap seg =
    let p1 = pmap Map.! segKey1 seg
        p2 = pmap Map.! segKey2 seg
        vx1 = round $ x p1
        vy1 = round $ y p1
        vx2 = round $ x p2
        vy2 = round $ y p2
        style = case (segVis seg) of
            Visible -> Normal
            Hidden  -> Dotted
    in Line {lix1= vx1, lix2=vx2, liy1=vy1, liy2=vy2, liStyle = style}

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
drawObjToLines :: DrawObj -> [Line]
drawObjToLines (DrawObj edges pmap) = map (segToLine pmap) edges

-- Transform the drawObj so it fits on the drawing area
--   1. Move the center of the drawing to the origin.
--   2. Scale the drawing up or down to 85% of the size of the drawing board.
--   3. Move the drawing into the center of the drawing board.
drawObjCenter :: P2 -> DrawObj -> DrawObj
drawObjCenter extBoard (drawObj@DrawObj{doVerts}) =
    let points = Map.elems doVerts
        maxxy = foldr max_xy (P2 (-1E10) (-1E10)) points
        minxy = foldr min_xy (P2 1E10 1E10) points
        centImg = divBy (maxxy + minxy) 2
        centBoard = divBy extBoard 2
        extImg = maxxy - minxy
        moveTo0 = flip (-) centImg
        scale = multWith $ scaleFactor (multWith 0.85 extBoard) extImg
        center = (+) centBoard
    in  drawObj{doVerts = Map.map (center . scale . moveTo0) doVerts}

-- scaleFactor :: BoardExt ->  ImageExt -> Factor
scaleFactor :: P2 -> P2 -> Double
scaleFactor (P2 x1 y1) (P2 x2 y2) =
    let check 0 = 1
        check d = d
    in  (min (x1/check x2) (y1/check y2))


type Matrix a = V.Vector (VU.Vector Double)

-- calculate a matrix that transforms the input vector to 1 0 0.
drehma :: VU.Vector Double -> Matrix Double
drehma tv0 =
    let d0 = sqrt $ VU.sum $ VU.zipWith (*) tv0 tv0
        tv = VU.map (/d0) tv0
        ltv = VU.toList tv
        tvt = VU.tail tv
        d  = sqrt $ VU.sum $ VU.zipWith (*) tvt tvt
        ca  = if d == 0 then 1 else (VU.last tv) / d
        sa  = if d == 0 then 0 else -1 * ltv!!1 / d
        cb :: Double
        cb = ltv !! 0
        sb = -sa* ltv!!1 + ca * ltv!!2
        tm1 = VU.fromList [cb, -sa*sb, ca*sb]
        tm2 = VU.fromList [0, ca, sa]
        tm3 = VU.fromList [-sb, -sa*cb, cb*ca]
    in  V.fromList [tm1,tm2,tm3]

multvect :: Matrix Double -> VU.Vector Double -> VU.Vector Double
multvect mat vec = V.convert $ V.map (sp vec) mat

-- project a point from the 3-dim space to the 2-dim plane
-- For parallel projection to 1 0 0, this is just using the y and z components
-- For zentral proj, we need a little bit more.
-- TODO Zentralproj
project :: DispParams -> VU.Vector Double -> P2
project dparms vect3 =
    let v2 = VU.tail vect3
    in P2 (VU.head v2) (VU.last v2)
