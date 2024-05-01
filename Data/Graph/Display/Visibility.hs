module Data.Graph.Display.Visibility where

import Data.Graph.Dag
import Data.Graph.HSI
import Data.Graph.Display.Data

import Control.Monad (when )
import qualified Data.Vector.Unboxed as VU
import qualified Data.EnumMap.Strict as Map

-- Calculate the visibility flags for all faces of our polytope
visPoly :: VU.Vector Double -> VisPolytope -> VisDag
visPoly direction poly =
    dsDag $ preOrderFilter Multiple (visNode direction) condNode Visible $ polyDag poly
    -- It's not necessary to reset the visibility field on all the nodes,
    --   as long as the new dag is not written back to the polytope.
  where
    hsmap = polyHs poly
    -- NodeFunction to calculate the visiblity of a node
    visNode :: VU.Vector Double -> NodeFunction Face Visibility Visibility
    visNode  dirvect (nodekey, node) = do
        dag <- getDag
        vis <- getClState
        let -- if the scalarproduct beetween the direction vector and
            -- the hyper plane vector is positive, then the hyperplane is visible
            calcNewVis :: Dim -> Visibility
            calcNewVis dim
              | dim == 0        = Hidden
              | dim == hyperDim = if cosDirHsvect >= 0
                                                then Visible
                                                else Hidden
              | otherwise        = vis
            -- The dimension of a hyperplane is one less, than the dimension of
            --   our space
            hyperDim = (fromInteger . toInteger) (VU.length direction - 1)
            -- `hsmap` we get from the enclosing closure (where above)
            hskey = head $ nodeHsKeys node
            hsVect = hsEquation $ hsmap Map.! hskey
            cosDirHsvect = sp dirvect hsVect
        putClState $ calcNewVis $ nodeDim node
        -- if a face is visible, then all its kids are also visible.
        when (vis == Visible && nodeAttr node == Hidden) $ do
            let newNode = nodeSetAttr vis node
            putDag $ dagUpdateNode dag nodekey newNode

    -- Predicate function to stop processing on nodes that are already marked Visible
    -- and do not process Vertices.
    condNode :: NodePredicate Face Visibility
    condNode node = condVis && condDim
      where
        -- Process only nodes, that are not yet visible
        condVis = case nodeAttr node of
            Hidden -> True
            Visible -> False
        -- Do not process Vertexes
        condDim = nodeDim node > 0
