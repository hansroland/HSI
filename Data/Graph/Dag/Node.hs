{-# OPTIONS_HADDOCK hide #-}
{-# Language GeneralisedNewtypeDeriving, DerivingStrategies #-}
{-# Language DeriveFunctor #-}
{-# Language NamedFieldPuns #-}

module Data.Graph.Dag.Node (
    NodeKey,
    Node(..),
    nodeAddKey,
    nodeAddKeys,
    nodeSetData

) where

-- | Keys or IDs of a node.
-- An unique value for every node.
-- They are created at the time we insert a node into the Dag.
newtype NodeKey = NodeKey Int
    deriving (Eq, Ord, Enum)
    deriving newtype (Read, Show, Num)

-- | A Node is a list of sub nodes and some additional data.
data Node n a = Node
        {nodeKids :: ![NodeKey],                  -- ^ The keys of the subnodes.
        nodeData :: !n,                           -- ^ The node data, or payload.
        nodeAttr :: !a}                           -- ^ An additional node attribute
    deriving (Functor)

instance (Show n, Show a) => Show (Node n a) where
    show Node {nodeKids, nodeData} =
        show nodeKids ++ " " ++ show nodeData

-- | Add a subnode key to a node.
nodeAddKey :: NodeKey -> Node n a -> Node n a
nodeAddKey newKey node = node {nodeKids = newKey : nodeKids node }

-- | Add multiple subnode keys to a node.
nodeAddKeys :: [NodeKey] -> Node n a -> Node n a
nodeAddKeys newKeys node =
    node {nodeKids = newKeys <> nodeKids node}

-- | Set a new value to the nodeData of a node.
nodeSetData :: n -> Node n a -> Node n a
nodeSetData newData node@Node{} = node { nodeData = newData }
