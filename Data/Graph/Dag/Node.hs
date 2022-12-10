{-# Language GeneralisedNewtypeDeriving, DerivingStrategies #-}
{-# Language DeriveFunctor #-}
{-# Language NamedFieldPuns #-}

module Data.Graph.Dag.Node where

-- --------------------------------------------------------------
-- Node Key
-- --------------------------------------------------------------
newtype NodeKey = NodeKey Int
    deriving (Eq, Ord, Enum)
    deriving newtype (Read, Show, Num)

-- --------------------------------------------------------------
-- Node
-- --------------------------------------------------------------
-- A Node is a list of sub nodes and some data
data Node n a = Node
        {nodeKids :: ![NodeKey],
        nodeData :: !n ,
        nodeAttr :: !a}
    deriving (Functor)

instance (Show n, Show a) => Show (Node n a) where
    show Node {nodeKids, nodeData} =
        show nodeKids ++ " " ++ show nodeData

-- Add a subnode key to a node
-- Attention: It's important to add new keys in front
nodeAddKey :: NodeKey -> Node n a -> Node n a
nodeAddKey newKey node = node {nodeKids = newKey : nodeKids node }

-- Add multiple subnode keys to a node
nodeAddKeys :: [NodeKey] -> Node n a -> Node n a
nodeAddKeys newKeys node =
    node {nodeKids = newKeys <> nodeKids node}

-- Update the nodeData of a node
nodeUpdateData :: Node n a -> n -> a -> Node n a
nodeUpdateData node newData newAttr = node { nodeData = newData, nodeAttr = newAttr}

-- isLeaf
isLeaf :: Node n a -> Bool
isLeaf (Node{nodeKids}) = null nodeKids
