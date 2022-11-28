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

-- Convert an Int to a NodeKey
int2NodeKey :: Int -> NodeKey
int2NodeKey = NodeKey

-- --------------------------------------------------------------
-- Node
-- --------------------------------------------------------------
-- A Node is a list of sub nodes and some data
data Node a = Node {nodeKids :: ![NodeKey], nodeData :: !a}
     deriving (Functor)

instance Show a => Show (Node a) where
    show Node {nodeKids, nodeData} =
        show nodeKids ++ " " ++ show nodeData

-- Add a subnode key to a node
-- Attention: It's important to add new keys in front
nodeAddKey :: NodeKey -> Node a -> Node a
nodeAddKey newKey node = node {nodeKids = newKey : nodeKids node }

-- Add multiple subnode keys to a node
nodeAddKeys :: [NodeKey] -> Node a -> Node a
nodeAddKeys newKeys node =
    node {nodeKids = newKeys <> nodeKids node}

-- Update the nodeData of a node
nodeUpdateData :: Node a -> a -> Node a
nodeUpdateData node newData = node { nodeData = newData}

-- Update the nodeData of a node with a function
nodeUpdateDataWith :: (a -> a) -> Node a -> Node a
nodeUpdateDataWith f node = node{ nodeData = f (nodeData node)}

-- isLeaf
isLeaf :: Node a -> Bool
isLeaf (Node{nodeKids}) = null nodeKids
