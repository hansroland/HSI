{-# OPTIONS_HADDOCK hide #-}
{-# Language NamedFieldPuns #-}
{-# LANGUAGE FlexibleContexts #-}   -- used for mtl functions
{-# Language ScopedTypeVariables #-}
-- |
-- Module      : Data.Graph.Dag.Dag
-- Copyright   : (c) Roland Senn 2022-2023
-- License     : BSD-style
-- DAG Directed Acyclic Graph to be used for the Halfspace Intersection project.
module Data.Graph.Dag.Dag where

import Data.Graph.Dag.Node ( NodeKey(..), Node(Node, nodeKids, nodeAttr) )

import Prelude hiding (pred)
import Data.EnumMap.Strict (EnumMap)
import qualified Data.EnumMap.Strict as Map
import Data.Set (Set)
import qualified Data.Set as Set

import Control.Monad.State.Strict
    ( State, MonadState(put, get), gets, modify', execState )
import Data.Maybe ( fromMaybe )
import Data.List ((\\), nub)

-- | Directed Acyclic Graphs.

-- | A Dag contains the NodeKey of the start node and a map with all nodes.
data Dag n a = Dag
      { dagStart :: !NodeKey,                 -- ^ The start node for this Dag in dagNodes
        dagNodes :: !(NodeMap n a)            -- ^ The node map with all the nodes
      }

-- | Store the Dag in a Map. Keys are NodeKeys.
type NodeMap n a = EnumMap NodeKey (Node n a)


instance (Show n, Show a) => Show (Dag n a) where
    show Dag {dagStart, dagNodes} = concat
      ("DAG start = " : show  dagStart : nl : map showassoc ( Map.toAscList dagNodes))
      where
        showassoc :: (Show n, Show a) => (NodeKey, Node n a) -> String
        showassoc (key, nd) = show key ++ " => " ++ show nd
            ++ "  " ++ show (nodeAttr nd) ++ nl
        nl = ['\n']

-- | Return the node with a given NodeKey.
-- We don't check for failure. Every NodeKey we use, MUST exist in the map!
dagNode :: Dag n a -> NodeKey -> Node n a
dagNode dag key = (dagNodes dag) Map.! key

-- | Return the start node
dagStartNode :: Dag n a -> Node n a
dagStartNode dag = dagNode dag $ dagStart dag

-- | Initialize a DAG with the root node
dagInit :: NodeKey -> Dag n a
dagInit (NodeKey key) = Dag {dagStart = (NodeKey key), dagNodes = Map.empty}

-- | Create new node for a given NodeKey and insert it into the dag
dagCreateNode :: NodeKey -> [NodeKey] -> n -> a -> Dag n a -> Dag n a
dagCreateNode key subs nd attr dag =
    dag { dagNodes = Map.insert key (Node subs nd attr) (dagNodes dag)}

-- | Insert a new Node insert it into the dag. Return the new NodeKey
dagInsertNode :: Node n a -> Dag n a -> (NodeKey, Dag n a)
dagInsertNode node dag =
    let maxKey = fromMaybe 0 $ fst <$> (Map.lookupMax $ dagNodes dag)
        hsKey = (1 + maxKey)
        newnodes = Map.insert hsKey node $ dagNodes dag
    in  (hsKey, dag{dagNodes = newnodes})

-- | Update a node with a given NodeKey. (If it doesn't exist, it will be added).
dagUpdateNode :: Dag n a -> NodeKey -> Node n a -> Dag n a
dagUpdateNode dag key node =
    dag {dagNodes = Map.insert key node $ dagNodes dag}

-- | Get the NodeKeys and nodes of all the grandchildren of a given node.
dagGrandNodes :: Dag n a -> Node n a ->[(NodeKey, Node n a)]
dagGrandNodes dag node =
  let kidskeys = nodeKids node
      kidsnodes = dagNode dag <$> kidskeys
      grandkeys = nub $ concat $ nodeKids <$> kidsnodes
      grandnodes = dagNode dag <$> grandkeys
  in zip grandkeys grandnodes

-- | Return all the associations (NodeKey, Node) of a given Dag.
dagNodeAssocs :: Dag n a -> [(NodeKey, Node n a)]
dagNodeAssocs = Map.toAscList . dagNodes

-- | A node may have several parents.
-- Depending on the alogrithm, we visit this node from each parent (`Multiple` visits)
-- or only once (`Single` visit)
data VisitFreq = Single                      -- Single visit to each node
               | Multiple                    -- Multiple visits to each node

-- | The DagAlgoData structure contains all the data we pass as working state
-- to the nodes during pre- or post order visits.
data DagAlgoData n a c = DagAlgoData {
        dsDag     :: !(Dag n a),              -- ^ The dag we are working on.
        dsVisited :: !(Set NodeKey),          -- ^ The nodes already visited.
        dsClState :: !c }                     -- ^ Some state to be used by the clients of the algorithms.

-- | Initialize the DagAlogData
dsInit :: Dag n a -> c -> DagAlgoData n a c
dsInit dag clstate = DagAlgoData {dsDag = dag, dsVisited = Set.empty, dsClState = clstate}

-- | Function to modify the `visited` field in a DagAlgoData record
modifyVisited :: MonadState (DagAlgoData n a u) m => VisitFreq -> (Set NodeKey -> Set NodeKey) -> m ()
modifyVisited Multiple _ = pure ()
modifyVisited Single f = modify' $ modVis f
  where
    modVis :: (Set NodeKey -> Set NodeKey) -> DagAlgoData n a c -> DagAlgoData n a c
    modVis f' dagState = dagState {dsVisited = f' (dsVisited dagState)}

-- | Function to modify the `dsDag` field in a DagAlgoData
modifyDag :: MonadState (DagAlgoData n a u) m =>  (Dag n a -> Dag n a) -> m ()
modifyDag f = modify' $ modDag f
  where
    modDag :: (Dag n a -> Dag n a) -> DagAlgoData n a c -> DagAlgoData n a c
    modDag f' dagState = dagState {dsDag = f' (dsDag dagState)}

-- | Return the Dag
getDag :: MonadState (DagAlgoData n a u) m => m (Dag n a)
getDag = gets dsDag

-- |`Store` a new Dag.
putDag :: MonadState (DagAlgoData n a u) m => Dag n a -> m ()
putDag dag = do
  dss <- get
  put $ dss {dsDag = dag}

-- | Return the client state.
getClState :: MonadState (DagAlgoData n a c) m => m c
getClState = gets dsClState

-- |`Store` a new client State.
putClState :: MonadState (DagAlgoData n a c) m => c -> m ()
putClState clstate = do
  dss <- get
  put $ dss {dsClState = clstate}

-- | Get the node with a given NodeKey.
getNode :: MonadState (DagAlgoData n a u) m => NodeKey -> m (Node n a)
getNode nodeKey = do
  dag <- gets dsDag
  pure $ dagNode dag nodeKey

-- ---------------------------------------------------------------------------
-- Node Functions
-- ---------------------------------------------------------------------------
-- | The type of the function that is called for a node visit during a traversal.
type NodeFunction n a c = (NodeKey, Node n a)               -- ^ The NodeKey and the node visited.
                          -> State (DagAlgoData n a c) ()   -- ^ The result of the NodeFunction to be passeed to the next node.

-- | A filter function called by the traversal function to check
-- whether the node must be visited.
-- When the predicated function returns True, the traversal function will visit the node.
type NodePredicate n a = Node n a  -- ^ Current Node
                        -> Bool

-- ---------------------------------------------------------------------------
-- Postorder processing:
-- ---------------------------------------------------------------------------

-- | Traversal: Visit all nodes in postorder (bottom up) sequence.
--  Execute the NodeFunction on every node.
postOrder :: VisitFreq -> NodeFunction n a c -> c -> Dag n a -> DagAlgoData n a c
postOrder visitFreq nodefun clstate dag =
  postOrderFilter visitFreq nodefun (const True) clstate dag

-- | Traversal: Visit nodes (and subnodes) in postorder (bottom up) sequence.
--  Only Nodes that fullfill the predicate function will be visited.
--  If a node doesn't fullfil the predicate function, all it's child nodes won't be visited.
--  Execute the NodeFunction on every visited node.
postOrderFilter :: forall n a c . VisitFreq -> NodeFunction n a c -> NodePredicate n a -> c -> Dag n a -> DagAlgoData n a c
postOrderFilter visitFreq nodefun pred ustat dag =
    execState (go `mapM` [dagStart dag]) (dsInit dag ustat)
  where
    go :: NodeKey -> State (DagAlgoData n a c) ()
    go  key = do
        -- Get the current node
        node <- getNode key
        if  pred node
            then do
                -- Conditionally add the current key to the set of visited keys
                modifyVisited visitFreq $ Set.insert key
                -- Get the direct children that haven't been visited
                visited <- gets dsVisited
                let subkeys = (nodeKids node) \\ (Set.toList visited)
                -- Recursion over the children
                _ <- go `mapM` subkeys
                -- Visit the current node
                _ <- nodefun (key, node)
                pure ()
            else do
                modifyVisited visitFreq $ Set.insert key

-- -------------------------------------------------------------------------------------------
-- Preorder processing
-- -------------------------------------------------------------------------------------------

-- | Traversal: Visit all nodes in preorder (top down) sequence.
--  Execute the NodeFunction on every node.
preOrder :: VisitFreq -> NodeFunction n a c -> c -> Dag n a -> DagAlgoData n a c
preOrder visitFreq nodefun clstate dag  = preOrderFilter visitFreq nodefun (const True) clstate dag

-- | Traversal: Visit nodes (and subnodes) in preorder (top down) sequence.
--  Only Nodes that fullfill the predicate function will be visited.
--  If a node doesn't fullfil the predicate function, all it's child nodes won't be visited.
--  Execute the NodeFunction on every visited node.
preOrderFilter :: forall n a c. VisitFreq -> NodeFunction n a c -> NodePredicate n a -> c -> Dag n a -> DagAlgoData n a c
preOrderFilter visitFreq nodefun pred clstate dag =
    execState (go `mapM` [dagStart dag]) (dsInit dag clstate)
  where
    go :: NodeKey -> State (DagAlgoData n a c) ()
    go key = do
        -- Get the current node
        node <- getNode key
        if  pred node
            then do
                -- Visit the current node
                _ <- nodefun (key, node)
                -- Conditionally add the current NodeKey to set of visited keys
                modifyVisited visitFreq $ Set.insert key
                visited <- gets dsVisited
                -- Get the direct children that haven't been visited
                let subkeys = (nodeKids node) \\ (Set.toList visited)
                -- Recursion over the children
                _ <- go `mapM` subkeys
                pure ()
            else do
                modifyVisited visitFreq $ Set.insert key
