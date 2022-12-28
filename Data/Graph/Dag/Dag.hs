{-# Language NamedFieldPuns #-}
{-# LANGUAGE FlexibleContexts #-}   -- used for mtl functions
{-# Language ScopedTypeVariables #-}

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

-- Dag: Directed Acyclic Graphs
-- Store the Dag in an EnumMap.
type NodeMap n a = EnumMap NodeKey (Node n a)

-- The Dag contains the index of the start node and a map of all nodes
data Dag n a = Dag
      { dagStart :: !NodeKey,
        dagNodes :: !(NodeMap n a)
      }

instance (Show n, Show a) => Show (Dag n a) where
    show Dag {dagStart, dagNodes} = concat
      ("DAG start = " : show  dagStart : nl : map showassoc ( Map.toAscList dagNodes))
      where
        showassoc :: (Show n, Show a) => (NodeKey, Node n a) -> String
        showassoc (key, nd) = show key ++ " => " ++ show nd
            ++ "  " ++ show (nodeAttr nd) ++ nl
        nl = ['\n']

-- Return the node of a key.
-- We don't check for failure. Every NodeKey we use, MUST exist in the map!
dagNode :: Dag n a -> NodeKey -> Node n a
dagNode dag key = (dagNodes dag) Map.! key

-- Return the start node
dagStartNode :: Dag n a -> Node n a
dagStartNode dag = dagNode dag $ dagStart dag

-- Initialize a DAG with the root node
dagInit :: NodeKey -> Dag n a
dagInit (NodeKey key) = Dag {dagStart = (NodeKey key), dagNodes = Map.empty}

-- Create new node for a given key and insert it into the dag
dagCreateNode :: NodeKey -> [NodeKey] -> n -> a -> Dag n a -> Dag n a
dagCreateNode key subs nd attr dag =
    dag { dagNodes = Map.insert key (Node subs nd attr) (dagNodes dag)}

-- Insert a new Node insert it into the dag. Return the new key
dagInsertNode :: Node n a -> Dag n a -> (NodeKey, Dag n a)
dagInsertNode node dag =
    let maxKey = fromMaybe 0 $ fst <$> (Map.lookupMax $ dagNodes dag)
        hsKey = (1 + maxKey)
        newnodes = Map.insert hsKey node $ dagNodes dag
    in  (hsKey, dag{dagNodes = newnodes})

-- Update a node with a given key (If it doesn't exist, it will be added)
dagUpdateNode :: Dag n a -> NodeKey -> Node n a -> Dag n a
dagUpdateNode dag key node =
    dag {dagNodes = Map.insert key node $ dagNodes dag}

-- Get the keys and nodes of grandchildren of a node
dagGrandNodes :: Dag n a -> Node n a ->[(NodeKey, Node n a)]
dagGrandNodes dag node =
  let kidskeys = nodeKids node
      kidsnodes = dagNode dag <$> kidskeys
      grandkeys = nub $ concat $ nodeKids <$> kidsnodes
      grandnodes = dagNode dag <$> grandkeys
  in zip grandkeys grandnodes

-- Return the associations (NodeKey, Node) of all the nodes
dagNodeAssocs :: Dag n a -> [(NodeKey, Node n a)]
dagNodeAssocs = Map.toAscList . dagNodes


-- DagAlgoData
-- This structure contains all the data we pass as working state
-- to the nodes during pre- or post order walks
data DagAlgoData n a c = DagAlgoData {
        dsDag     :: !(Dag n a),              -- The dag we are working on
        dsVisited :: !(Set NodeKey),          -- The nodes already visited
        dsClState :: !c }                     -- Some state to be used by the clients of the alogithms.

-- Visiting Frequency.
-- A node may have several parents.
-- Depending on the alogrithm, we visit this node from each parent (`Multiple` visits)
-- ore only once (`Single` visit)
data VisitFreq = Single                      -- Single visit to each node
               | Multiple                    -- Multiple visits to each node

-- Initialize the DagAlogData
dsInit :: Dag n a -> c -> DagAlgoData n a c
dsInit dag clstate = DagAlgoData {dsDag = dag, dsVisited = Set.empty, dsClState = clstate}

-- Function to modify the `visited` field in a DagAlgoData record
modifyVisited :: MonadState (DagAlgoData n a u) m => VisitFreq -> (Set NodeKey -> Set NodeKey) -> m ()
modifyVisited Multiple _ = pure ()
modifyVisited Single f = modify' $ modVis f
  where
    modVis :: (Set NodeKey -> Set NodeKey) -> DagAlgoData n a c -> DagAlgoData n a c
    modVis f' dagState = dagState {dsVisited = f' (dsVisited dagState)}

-- Function to modify the `dsDag` field in a DagAlgoData
modifyDag :: MonadState (DagAlgoData n a u) m =>  (Dag n a -> Dag n a) -> m ()
modifyDag f = modify' $ modDag f
  where
    modDag :: (Dag n a -> Dag n a) -> DagAlgoData n a c -> DagAlgoData n a c
    modDag f' dagState = dagState {dsDag = f' (dsDag dagState)}

-- Return the Dag
getDag :: MonadState (DagAlgoData n a u) m => m (Dag n a)
getDag = gets dsDag

-- `Store` a new Dag
putDag :: MonadState (DagAlgoData n a u) m => Dag n a -> m ()
putDag dag = do
  dss <- get
  put $ dss {dsDag = dag}

-- Return the client State
getClState :: MonadState (DagAlgoData n a c) m => m c
getClState = gets dsClState

-- `Store` a new client State
putClState :: MonadState (DagAlgoData n a c) m => c -> m ()
putClState clstate = do
  dss <- get
  put $ dss {dsClState = clstate}

getNode :: MonadState (DagAlgoData n a u) m => NodeKey -> m (Node n a)
getNode nodeKey = do
  dag <- gets dsDag
  return $ dagNode dag nodeKey

-- ---------------------------------------------------------------------------
-- Node Functions
-- ---------------------------------------------------------------------------
type NodeFunction n a c = (NodeKey, Node n a) -> State (DagAlgoData n a c) ()
type NodePredicate n a = Node n a -> Bool

-- ---------------------------------------------------------------------------
-- Postorder processing:
-- ---------------------------------------------------------------------------

-- Process postorder without filtering
postOrder :: VisitFreq -> NodeFunction n a c -> c -> Dag n a -> Dag n a
postOrder visitFreq nodefun clstate dag =
  postOrderFilter visitFreq nodefun (const True) clstate dag

-- Process postorder with a filter function to skip nodes
postOrderFilter :: forall n a c . VisitFreq -> NodeFunction n a c -> NodePredicate n a -> c -> Dag n a -> Dag n a
postOrderFilter visitFreq nodefun pred ustat dag =
    dsDag $ execState (go `mapM` [dagStart dag]) (dsInit dag ustat)
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

-- Process preorder without filtering
preOrder :: VisitFreq -> NodeFunction n a c -> c -> Dag n a -> Dag n a
preOrder visitFreq nodefun clstate dag  = preOrderFilter visitFreq nodefun (const True) clstate dag

-- Process preorder with a filter function to skip nodes
preOrderFilter :: forall n a c. VisitFreq -> NodeFunction n a c -> NodePredicate n a -> c -> Dag n a -> Dag n a
preOrderFilter visitFreq nodefun pred clstate dag =
    dsDag $ execState (go `mapM` [dagStart dag]) (dsInit dag clstate)
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
