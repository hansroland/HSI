{-# Language NamedFieldPuns #-}
{-# LANGUAGE FlexibleContexts #-}   -- used for mtl functions
{-# Language ScopedTypeVariables #-}

-- DAG Directed Acyclic Graph to be used for the Halfspace Intersection project.
module Data.Graph.Dag.Dag where

import Prelude hiding (pred)
import Data.Graph.Dag.Node ( NodeKey(..), Node(Node, nodeKids) )

import Data.EnumMap.Strict (EnumMap)
import qualified Data.EnumMap.Strict as Map
import Data.Set (Set)
import qualified Data.Set as Set

import Control.Monad.State.Strict
import Data.Maybe ( fromMaybe )
import Data.List ((\\), nub)

type NodeMap a = EnumMap NodeKey (Node a)
-- --------------------------------------------------------------
-- Dag: Directed Acyclic Graphs
-- --------------------------------------------------------------
-- The Dag contains the index of the start node and a map of all nodes
data Dag n = Dag {start :: !NodeKey, dagMap :: !(NodeMap n)}

instance Show n => Show (Dag n) where                   -- TODO: improve this code
    show Dag {start, dagMap} = concat
      ("DAG start = " : show  start : nl : map showassoc ( Map.toAscList dagMap))
      where
        showassoc :: Show n => (NodeKey, Node n) -> String
        showassoc (key, nd) = show key ++ " => " ++ show nd ++ nl
        nl = ['\n']

-- Return the node of a key.
dagNode :: Dag n -> NodeKey -> Node n
dagNode dag key = (dagMap dag) Map.! key   -- TODO: Comment on missing keys

-- Return the start node
dagStartNode :: Dag n -> Node n
dagStartNode dag = dagNode dag $ start dag

-- Initialize a DAG with the root node
dagInit :: NodeKey -> Dag n
dagInit (NodeKey key) = Dag {start = (NodeKey key), dagMap = Map.empty}

-- Create and insert a new node with a given key
dagCreateNode :: NodeKey -> [NodeKey] -> n -> Dag n -> Dag n
dagCreateNode key subs pl dag =
    dag { dagMap = Map.insert key (Node subs pl) (dagMap dag)}

-- Insert a new Node and return the new key
dagInsertNode :: Node n -> Dag n -> (NodeKey, Dag n)
dagInsertNode node dag =
    let maxKey = fromMaybe 0 $ fst <$> (Map.lookupMax $ dagMap dag)
        hsKey = (1 + maxKey)
        newmap = Map.insert hsKey node $ dagMap dag
    in  (hsKey, dag{dagMap = newmap})

-- Update a node with a given key (If it doesn't exist, it will be added)
dagUpdateNode :: Dag n -> NodeKey -> Node n -> Dag n
dagUpdateNode dag key node =
    dag {dagMap = Map.insert key node $ dagMap dag}

-- Get the keys and nodes of grandchildren of a node
dagGrandNodes :: Dag n -> Node n ->[(NodeKey, Node n)]
dagGrandNodes dag node =
  let kidskeys = nodeKids node
      kidsnodes = dagNode dag <$> kidskeys
      grandkeys = nub $ concat $ nodeKids <$> kidsnodes
      grandnodes = dagNode dag <$> grandkeys
  in zip grandkeys grandnodes

-- Return the associations (NodeKey, Node) of all the nodes
dagNodeAssocs :: Dag n -> [(NodeKey, Node n)]
dagNodeAssocs = Map.toAscList . dagMap

-- ---------------------------------------------------------------------------
-- DagAlgoData
-- ---------------------------------------------------------------------------
data DagAlgoData n u = DagAlgoData {dsDag :: Dag n, dsDups :: Set NodeKey, dsUstate :: u}

dsInit :: Dag n -> u -> DagAlgoData n u
dsInit dag ustate = DagAlgoData {dsDag = dag, dsDups = Set.empty, dsUstate = ustate}

-- Function to modify the `dsDups` field in a DagAlgoData record
-- TODO: improve subfunction
modifyDups :: MonadState (DagAlgoData n u) m => (Set NodeKey -> Set NodeKey) -> m ()
modifyDups f = modify' $ modDups f
  where
    modDups :: (Set NodeKey -> Set NodeKey) -> DagAlgoData n u -> DagAlgoData n u
    modDups f' dagState = dagState {dsDups = f' (dsDups dagState)}

-- Function to modify the `dsDag` field in a DagAlgoData
modifyDag :: MonadState (DagAlgoData n u) m =>  (Dag n -> Dag n) -> m ()
modifyDag f = modify' $ modDag f
  where
    modDag :: (Dag n -> Dag n) -> DagAlgoData n u -> DagAlgoData n u
    modDag f' dagState = dagState {dsDag = f' (dsDag dagState)}

-- Return the Dag
getDag :: MonadState (DagAlgoData n u) m => m (Dag n)
getDag = gets dsDag

-- `Store` a new Dag
putDag :: MonadState (DagAlgoData n u) m => Dag n -> m ()
putDag dag = do
  dss <- get
  put $ dss {dsDag = dag}

-- Return the User State
getUstate :: MonadState (DagAlgoData n u) m => m u
getUstate = gets dsUstate

-- `Store` a new User State
putUstate :: MonadState (DagAlgoData n u) m => u -> m ()
putUstate ustate = do
  dss <- get
  put $ dss {dsUstate = ustate}

getNode :: MonadState (DagAlgoData n u) m => NodeKey -> m (Node n)
getNode nodeKey = do
  dag <- gets dsDag
  return $ dagNode dag nodeKey

-- ---------------------------------------------------------------------------
-- Node Functions
-- ---------------------------------------------------------------------------
type NodeFunction n u = (NodeKey, Node n) -> State (DagAlgoData n u) ()
type NodePredicate n = Node n -> Bool

-- ---------------------------------------------------------------------------
-- Postorder processing
-- ---------------------------------------------------------------------------

-- Process postorder. Single visits to each node.
postOrderSingle :: NodeFunction n u -> u -> Dag n -> Dag n
postOrderSingle nodefun ustate dag =
  postOrderSingleFilter nodefun (const True) ustate dag

-- Process postorder. Single visits to each node. Filter function to skip nodes
postOrderSingleFilter :: forall n u . NodeFunction n u -> NodePredicate n -> u -> Dag n -> Dag n
postOrderSingleFilter nodefun pred ustat dag =
    dsDag $ execState (go `mapM` [start dag]) (dsInit dag ustat)
  where
    go :: NodeKey -> State (DagAlgoData n u) ()
    go  key = do
        -- Get the current node
        node <- getNode key
        if  pred node
            then do
                -- Add the current key to the processed keys
                modifyDups $ Set.insert key
                -- Get the direct children that haven't been processed
                dups <- gets dsDups
                let subkeys = (nodeKids node) \\ (Set.toList dups)
                -- Recursion over the children
                _ <- go `mapM` subkeys
                -- Process the current node
                _ <- nodefun (key, node)
                pure ()
            else do
                modifyDups $ Set.insert key


-- Process postorder. Multiple visits to each node.
postOrderMultiple :: NodeFunction n u -> u -> Dag n -> Dag n
postOrderMultiple nodefun ustate dag =
  postOrderMultipleFilter nodefun (const True) ustate dag

-- Process postorder. Multiple visits to each node. Filter function to skip nodes
postOrderMultipleFilter :: forall n u . NodeFunction n u -> NodePredicate n -> u -> Dag n -> Dag n
postOrderMultipleFilter nodefun pred ustat dag =
    dsDag $ execState (go `mapM` [start dag]) (dsInit dag ustat)
  where
    go ::  NodeKey -> State (DagAlgoData n u) ()
    go key = do
        -- Get the current node
        node <- getNode key
        if  pred node
            then do
                -- Recursion over the children
                _ <- go `mapM` (nodeKids node)
                -- Process the current node
                _ <- nodefun (key, node) -- ustate1 dag1
                pure ()
            else pure ()

-- -------------------------------------------------------------------------------------------
-- Preorder processing
-- -------------------------------------------------------------------------------------------

-- Process preorder. Single visits to each node.
preOrderSingle :: NodeFunction n u -> u -> Dag n -> Dag n
preOrderSingle nodefun ustate dag  = preOrderSingleFilter nodefun (const True) ustate dag

-- Process preorder. Single visits to each node. Filter function to skip nodes
preOrderSingleFilter :: forall n u. NodeFunction n u-> NodePredicate n -> u -> Dag n -> Dag n
preOrderSingleFilter nodefun pred ustate dag =
    dsDag $ execState (go `mapM` [start dag]) (dsInit dag ustate)
  where
    go :: NodeKey -> State (DagAlgoData n u) ()
    go key = do
        -- Get the current node
        node <- getNode key
        if  pred node
            then do
                -- Process the current node
                _ <- nodefun (key, node)
                -- Add the current NodeKey to the set of the processed
                modifyDups $ Set.insert key
                dups <- gets dsDups
                -- Get the direct children that haven't been processed
                let subkeys = (nodeKids node) \\ (Set.toList dups)
                -- Recursion over the children
                _ <- go `mapM` subkeys
                pure ()
            else do
                modifyDups $ Set.insert key

-- Process preorder. Multiple visits to each node. Filter function to skip nodes
preOrderMultipleFilter :: forall n u . NodeFunction n u -> NodePredicate n -> u -> Dag n -> Dag n
preOrderMultipleFilter nodefun pred param dag =
    dsDag $ execState (go `mapM` [start dag]) (dsInit dag param)
  where
    go :: NodeKey -> State (DagAlgoData n u) ()
    go  key = do
        -- Get the current node
        node <- getNode key
        if  pred node
            then do
                -- Process the current node
                _ <- nodefun (key, node) -- param dag1
                -- Recursion over the children
                _ <- go `mapM` (nodeKids node)
                pure ()
            else pure ()
