{-# Language NamedFieldPuns #-}
{-# LANGUAGE FlexibleContexts #-}   -- used for mtl functions
{-# Language ScopedTypeVariables #-}

-- DAG Directed Acyclic Graph to be used for the Halfspace Intersection project.
module Data.Graph.Dag.Dag where

import Prelude hiding (pred)
import Data.Graph.Dag.Node ( NodeKey(..), Node(Node, nodeKids, nodeAttr) )

import Data.EnumMap.Strict (EnumMap)
import qualified Data.EnumMap.Strict as Map
import Data.Set (Set)
import qualified Data.Set as Set

import Control.Monad.State.Strict
import Data.Maybe ( fromMaybe )
import Data.List ((\\), nub)

-- --------------------------------------------------------------
-- Dag: Directed Acyclic Graphs
-- --------------------------------------------------------------
type NodeMap n a = EnumMap NodeKey (Node n a)
-- The Dag contains the index of the start node and a map of all nodes
data Dag n a = Dag
      { dagStart :: !NodeKey,
        dagNodes :: !(NodeMap n a)
      }

instance (Show n, Show a) => Show (Dag n a) where                   -- TODO: improve this code
    show Dag {dagStart, dagNodes} = concat
      ("DAG start = " : show  dagStart : nl : map showassoc ( Map.toAscList dagNodes))
      where
        showassoc :: (Show n, Show a) => (NodeKey, Node n a) -> String
        showassoc (key, nd) = show key ++ " => " ++ show nd
            ++ "  " ++ show (nodeAttr nd) ++ nl
        nl = ['\n']

-- Return the node of a key.
dagNode :: Dag n a -> NodeKey -> Node n a
dagNode dag key = (dagNodes dag) Map.! key   -- TODO: Comment on missing keys

-- Return the start node
dagStartNode :: Dag n a -> Node n a
dagStartNode dag = dagNode dag $ dagStart dag

-- Initialize a DAG with the root node
dagInit :: NodeKey -> Dag n a
dagInit (NodeKey key) = Dag {dagStart = (NodeKey key), dagNodes = Map.empty}

-- Create and insert  for a given key a new node with its attributes
dagCreateNode :: NodeKey -> [NodeKey] -> n -> a -> Dag n a -> Dag n a
dagCreateNode key subs nd attr dag =
    dag { dagNodes = Map.insert key (Node subs nd attr) (dagNodes dag)}

-- Insert a new Node and return the new key
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

-- ---------------------------------------------------------------------------
-- DagAlgoData
-- ---------------------------------------------------------------------------
data DagAlgoData n a u = DagAlgoData {dsDag :: !(Dag n a), dsDups :: !(Set NodeKey), dsUstate :: !u}

data VisitFreq = Single                      -- Single visit to each node
               | Multiple                    -- Multiple visits to each node

dsInit :: Dag n a -> u -> DagAlgoData n a u
dsInit dag ustate = DagAlgoData {dsDag = dag, dsDups = Set.empty, dsUstate = ustate}

-- Function to modify the `dsDups` field in a DagAlgoData record
-- TODO: improve subfunction
modifyDupsCond :: MonadState (DagAlgoData n a u) m => VisitFreq -> (Set NodeKey -> Set NodeKey) -> m ()
modifyDupsCond Multiple _ = pure ()
modifyDupsCond Single f = modify' $ modDups f
  where
    modDups :: (Set NodeKey -> Set NodeKey) -> DagAlgoData n a u -> DagAlgoData n a u
    modDups f' dagState = dagState {dsDups = f' (dsDups dagState)}

-- Function to modify the `dsDag` field in a DagAlgoData
modifyDag :: MonadState (DagAlgoData n a u) m =>  (Dag n a -> Dag n a) -> m ()
modifyDag f = modify' $ modDag f
  where
    modDag :: (Dag n a -> Dag n a) -> DagAlgoData n a u -> DagAlgoData n a u
    modDag f' dagState = dagState {dsDag = f' (dsDag dagState)}

-- Return the Dag
getDag :: MonadState (DagAlgoData n a u) m => m (Dag n a)
getDag = gets dsDag

-- `Store` a new Dag
putDag :: MonadState (DagAlgoData n a u) m => Dag n a -> m ()
putDag dag = do
  dss <- get
  put $ dss {dsDag = dag}

-- Return the User State
getUstate :: MonadState (DagAlgoData n a u) m => m u
getUstate = gets dsUstate

-- `Store` a new User State
putUstate :: MonadState (DagAlgoData n a u) m => u -> m ()
putUstate ustate = do
  dss <- get
  put $ dss {dsUstate = ustate}

getNode :: MonadState (DagAlgoData n a u) m => NodeKey -> m (Node n a)
getNode nodeKey = do
  dag <- gets dsDag
  return $ dagNode dag nodeKey

-- ---------------------------------------------------------------------------
-- Node Functions
-- ---------------------------------------------------------------------------
type NodeFunction n a u = (NodeKey, Node n a) -> State (DagAlgoData n a u) ()
type NodePredicate n a = Node n a -> Bool

-- ---------------------------------------------------------------------------
-- Postorder processing:
-- ---------------------------------------------------------------------------

-- Process postorder without filtering
postOrder :: VisitFreq -> NodeFunction n a u -> u -> Dag n a -> Dag n a
postOrder visitFreq nodefun ustate dag =
  postOrderFilter visitFreq nodefun (const True) ustate dag

-- Process postorder with a filter function to skip nodes
postOrderFilter :: forall n a u . VisitFreq -> NodeFunction n a u -> NodePredicate n a -> u -> Dag n a -> Dag n a
postOrderFilter visitFreq nodefun pred ustat dag =
    dsDag $ execState (go `mapM` [dagStart dag]) (dsInit dag ustat)
  where
    go :: NodeKey -> State (DagAlgoData n a u) ()
    go  key = do
        -- Get the current node
        node <- getNode key
        if  pred node
            then do
                -- Conditionally add the current key to the processed keys
                modifyDupsCond visitFreq $ Set.insert key
                -- Get the direct children that haven't been processed
                dups <- gets dsDups
                let subkeys = (nodeKids node) \\ (Set.toList dups)
                -- Recursion over the children
                _ <- go `mapM` subkeys
                -- Process the current node
                _ <- nodefun (key, node)
                pure ()
            else do
                modifyDupsCond visitFreq $ Set.insert key

-- -------------------------------------------------------------------------------------------
-- Preorder processing
-- -------------------------------------------------------------------------------------------

-- Process preorder without filtering
preOrder :: VisitFreq -> NodeFunction n a u -> u -> Dag n a -> Dag n a
preOrder visitFreq nodefun ustate dag  = preOrderFilter visitFreq nodefun (const True) ustate dag

-- Process preorder with a filter function to skip nodes
preOrderFilter :: forall n a u. VisitFreq -> NodeFunction n a u-> NodePredicate n a -> u -> Dag n a -> Dag n a
preOrderFilter visitFreq nodefun pred ustate dag =
    dsDag $ execState (go `mapM` [dagStart dag]) (dsInit dag ustate)
  where
    go :: NodeKey -> State (DagAlgoData n a u) ()
    go key = do
        -- Get the current node
        node <- getNode key
        if  pred node
            then do
                -- Process the current node
                _ <- nodefun (key, node)
                -- Conditionally add the current NodeKey to processed keys
                modifyDupsCond visitFreq $ Set.insert key
                dups <- gets dsDups
                -- Get the direct children that haven't been processed
                let subkeys = (nodeKids node) \\ (Set.toList dups)
                -- Recursion over the children
                _ <- go `mapM` subkeys
                pure ()
            else do
                modifyDupsCond visitFreq $ Set.insert key
