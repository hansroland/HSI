-- |
-- Module      : Data.Graph.Dag
-- Copyright   : (c) Roland Senn 2022-2023
-- License     : BSD-style
--
-- A little library to process DAG (Directed acyclic graph) structures.
--
-- It has been specially designed to support the needs for the Hasse-diagrams in the HSI (Halfspace Intersection) project.
module Data.Graph.Dag (
        -- * Dag - Directed Acyclic Graph
        NodeMap,
        Dag(..),
        -- ** Operations on Dags
        dagInit,
        dagNode,
        dagStartNode,
        dagNodeAssocs,
        dagCreateNode,
        dagGrandNodes,
        dagUpdateNode,
        dagInsertNode,
        -- * Node
        Node (..),
         -- * NodeKey
        NodeKey,
       -- ** Operations on Nodes
        nodeAddKey,
        nodeAddKeys,
        nodeSetData,
        -- * Algorithms
        -- ** Visiting Frequeny
        VisitFreq(..),
        -- ** DagAlgoData
        DagAlgoData(),
        getClState,
        putClState,
        getDag,
        putDag,
        dsDag,
        dsClState,
        -- ** NodeFunction
        NodeFunction,
        -- ** NodePredicate
        NodePredicate,
        -- ** Traversals
        postOrder,
        postOrderFilter,
        preOrder,
        preOrderFilter
)
where

import Data.Graph.Dag.Dag
import Data.Graph.Dag.Node
