{-# LANGUAGE TemplateHaskell #-}

module Pact.Analyze.Graph where

import qualified Algebra.Graph             as Alga
import           Control.Lens              (makeLenses)
--import qualified Data.Map                   as Map
import           Data.Map.Strict           (Map)

import           Pact.Analyze.Types
import           Pact.Analyze.Util

--
-- TODO: any data types that are strictly only used for graph construction?
--       move them here!
--
--       Path? Vertex? Edge?
--

-- * Interface

class Monad m => MonadGraph e m where
  emit :: e -> m ()


-- * Implementation

data GraphState e
  = GraphState
    { _tsGraph         :: Alga.Graph Vertex
      -- ^ The execution graph we've built so far. This is expanded upon as we
      -- translate an entire function.
    , _tsPathHead      :: Vertex
      -- ^ The "latest" vertex/current path of the graph. This starts out as
      -- the single initial vertex. it splits into two if we hit a conditional,
      -- and rejoins afterwards.
    , _tsNextVertex    :: Vertex
    , _tsEdgeEvents    :: Map Edge [e]
      -- ^ Events added to each new 'Edge' upon creating a new 'Vertex' which
      -- closes/completes the 'Edge'.
    , _tsPendingEvents :: SnocList e
      -- ^ Events being accumulated until the creation of the next 'Vertex'.
    , _tsCurrentPath   :: Path
      -- ^ Path to be associated with the 'Edge' formed by the creation of the
      -- next 'Vertex'.
    , _tsPathEdges     :: Map Path [Edge]
      -- ^ Graph edges corresponding to a given execution "path".
      --
      -- 'TraceSubpathStart's are emitted once for each path: at the start of
      -- an execution trace, and at the beginning of either side of a
      -- conditional.  After a conditional, we resume the path from before the
      -- conditional.  Either side of a conditional will contain a minimum of
      -- two edges: splitting away from the other branch, and then rejoining
      -- back to the other branch at the join point. The following program:
      --
      --     (defun test ()
      --       (if true 1 2))
      --
      -- will result in the following diagram, with six total edges (where /,
      -- \, and - are edges):
      --        .
      --       / \
      --     ->   ->
      --       \./
      --
      -- The initial edge leading into the conditional, two for each branch,
      -- and a final edge after the two branches have rejoined one another. We
      -- must have two edges for each branch so that we can unambiguously talk
      -- about either branch in our graph representation, where we only one
      -- permit one edge in a given direction between two vertices.
      --
      -- Also note that in the presence of nested conditionals, these
      -- "branch-out" and "rejoin" edges will not be contiguous in the graph on
      -- the side of the outer conditional which contains the nested
      -- conditional:
      --       ......
      --     _/  .   \_
      --      \ / \ _/
      --        \./
      --
      -- We track all of the edges for each path so that we can determine the
      -- subset of edges on the graph that form the upper bound for the edges
      -- that are reached during a particular execution trace. We say "upper
      -- bound" here because some traces will not execute entirely to the end
      -- of the program due to the use of e.g. @enforce@ and @enforce-keyset@.
      --
      -- There's one more scenario where we start subpaths: for each of the
      -- "cases" of an @enforce-one@. Here's an example with three cases:
      --
      --     (enforce-one [case-1 case-2 case-3])
      --
      --     \____        <- case-1 runs, always
      --      \___ ._     <- case-2 runs if case-1 fails
      --       \__        <- case-3 runs if case-2 fails
      --
      -- The \ edges correspond to the execution of each case. The _ edges
      -- correspond to successful exit early due to the lack of a failure.
      -- These three "success" edges all join together at the same vertex.
    }

makeLenses ''GraphState
