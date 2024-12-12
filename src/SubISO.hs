module SubISO where

import Algebra.Graph.AdjacencyMap
import qualified Data.Set as Set
import Data.Set (Set)
import Data.Map (Map)
import qualified Data.Map as Map

-- Get the outgoing neighbors for a given vertex in a directed graph
outNeighbors :: Ord a => AdjacencyMap a -> a -> Set a
outNeighbors g v = postSet v g

-- Get the incoming neighbors for a given vertex in a directed graph
inNeighbors :: Ord a => AdjacencyMap a -> a -> Set a
inNeighbors g v = preSet v g

-- Generate a set of candidate mappings for a given pattern vertex based on both in-degree and out-degree constraints
generateCandidates :: (Ord a, Ord b)
                   => AdjacencyMap a -- ^ Pattern graph
                   -> AdjacencyMap b -- ^ Target graph
                   -> a              -- ^ Pattern vertex
                   -> [b]            -- ^ List of candidate target vertices
generateCandidates pattern target u =
  filter (\v -> Set.size (outNeighbors target v) >= Set.size (outNeighbors pattern u) &&
                Set.size (inNeighbors target v) >= Set.size (inNeighbors pattern u))
         (vertexList target)

-- Check if a candidate mapping (u -> v) can be added to the current mapping
isJoinable :: (Ord a, Ord b)
           => AdjacencyMap a -- ^ Pattern graph
           -> AdjacencyMap b -- ^ Target graph
           -> Map a b        -- ^ Current mapping
           -> a              -- ^ Pattern vertex
           -> b              -- ^ Target vertex
           -> Bool
isJoinable pattern target mapping u v =
  isConsistent pattern target mapping u v &&
  not (v `elem` Map.elems mapping) -- Ensure v is not already mapped

-- Check if a mapping is consistent with the structure of both graphs
isConsistent :: (Ord a, Ord b)
             => AdjacencyMap a -- ^ Pattern graph
             -> AdjacencyMap b -- ^ Target graph
             -> Map a b        -- ^ Current mapping
             -> a              -- ^ Pattern vertex
             -> b              -- ^ Target vertex
             -> Bool
isConsistent pattern target mapping u v =
  all (\u' -> case Map.lookup u' mapping of
                 Just v' -> hasEdge v v' target -- Check if there is a corresponding directed edge in the target graph
                 Nothing -> True                -- If not mapped, assume it's consistent
      ) (Set.toList $ outNeighbors pattern u) &&
  all (\u' -> case Map.lookup u' mapping of
                 Just v' -> hasEdge v' v target -- Reverse edge check for directed graphs
                 Nothing -> True
      ) (Set.toList $ inNeighbors pattern u)

-- Select a pivot vertex from the pattern graph to maximize constraint on future mappings
pivotVertexSelection :: (Ord a)
                     => AdjacencyMap a -- ^ Pattern graph
                     -> Map a b        -- ^ Current mapping
                     -> a              -- ^ Pivot vertex
pivotVertexSelection pattern mapping =
  head $ filter (`Map.notMember` mapping) (vertexList pattern)

-- Recursive subgraph search using backtracking
subgraphSearch :: (Ord a, Ord b)
               => AdjacencyMap a -- ^ Pattern graph
               -> AdjacencyMap b -- ^ Target graph
               -> Map a b        -- ^ Current mapping
               -> [Map a b]      -- ^ List of valid mappings (solutions)
subgraphSearch pattern target currentMapping
  | Map.size currentMapping == length (vertexList pattern) = [currentMapping] -- Complete mapping found
  | otherwise = concatMap extendMapping candidates
  where
    -- Select a pivot vertex using pivot selection
    u = pivotVertexSelection pattern currentMapping

    -- Generate candidate target vertices for the pivot vertex
    candidates = generateCandidates pattern target u

    -- Try extending the mapping
    extendMapping v =
      if isJoinable pattern target currentMapping u v
        then subgraphSearch pattern target (Map.insert u v currentMapping)
        else []

-- Entry point to find all valid subgraph isomorphisms
findSubgraphIsomorphisms :: (Ord a, Ord b)
                         => AdjacencyMap a -- ^ Pattern graph
                         -> AdjacencyMap b -- ^ Target graph
                         -> [Map a b]      -- ^ List of valid mappings
findSubgraphIsomorphisms pattern target =
  subgraphSearch pattern target Map.empty

-- Example usage
main :: IO ()
main = do
    let patternGraph = edge 1 2 <> edge 2 3
        targetGraph = edge 1 2 <> edge 1 3 <> edge 2 4
        largerGraph = edge 1 2 <> edge 2 3 <> edge 3 4 <> edge 4 5 <> edge 1 5
        starGraph = edges [(1, 2), (1, 3), (1, 4)]
        pathGraph = edges [(1, 2), (2, 3), (3, 4)]
    print $ findSubgraphIsomorphisms patternGraph targetGraph
    print $ findSubgraphIsomorphisms patternGraph largerGraph
    print $ findSubgraphIsomorphisms targetGraph largerGraph
    print $ findSubgraphIsomorphisms starGraph pathGraph
