module Algorithm where

import Graph
import SubISO
import qualified Algebra.Graph.AdjacencyMap as AdjMap
import qualified Data.Map as Map
import qualified Data.Set as Set
import Data.List

baseAlgorithm :: CallGraph -> CallGraph -> [Map.Map FName FName]
baseAlgorithm f g
    | AdjMap.vertexCount f < AdjMap.vertexCount g = findSubgraphIsomorphisms f g
    | otherwise = findSubgraphIsomorphisms g f

-- Generate subsets of a set with at least n elements
nonTrivialSubsets :: Ord a => Int -> Set.Set a -> [Set.Set a]
nonTrivialSubsets n vs = filter ((>= n) . Set.size) $ map Set.fromList $ subsequences (Set.toList vs)

-- Generate all subgraphs with at least n vertices
subgraphsWithAtLeastNVertices :: Ord a => Int -> AdjMap.AdjacencyMap a -> [AdjMap.AdjacencyMap a]
subgraphsWithAtLeastNVertices n g =
  let allVertices = AdjMap.vertexSet g
      subsets = nonTrivialSubsets n allVertices
      sortedSubsets = sortBy (\ s1 s2 -> compare (Set.size s1) (Set.size s2)) subsets
   in map (\subset -> AdjMap.induce (`Set.member` subset) g) subsets

searchMappings :: CallGraph -> CallGraph -> [[Map.Map FName FName]]
searchMappings f g
    | AdjMap.vertexCount f < AdjMap.vertexCount g = tracker (baseAlgorithm g) $ subgraphsWithAtLeastNVertices 3 f
    | otherwise = tracker (baseAlgorithm f) $ subgraphsWithAtLeastNVertices 3 g where
        tracker :: (CallGraph -> [Map.Map FName FName]) -> [CallGraph] -> [[Map.Map FName FName]]
        tracker _ [] = []
        tracker alg (graph:graphs)
            | null res = tracker alg graphs
            | otherwise = res : tracker alg (filter (not . (`AdjMap.isSubgraphOf` graph)) graphs) where
                res = alg graph
