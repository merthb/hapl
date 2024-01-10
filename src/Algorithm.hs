module Algorithm (Code(..),Response(..),mainAlgorithm,zipOnFunName,makePairs,runOnAll) where

import Graph
import Data.Graph.AStar
import Data.List hiding (union)
import Data.HashSet hiding (map, filter, null, foldr)

type ID = String
type Code = (ID, [CallGraph]) -- each graph gets a unique id so that the results will be matched to the right codes

type MatchNum = Int
type Response = (ID, ID, [(String, MatchNum)])

heuristic :: CallGraph -> CallGraph -> CallGraph -> Int -- h(n) = cost_of_subst * min(RG, RH) + cost_of_insdel * abs(RG - RH)
heuristic startg goalg g = min rgn rhn + abs (rgn - rhn) where 
    rgn = rg startg g
    rhn = rh goalg g

rh :: CallGraph -> CallGraph -> Int -- RH : the set of remaining vertices of the goalg that have not yet been inserted into g
rh goalg (Vertex f fs)
    | f `inG` goalg = sum $ map (rh goalg) fs
    | otherwise = 1 + sum (map (rh goalg) fs)

rg :: CallGraph -> CallGraph -> Int -- RG : the set of remaining vertices of the startg that are still in the partially transformed graph g
rg startg (Vertex f fs)
    | f `inG` startg = 1 + sum (map (rg startg) fs)
    | otherwise = sum $ map (rg startg) fs

neighbors :: CallGraph -> CallGraph -> HashSet CallGraph -- all edit operations that can be done to get closer to goalg
neighbors goalg@(Vertex f fs) curr@(Vertex g gs)
    | f == g && not (null (filter (\ (Vertex f _) -> and (map (not . startsWith f) gs)) fs)) = fromList (map (\ f -> insertEdge curr f) (filter (\ (Vertex f _) -> and (map (not . startsWith f) gs)) fs))
    | f == g && not (null (filter (\ (Vertex g _) -> and (map (not . startsWith g) fs)) gs)) = fromList (map (\ g -> deleteEdge curr g) (filter (\ (Vertex g _) -> and (map (not . startsWith g) fs)) gs))
    | f == g = foldr union empty (map (\ g -> foldr union empty (map (neighbors g) fs)) gs)
    | otherwise = Data.HashSet.insert (Vertex f gs) empty

cost :: CallGraph -> CallGraph -> Int -- cost of getting from graph g to graph h (every insertion, deletion or substitution costs 1)
cost g@(Vertex gf gs) h@(Vertex hf hs)
    | gf == hf = sum (map (uncurry cost) zipped) + (length sgs - length zipped) + (length shs - length zipped)
    | otherwise = 1 + sum (map (uncurry cost) zipped) + (length sgs - length zipped) + (length shs - length zipped)  where
        zipped = map (\(x, y, z) -> (y, z)) $ zipOnFunName sgs shs
        sgs = sort gs
        shs = sort hs

similarityScore :: CallGraph -> CallGraph -> CallGraph -> MatchNum -- get the match number from the original graph and the shortest path to the other one
similarityScore startg goalg path = round (100 * ((fromIntegral (maxc - abscost)) / (fromIntegral maxc))) where
    maxc = maxCost startg goalg
    abscost = cost startg path

maxCost :: CallGraph -> CallGraph -> Int
maxCost startg goalg = vg + eg + vh + eh where
    vg = vertNum startg
    vh = vertNum goalg
    eg = edgeNum startg
    eh = edgeNum goalg

mainAlgorithm :: CallGraph -> CallGraph -> MatchNum
mainAlgorithm startGraph goalGraph =
  case aStar (neighbors goalGraph) (\g h -> cost g h) (heuristic startGraph goalGraph) (== goalGraph) startGraph of
    Just [] -> 100
    Just path -> similarityScore startGraph goalGraph (last path)
    Nothing -> 0

makePairs :: [a] -> [(a, a)]
makePairs [] = []
makePairs (x:xs) = pairElement x xs ++ makePairs xs where
    pairElement :: a -> [a] -> [(a,a)]
    pairElement _ [] = []
    pairElement e (x:xs) = (e , x) : pairElement e xs

runOnAll :: [Code] -> [Response]
runOnAll = map (\ ((id1, g), (id2, h)) -> (id1, id2, map (\ (fn, x, y) -> (fn, mainAlgorithm x y)) (zipOnFunName g h))) . makePairs

zipOnFunName :: [CallGraph] -> [CallGraph] -> [(String, CallGraph, CallGraph)]
zipOnFunName [] _ = []
zipOnFunName _ [] = []
zipOnFunName (vf@(Vertex (F fn fty _) _):fs) gs
    | Just vg <- find (\ (Vertex (F n ty _) _) -> fn == n && fty == ty) gs = (dropWhile (=='.') fn, vf, vg) : zipOnFunName fs gs
    | Just vg <- find (\ (Vertex (F n ty _) _) -> fn == n) gs = (dropWhile (=='.') fn, vf, vg) : zipOnFunName fs gs
    | otherwise = zipOnFunName fs gs