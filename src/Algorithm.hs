module Algorithm (f,ID,Code,MatchNum,Response,heuristic,neighbors,cost,maxCost,similarityScore,mainAlgorithm,runOnAll,makePairs,zipOnFunName) where

import Graph
import Data.Graph.AStar
import Data.List
import qualified Data.HashSet as HS
import Language.Haskell.Exts.Simple

type ID = String -- the id of a code is it's path to the file containing the code
type Code = (ID, [CallGraph]) -- each code gets it's unique id so that the results will be matched to the right codes, plus it has the call graph(s) of the code

type MatchNum = Int -- the percentage of the match
type Response = (ID, ID, [(String, MatchNum)]) -- the two code id that were matched, and a list of the checked functions and their percentage

-- the heuristic function of the a* algorithm
heuristic :: CallGraph -> CallGraph -> CallGraph -> Int -- h(n) = cost_of_subst * min(RG, RH) + cost_of_insdel * abs(RG - RH)
heuristic startg goalg g = (min rgn rhn) + 10 * (abs (rgn - rhn)) where 
    rgn = rg startg g
    rhn = rh goalg g

-- the set of remaining vertices of the goalg that have not yet been inserted into the partially transformed graph 
rh :: CallGraph -> CallGraph -> Int
rh goalg (Vertex f fs)
    | f `inG` goalg = HS.foldr (+) 0 $ HS.map (rh goalg) fs
    | otherwise = 1 + (HS.foldr (+) 0 $ HS.map (rh goalg) fs)

-- the set of remaining vertices of the startg that are still in the partially transformed graph 
rg :: CallGraph -> CallGraph -> Int
rg startg (Vertex f fs)
    | f `inG` startg = 1 + (HS.foldr (+) 0 $ HS.map (rg startg) fs)
    | otherwise = HS.foldr (+) 0 $ HS.map (rg startg) fs

-- the neighbor function of the a* algorithm
-- the possible next steps to get from the first graph to the second
neighbors :: CallGraph -> CallGraph -> HS.HashSet CallGraph
neighbors goalg@(Vertex f fs) curr@(Vertex g gs)
    | fg && nd1 && nd2 = magic (HS.toList diff2) (HS.toList diff1) gs g
    | fg && nd1 = HS.map (\ f -> insertEdge curr f) diff1
    | fg && nd2 = HS.map (\ g -> deleteEdge curr g) diff2
    | fg = foldr HS.union HS.empty (map (\ (fx, gx) -> HS.map (\ h -> (Vertex g $ HS.map (\ y -> if y == gx then h else y) gs)) $ neighbors fx gx) $ zipOn (HS.toList fs) (HS.toList gs))
    | otherwise = HS.fromList [(\ f -> (Vertex f gs)) f] where
        fg = f == g
        diff1 = HS.filter (\ (Vertex x _) -> HS.null (HS.filter (\ y -> startsWith x y) gs)) fs
        diff2 = HS.filter (\ (Vertex x _) -> HS.null (HS.filter (\ y -> startsWith x y) fs)) gs
        nd1 = not (HS.null diff1)
        nd2 = not (HS.null diff2)

        -- function that handles partially same parts of the graphs
        magic :: [CallGraph] -> [CallGraph] -> HS.HashSet CallGraph -> Function -> HS.HashSet CallGraph
        magic [] _ _ _ = HS.empty
        magic _ [] _ _ = HS.empty
        magic (fv@(Vertex f@(F fn ft fr) fs):xs) ys hs h
            | Just (Vertex v@(F vn vt vr) vs) <- find (\ (Vertex g@(F _ gt _) _) -> gt == ft) ys = HS.insert ((\ x -> Vertex h $ HS.map (\ y -> if y == fv then Vertex x fs else y) hs) v) (magic xs ys hs h)
            | Just (Vertex v@(F vn vt vr) vs) <- find (\ (Vertex g@(F gn _ _) _) -> gn == fn) ys = HS.insert ((\ x -> Vertex h $ HS.map (\ y -> if y == fv then Vertex x fs else y) hs) v) (magic xs ys hs h)
            | otherwise = HS.insert (Vertex h $ HS.delete fv hs) $ magic xs ys hs h

-- the cost function of the a* algorithm
-- cost of getting from the first graph to the second
cost :: CallGraph -> CallGraph -> Int
cost g@(Vertex gf gs) h@(Vertex hf hs) = costF gf hf + sum (map (uncurry cost) zipped) + sum (map (uncurry cost) matched) + sum (map costLen ngs) + sum (map costLen nhs) where
        zipped = zipOn (HS.toList gs) (HS.toList hs)
        gsz = HS.toList $ HS.filter (\ g -> not $ elem g (map fst zipped)) gs
        hsz = HS.toList $ HS.filter (\ h -> not $ elem h (map snd zipped)) hs
        (matched, ngs, nhs) = magic gsz hsz

        -- function that handles partially same parts of the graphs
        magic :: [CallGraph] -> [CallGraph] -> ([(CallGraph, CallGraph)], [CallGraph], [CallGraph])
        magic [] ys = ([], [], ys)
        magic xs [] = ([], xs, [])
        magic (fv@(Vertex f@(F fn ft fr) fs):xs) ys
            | Just v <- find (\ (Vertex g@(F _ gt _) _) -> gt == ft) ys = let
                (matched, ngs, nhs) = magic xs (delete v ys)
                in ((fv, v):matched, ngs, nhs)
            | Just v <- find (\ (Vertex g@(F gn _ _) _) -> gn == fn) ys = let
                (matched, ngs, nhs) = magic xs (delete v ys)
                in ((fv, v):matched, ngs, nhs)
            | otherwise = let
                (matched, ngs, nhs) = magic xs ys
                in (matched, (fv:ngs), nhs)

-- get the substitution cost by adding up 10-s the times of the length of the graph
-- costLen(g) = Σ(i∈[1..len(g)]): 10
-- len(g) = the number of vertexes in the graph
costLen :: CallGraph -> Int
costLen (Vertex _ fs)
    | HS.null fs = 10
    | otherwise = 10 + sum (map costLen $ HS.toList fs)

-- cost of getting from the first function to the second
costF :: Function -> Function -> Int
costF (F fn ft fr) (F gn gt gr)
    | fn /= gn && ft /= gt && fr /= gr = 6
    | ft /= gt && fr /= gr = 5
    | fn /= gn && ft /= gt = 4
    | fn /= gn && fr /= gr = 3
    | ft /= gt = 3
    | fr /= gr = 2
    | fn /= gn = 1
    | otherwise = 0

-- zips the graph lists on the root function names
zipOn :: [CallGraph] -> [CallGraph] -> [(CallGraph, CallGraph)]
zipOn [] _ = []
zipOn _ [] = []
zipOn (vf@(Vertex f _):fs) gs
    | Just vg <- find (\ (Vertex g _) -> f == g) gs = (vf, vg) : zipOn fs gs
    | otherwise = zipOn fs gs

-- gets the percentage of similarity using the cost function and the maximum of reachable cost
similarityScore :: CallGraph -> CallGraph -> [CallGraph] -> MatchNum -- get the match number from the original graph and the shortest path to the other one
similarityScore startg goalg path = round (100 * ((fromIntegral (maxc - abscost)) / (fromIntegral maxc))) where
    maxc = maxCost startg goalg
    abscost = cost startg (last path) -- sum $ zipWith cost path (drop 1 path)

-- calculates the estimated max cost of getting from the first graph to the second
maxCost :: CallGraph -> CallGraph -> Int
maxCost startg goalg = 6 * (vs + vg) + 10 * (es + eg) where
    vs = vertNum startg
    vg = vertNum goalg
    es = edgeNum startg
    eg = edgeNum goalg

-- the a* algorithm using the functions defined above
mainAlgorithm :: CallGraph -> CallGraph -> MatchNum
mainAlgorithm startGraph goalGraph =
  case aStar (neighbors goalGraph) cost (heuristic startGraph goalGraph) (== goalGraph) startGraph of
    Just [] -> 100
    Just path -> similarityScore startGraph goalGraph path
    Nothing -> 0

-- makes all the possible pairs from a list
makePairs :: [a] -> [(a, a)]
makePairs [] = []
makePairs (x:xs) = pairElement x xs ++ makePairs xs where
    pairElement :: a -> [a] -> [(a,a)]
    pairElement _ [] = []
    pairElement e (x:xs) = (e , x) : pairElement e xs

-- runs the a* algorithm on all possible pairs of the codes in the list
runOnAll :: [Code] -> [Response]
runOnAll = map (\ ((id1, g), (id2, h)) -> (id1, id2, map (\ (fn, x, y) -> (fn, mainAlgorithm x y)) (zipOnFunName g h))) . makePairs

-- this function zips the call graphs on function name equality and preserves the function name as an identifier
zipOnFunName :: [CallGraph] -> [CallGraph] -> [(String, CallGraph, CallGraph)]
zipOnFunName [] _ = []
zipOnFunName _ [] = []
zipOnFunName (vf@(Vertex (F fn fty _) _):fs) gs
    | Just vg <- find (\ (Vertex (F n ty _) _) -> fn == n && fty == ty) gs = (dropWhile (=='.') fn, vf, vg) : zipOnFunName fs gs
    | Just vg <- find (\ (Vertex (F n ty _) _) -> fn == n) gs = (dropWhile (=='.') fn, vf, vg) : zipOnFunName fs gs
    | otherwise = zipOnFunName fs gs

f path1 path2 = do
    x <- parseFile path1
    y <- parseFile path2
    case x of
        ParseOk (Module _ _ _ declx) -> case y of
            ParseOk (Module _ _ _ decly) -> putStrLn $ show $ mainAlgorithm (wholeCodeGraph declx) (wholeCodeGraph decly)
            _ -> putStrLn "Test file couldn't be parsed, possibly syntax error in file."
        _ -> putStrLn "Test file couldn't be parsed, possibly syntax error in file."
