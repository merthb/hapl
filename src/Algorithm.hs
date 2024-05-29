module Algorithm (ID,Code,MatchNum,Response,heuristic,neighbors,cost,maxCost,similarityScore,mainAlgorithm,runOnAll,makePairs,zipOnFunName) where

import Graph
import Data.Graph.AStar
import Data.List
import qualified Data.HashSet as HS
import Language.Haskell.Exts.Simple

type ID = String
type Code = (ID, [CallGraph])
type MatchNum = Int
type Response = (ID, ID, [(String, MatchNum)])

-- az A* algoritmus heurisztikája
heuristic :: CallGraph -> CallGraph -> CallGraph -> Int
heuristic startg goalg g = (min rgn rhn) + 10 * (abs (rgn - rhn)) where 
    rgn = rg startg goalg g
    rhn = rh goalg g

-- azon csúcsok számossága, amelyek a célgráfban jelen vannak, de a részben transzformált gráfban még nem szerepelnek
rh :: CallGraph -> CallGraph -> Int
rh goalg (Vertex f fs)
    | f `inG` goalg = HS.foldr (+) 0 $ HS.map (rh goalg) fs
    | otherwise = 1 + (HS.foldr (+) 0 $ HS.map (rh goalg) fs)

-- azon csúcsok számossága, amelyek a kiindulási gráfból még jelen vannak a részben transzformált gráfban, de a célgráfban nem
rg :: CallGraph -> CallGraph -> CallGraph -> Int
rg startg goalg (Vertex f fs)
    | f `inG` startg && not (f `inG` goalg) = 1 + (HS.foldr (+) 0 $ HS.map (rg startg goalg) fs)
    | otherwise = HS.foldr (+) 0 $ HS.map (rg startg goalg) fs

-- az A* algoritmus szomszédkereső függvénye
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

        -- ez a függvény kezeli a részben egyező részeket
        magic :: [CallGraph] -> [CallGraph] -> HS.HashSet CallGraph -> Function -> HS.HashSet CallGraph
        magic [] _ _ _ = HS.empty
        magic _ [] _ _ = HS.empty
        magic (fv@(Vertex f@(F fn ft fr) fs):xs) ys hs h
            | Just (Vertex v@(F vn vt vr) vs) <- find (\ (Vertex g@(F _ gt _) _) -> gt == ft) ys = HS.insert ((\ x -> Vertex h $ HS.map (\ y -> if y == fv then Vertex x fs else y) hs) v) (magic xs ys hs h)
            | Just (Vertex v@(F vn vt vr) vs) <- find (\ (Vertex g@(F gn _ _) _) -> gn == fn) ys = HS.insert ((\ x -> Vertex h $ HS.map (\ y -> if y == fv then Vertex x fs else y) hs) v) (magic xs ys hs h)
            | otherwise = HS.insert (Vertex h $ HS.delete fv hs) $ magic xs ys hs h

-- az A* algoritmus költségszámítása
cost :: CallGraph -> CallGraph -> Int
cost g@(Vertex gf gs) h@(Vertex hf hs) = costF gf hf + sum (map (uncurry cost) zipped) + sum (map (uncurry cost) matched) + sum (map costLen ngs) + sum (map costLen nhs) where
        zipped = zipOn (HS.toList gs) (HS.toList hs)
        gsz = HS.toList $ HS.filter (\ g -> not $ elem g (map fst zipped)) gs
        hsz = HS.toList $ HS.filter (\ h -> not $ elem h (map snd zipped)) hs
        (matched, ngs, nhs) = magic gsz hsz
        -- ez a függvény kezeli a részben egyező részeket
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
        -- behelyettesítés költségének kalkulátora
        costLen :: CallGraph -> Int
        costLen (Vertex _ fs)
            | HS.null fs = 10
            | otherwise = 10 + sum (map costLen $ HS.toList fs)
        -- két függvény közti költséget számolja ki
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

-- összepárosítja a két gráflistát úgy, hogy a gráfok csúcsában ugyanazok a függvények legyenek egy párban
zipOn :: [CallGraph] -> [CallGraph] -> [(CallGraph, CallGraph)]
zipOn [] _ = []
zipOn _ [] = []
zipOn (vf@(Vertex f _):fs) gs
    | Just vg <- find (\ (Vertex g _) -> f == g) gs = (vf, vg) : zipOn fs gs
    | otherwise = zipOn fs gs

-- kiszámolja az egyezési százalékot a legrövidebb út és a vélhető maximum költség segítségével
similarityScore :: CallGraph -> CallGraph -> [CallGraph] -> MatchNum
similarityScore startg goalg path = round (100 * ((fromIntegral (maxc - abscost)) / (fromIntegral maxc))) where
    maxc = maxCost startg goalg
    abscost = sum $ zipWith cost path (drop 1 path)

-- kiszámolja a vélhető maximumköltséget két gráf közt
maxCost :: CallGraph -> CallGraph -> Int
maxCost startg goalg = 6 * (vs + vg) + 10 * (es + eg) where
    vs = vertNum startg
    vg = vertNum goalg
    es = edgeNum startg
    eg = edgeNum goalg

-- az A* algoritmus
mainAlgorithm :: CallGraph -> CallGraph -> MatchNum
mainAlgorithm startGraph goalGraph =
  case aStar (neighbors goalGraph) cost (heuristic startGraph goalGraph) (== goalGraph) startGraph of
    Just [] -> 100
    Just path -> similarityScore startGraph goalGraph path
    Nothing -> 0

-- előállítja az összes lehetséges párosítást egy listából
makePairs :: [a] -> [(a, a)]
makePairs [] = []
makePairs (x:xs) = pairElement x xs ++ makePairs xs where
    pairElement :: a -> [a] -> [(a,a)]
    pairElement _ [] = []
    pairElement e (x:xs) = (e , x) : pairElement e xs

-- futtatja az algoritmust az összes lehetséges párosításon
runOnAll :: [Code] -> [Response]
runOnAll = map (\ ((id1, g), (id2, h)) -> (id1, id2, map (\ (fn, x, y) -> (fn, mainAlgorithm x y)) (zipOnFunName g h))) . makePairs

-- összepárosítja a két gráflistát úgy, hogy a gráfok csúcsában ugyanazok a függvénynevek legyenek egy párban, ezt a függvénynevet eltárolja mellettük
zipOnFunName :: [CallGraph] -> [CallGraph] -> [(String, CallGraph, CallGraph)]
zipOnFunName [] _ = []
zipOnFunName _ [] = []
zipOnFunName (vf@(Vertex (F fn fty _) _):fs) gs
    | Just vg <- find (\ (Vertex (F n ty _) _) -> fn == n && fty == ty) gs = (dropWhile (=='.') fn, vf, vg) : zipOnFunName fs gs
    | Just vg <- find (\ (Vertex (F n ty _) _) -> fn == n) gs = (dropWhile (=='.') fn, vf, vg) : zipOnFunName fs gs
    | otherwise = zipOnFunName fs gs
