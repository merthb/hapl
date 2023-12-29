{-# LANGUAGE GADTs #-}
{-# LANGUAGE DeriveAnyClass #-}
module Graph where -- (Graph(..), Function(..), CallGraph(..), parseInFile, callsFromBinds, functionCalls, getCallGraphs) where
import Language.Haskell.Exts.Simple
import Data.List
import Data.Hashable

-- this module is for the call-graph algebraic data structure

-- the graph implementation we will use to represent the call-graph
data Graph a = Vertex {node :: a, edges :: [Graph a]}
    deriving (Eq, Ord)

instance Show a => Show (Graph a) where
    show (Vertex node edges) = case edges of
        [] -> show node
        xs -> show node ++ " => " ++ show xs

instance (Hashable a) => Hashable (Graph a) where
    hashWithSalt salt (Vertex val children) = hashWithSalt (hashWithSalt salt val) childrenHash where
        childrenHash = foldl hashWithSalt 0 (map (hashWithSalt salt) children)

-- functions have their unique names and their type signatures
data Function = F String String Bool
    deriving (Eq, Ord)

instance Hashable Function where
    hashWithSalt salt (F name typ boolVal) = hashWithSalt (hashWithSalt (hashWithSalt salt name) typ) boolVal

-- our call-graph will be a graph that contains functions for we want to represent the function calls in the code
type CallGraph = Graph Function

instance Show Function where
    show (F name ty r) = name ++ " :: " ++ ty ++ " (rec: " ++ show r ++ ")"

inG :: Function -> CallGraph -> Bool
inG f (Vertex v vs)
    | f == v = True
    | otherwise = any (inG f) vs

vertNum :: CallGraph -> Int
vertNum (Vertex f fs) = 1 + sum (map edgeNum fs)

edgeNum :: CallGraph -> Int
edgeNum (Vertex f fs) = length fs + sum (map edgeNum fs)

insertEdge :: CallGraph -> CallGraph -> CallGraph
insertEdge (Vertex f fs) g = (Vertex f (g : fs))

deleteEdge :: CallGraph -> CallGraph -> CallGraph
deleteEdge fg@(Vertex f fs) g = Vertex f (delete g fs)

startsWith :: Function -> CallGraph -> Bool
startsWith f (Vertex g _) = f == g

showType :: Type -> String
showType (TyList xs) = "[" ++ showType xs ++ "]"
showType (TyApp ty par) = showType ty ++ " " ++ showType par
showType (TyFun from to) = showType from ++ " -> " ++ showType to
showType (TyTuple _ tys) = "(" ++ (tail $ tail $ concat $ map ((", " ++) . showType) tys) ++ ")"
showType (TyVar name) = case name of Ident xs -> xs; Symbol xs -> xs
showType (TyCon name) = case name of Qual (ModuleName m) n -> m ++ '.' : showType (TyVar n); UnQual n -> showType (TyVar n); _ -> ""
showType (TyParen ty) = "(" ++ showType ty ++ ")"
showType x = ""

filteredPreludeFuns :: [String]
filteredPreludeFuns = ["length", "map", "filter", "all", "any", "and", "or", "++", "concat", "concatMap", ":", "scanl", "scanr", "iterate", "cycle", "replicate", "repeat", "take", "takeWhile", "drop", "dropWhile", "span", "break", "splitAt", "notElem", "lookup", "zip", "zipWith", "unzip", "lines", "unlines", "words", "unwords", "head", "tail", "last", "init", "!!", "null", "reverse", "max", "min", "maximum", "minimum", "id", ".", "not", "&&", "||", "fst", "snd", "curry", "uncurry", "fromInteger", "toInteger", "even", "odd", "fromIntegral", "foldl", "foldr", "elem", "sum", "product", "const", "flip", "until", "error", "undefined", "show", "read"]

toFun :: String -> Function
toFun xs = F xs "" False

parseInFile :: [Decl] -> [Function]
parseInFile xs = parseDec xs []

parseDec :: [Decl] -> [Function] -> [Function]
parseDec [] _ = []
parseDec ((TypeSig names ty):xs) fs = news ++ parseDec xs (news ++ fs) where
    news = map (\ name -> case name of Ident xs -> F xs (showType ty) False; Symbol xs -> F xs (showType ty) False) names
parseDec ((PatBind _ _ binds):xs) fs = news ++ parseDec xs (news ++ fs) where
    news = fromBinds binds fs
parseDec ((FunBind matches):xs) fs = news ++ parseDec xs (news ++ fs) where
    news = foldr (\ m funs -> case m of (Match name patts _ binds) -> handle name patts fs ++ fromBinds binds fs ++ funs; _ -> funs) [] matches
    handle (Ident name) patts xs
        | null (filter (\ (F n _ _) -> n == name) xs) = [F name (createTypeSig patts ['a'..]) False]
        | otherwise = []
    handle (Symbol name) patts xs
        | null (filter (\ (F n _ _) -> n == name) xs) = [F name (createTypeSig patts ['a'..]) False]
        | otherwise = []
    createTypeSig [] (a:as) = [a]
    createTypeSig (_:xs) (a:as) = a : " -> " ++ createTypeSig xs as 
parseDec (_:xs) fs = parseDec xs fs

fromBinds :: Maybe Binds -> [Function] -> [Function]
fromBinds Nothing _ = []
fromBinds (Just (BDecls dl)) fs = parseDec dl fs
fromBinds _ _ = []

functionCalls :: [Function] {-which functions we need-} -> [Function] {-all the functions we know of-} -> [Decl] {-the code-} -> [(Function, [Function])] {-which functions they call-}
functionCalls [] _ _ = []
functionCalls (f:fs) kf dl = (f, nub (getCalls f kf dl)) : functionCalls fs kf dl where
    getCalls :: Function -> [Function] -> [Decl] -> [Function]
    getCalls _ [] _ = []
    getCalls _ _ [] = []
    getCalls f kf ((FunBind matches):xs) = fromMatches f matches kf ++ getCalls f kf xs where
        fromMatches :: Function -> [Match] -> [Function] -> [Function]
        fromMatches _ [] _ = []
        fromMatches f@(F n t r) ((Match fn _ rhs binds):ms) kf
            | n == (case fn of Ident xs -> xs; Symbol xs -> xs) = fromRhs rhs kf ++ callsFromBinds binds kf ++ fromMatches f ms kf
            | otherwise = callsFromBindsF binds f kf ++ fromMatches f ms kf where
                fromRhs :: Rhs -> [Function] -> [Function]
                fromRhs (UnGuardedRhs exp) kf = evalExp exp kf
                fromRhs (GuardedRhss rhss) kf = concatMap (\ (GuardedRhs stmt exp) -> evalExp exp kf ++ procStmt (RecStmt stmt) kf) rhss
        fromMatches f (_:ms) kf = fromMatches f ms kf
    getCalls f@(F n t r) kf ((PatBind (PVar name) rhs binds):xs)
        | n == (case name of Ident xs -> xs; Symbol xs -> xs) = callsFromBinds binds kf ++ (case rhs of (UnGuardedRhs exp) -> evalExp exp kf; (GuardedRhss rhss) -> concatMap (\ (GuardedRhs stmt exp) -> evalExp exp kf ++ procStmt (RecStmt stmt) kf) rhss;) ++ getCalls f kf xs
        | otherwise = callsFromBindsF binds f kf ++ getCalls f kf xs
    getCalls f kf (_:xs) = getCalls f kf xs

callsFromBindsF :: Maybe Binds -> Function -> [Function] -> [Function]
callsFromBindsF (Just (BDecls decls)) f kf = helperF f kf decls where
    helperF :: Function -> [Function] -> [Decl] -> [Function]
    helperF _ [] _ = []
    helperF f kf dl = nub (getCallsF f kf dl) where
        getCallsF :: Function -> [Function] -> [Decl] -> [Function]
        getCallsF _ [] _ = []
        getCallsF _ _ [] = []
        getCallsF f kf ((FunBind matches):xs) = fromMatches f matches kf ++ getCallsF f kf xs where
            fromMatches :: Function -> [Match] -> [Function] -> [Function]
            fromMatches _ [] _ = []
            fromMatches f@(F n t r) ((Match fn _ rhs binds):ms) kf
                | n == (case fn of Ident xs -> xs; Symbol xs -> xs) = fromRhs rhs kf ++ callsFromBindsF binds f kf ++ fromMatches f ms kf
                | otherwise = callsFromBindsF binds f kf ++ fromMatches f ms kf where
                    fromRhs :: Rhs -> [Function] -> [Function]
                    fromRhs (UnGuardedRhs exp) kf = evalExp exp kf
                    fromRhs (GuardedRhss rhss) kf = concatMap (\ (GuardedRhs stmt exp) -> evalExp exp kf ++ procStmt (RecStmt stmt) kf) rhss
            fromMatches f (_:ms) kf = fromMatches f ms kf
        getCallsF f kf (_:xs) = getCallsF f kf xs
callsFromBindsF _ _ _ = []

callsFromBinds :: Maybe Binds -> [Function] -> [Function]
callsFromBinds (Just (BDecls decls)) kf = helper kf decls where
    helper :: [Function] -> [Decl] -> [Function]
    helper [] _ = []
    helper kf dl = nub (getCallsh kf dl) where
        getCallsh :: [Function] -> [Decl] -> [Function]
        getCallsh [] _ = []
        getCallsh _ [] = []
        getCallsh kf ((PatBind _ rhs binds):xs) = callsFromBinds binds kf ++ (case rhs of (UnGuardedRhs exp) -> evalExp exp kf; (GuardedRhss rhss) -> concatMap (\ (GuardedRhs stmt exp) -> evalExp exp kf ++ procStmt (RecStmt stmt) kf) rhss;) ++ getCallsh kf xs
        getCallsh kf (_:xs) = getCallsh kf xs
callsFromBinds _ _ = []

evalExp :: Exp -> [Function] -> [Function]
evalExp (Var name) kf
    | Just f <- find (\ (F fn _ _) -> n == fn) kf = [f]
    | otherwise = [] where
        n = (case name of Qual _ (Ident xs) -> xs; Qual _ (Symbol xs) -> xs; UnQual (Ident xs) -> xs; UnQual (Symbol xs) -> xs; _ -> "";)
evalExp (App exp1 exp2) kf = evalExp exp1 kf ++ evalExp exp2 kf
evalExp (InfixApp exp1 (QVarOp name) exp2) kf
    | Just f <- find (\ (F fn _ _) -> n == fn) kf = f : evalExp exp1 kf ++ evalExp exp2 kf
    | otherwise = evalExp exp1 kf ++ evalExp exp2 kf where
        n = (case name of Qual _ (Ident xs) -> xs; Qual _ (Symbol xs) -> xs; UnQual (Ident xs) -> xs; UnQual (Symbol xs) -> xs; _ -> "";)
evalExp (InfixApp exp1 _ exp2) kf = evalExp exp1 kf ++ evalExp exp2 kf
evalExp (Lambda _ exp) kf = evalExp exp kf
evalExp (Paren exp) kf = evalExp exp kf
evalExp (Let binds exp) kf = fromBinds (Just binds) kf ++ evalExp exp kf
evalExp (If exp1 exp2 exp3) kf = evalExp exp1 kf ++ evalExp exp2 kf ++ evalExp exp3 kf
evalExp (MultiIf grhss) kf = concatMap (\ (GuardedRhs stmt exp) -> evalExp exp kf ++ procStmt (RecStmt stmt) kf) grhss
evalExp (Case exp alts) kf = evalExp exp kf ++ concatMap (\ (Alt _ def binds) -> fromBinds binds kf ++ (case def of UnGuardedRhs exp -> evalExp exp kf; GuardedRhss grhss -> concatMap (\ (GuardedRhs stmt exp) -> evalExp exp kf ++ procStmt (RecStmt stmt) kf) grhss;)) alts
evalExp (Do stmts) kf = procStmt (RecStmt stmts) kf
evalExp (MDo stmts) kf = procStmt (RecStmt stmts) kf
evalExp (Tuple _ exps) kf = concatMap ((flip evalExp) kf) exps
evalExp (TupleSection _ mexps) kf = concatMap ((flip evalExp) kf) [x | (Just x)<-mexps]
evalExp (List exps) kf = concatMap ((flip evalExp) kf) exps
evalExp (ParArray exps) kf = concatMap ((flip evalExp) kf) exps
evalExp (NegApp exp) kf = evalExp exp kf
evalExp (EnumFrom exp) kf = evalExp exp kf
evalExp (EnumFromTo exp1 exp2) kf = evalExp exp1 kf ++ evalExp exp2 kf
evalExp (EnumFromThen exp1 exp2) kf = evalExp exp1 kf ++ evalExp exp2 kf
evalExp (EnumFromThenTo exp1 exp2 exp3) kf = evalExp exp1 kf ++ evalExp exp2 kf ++ evalExp exp3 kf
evalExp (ParArrayFromTo exp1 exp2) kf = evalExp exp1 kf ++ evalExp exp2 kf
evalExp (ParArrayFromThenTo exp1 exp2 exp3) kf = evalExp exp1 kf ++ evalExp exp2 kf ++ evalExp exp3 kf
evalExp (ListComp exp qstmts) kf = evalExp exp kf ++ concatMap ((flip procQStmt) kf) qstmts
evalExp _ _ = []

procStmt :: Stmt -> [Function] -> [Function]
procStmt (Generator _ exp) kf = evalExp exp kf
procStmt (Qualifier exp) kf = evalExp exp kf
procStmt (LetStmt binds) kf = fromBinds (Just binds) kf
procStmt (RecStmt stmts) kf = concatMap ((flip procStmt) kf) stmts

procQStmt :: QualStmt -> [Function] -> [Function]
procQStmt (QualStmt stmt) kf = procStmt stmt kf
procQStmt (ThenTrans exp) kf = evalExp exp kf
procQStmt (ThenBy exp1 exp2) kf = evalExp exp1 kf ++ evalExp exp2 kf
procQStmt (GroupBy exp) kf = evalExp exp kf
procQStmt (GroupUsing exp) kf = evalExp exp kf
procQStmt (GroupByUsing exp1 exp2) kf = evalExp exp1 kf ++ evalExp exp2 kf

----------------------------------------

findRec :: [(Function, [Function])] -> [Function]
findRec [] = []
findRec ((f@(F n t r), fs):xs)
    | elem f fs = F n t True : findRec xs
    | otherwise = findRec xs

flagRecursion :: [Function] -> [(Function, [Function])] -> [(Function, [Function])]
flagRecursion [] xs = xs
flagRecursion (f@(F n t r):fs) xs = flagRecursion fs $ map (setRec f) xs where
    setRec :: Function -> (Function, [Function]) -> (Function, [Function])
    setRec f@(F n t r) (g@(F n1 _ _), fs)
        | n == n1 = (f, delete (F n t False) fs)
        | otherwise = (g, map (\ h@(F n2 t2 r2) -> if n == n2 then f else h) fs)

getCallGraphs :: [String] {-name of functions we need-} -> [Decl] -> [CallGraph]
getCallGraphs fsn decl = filter (\ (Vertex (F n t r) fs) -> elem n fsn) $ map (genGraph dict []) dict where
    d = functionCalls (parseInFile decl) (parseInFile decl ++ map toFun filteredPreludeFuns) decl
    dict = flagRecursion (findRec d) d

genGraph :: [(Function, [Function])] -> [Function] -> (Function, [Function]) -> CallGraph
genGraph dict procc (f, fs) = Vertex f $ genChilds fs dict (f:procc)where
    genChilds :: [Function] -> [(Function, [Function])] -> [Function] -> [CallGraph]
    genChilds [] _ _ = []
    genChilds (f@(F n "" r):fs) d p = Vertex f [] : genChilds fs d p
    genChilds (f:fs) dict processed
        | Just fss <- lookup f dict = genGraph dict processed (f, filter (\ g -> not $ elem g processed) fss) : genChilds fs dict processed
        | otherwise = genChilds fs dict processed

---------------------------------------------
