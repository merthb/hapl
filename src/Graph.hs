{-# LANGUAGE GADTs #-}
module Graph (Graph (..),Function (..),CallGraph,preludeFuns,toFun,allDifferent,parseInFile,prep,functionCalls,handleRecursion,getCallGraphs,wholeCodeGraph,inG,vertNum,edgeNum,startsWith,deleteEdge,insertEdge) where
import Language.Haskell.Exts.Simple
import Data.List
import Data.Hashable
import qualified Data.HashSet as HS

-- this module is for the call-graph algebraic data structure and its functions

-- the graph implementation we will use to represent the call-graph
-- a graph will have a root element and a list of the child elements
-- we won't represent circles in the graph to make the algorithm a bit simpler and faster
data Graph a = Vertex {node :: a, edges :: HS.HashSet (Graph a)}
    deriving (Show, Eq, Ord)

-- -- Show instance to print the graph out in a more readable way
-- instance Show a => Show (Graph a) where
--     show v@(Vertex node edges) = (case HS.null edges of
--         True -> show node
--         False -> myShow v 1) where
--             myShow :: Show a => Graph a -> Int -> String
--             myShow (Vertex node edges) tabnum = show node ++ " => \n" ++ replicate tabnum '\t' ++ (drop (2 + tabnum) $ concatMap (((",\n" ++ replicate tabnum '\t') ++) . (flip myShow (tabnum + 1))) edges)

-- Hashable instance is needed for the a* algorithm
instance (Hashable a) => Hashable (Graph a) where
    hashWithSalt salt (Vertex val children) = hashWithSalt (hashWithSalt salt val) childrenHash where
        childrenHash = HS.foldr hashWithSalt 0 (HS.map (hashWithSalt salt) children)

-- functions have their unique names, their type signatures and a flag for recursiveness
data Function = F String String Bool
    deriving (Show, Eq, Ord)

-- a Hashable instance is needed so it can be contained in the Graph representation
instance Hashable Function where
    hashWithSalt salt (F name typ boolVal) = hashWithSalt (hashWithSalt (hashWithSalt salt name) typ) boolVal

-- -- show instance to print the function in a more readable way
-- instance Show Function where
--     show (F name ty r) = name ++ " :: " ++ ty ++ " (rec: " ++ show r ++ ")"

-- our call-graph will be a graph that contains functions for we want to represent the function calls in the code
type CallGraph = Graph Function

------------- some graph and parse functions ----------------------------

-- decides if a function is present in a graph
inG :: Function -> CallGraph -> Bool
inG f (Vertex v vs)
    | f == v = True
    | otherwise = HS.foldr (||) False $ HS.map (inG f) vs

-- returns the number of vertexes in the graph
vertNum :: CallGraph -> Int
vertNum (Vertex f fs) = 1 + HS.foldr (+) 0 (HS.map vertNum fs)

-- returns the number of edges in the graph
edgeNum :: CallGraph -> Int
edgeNum (Vertex f fs) = HS.size fs + HS.foldr (+) 0 (HS.map edgeNum fs)

-- inserts the second graph as an edge of the first graph
insertEdge :: CallGraph -> CallGraph -> CallGraph
insertEdge (Vertex f fs) g = Vertex f (HS.insert g fs)

-- deletes the second graph from the edges of the first graph
deleteEdge :: CallGraph -> CallGraph -> CallGraph
deleteEdge (Vertex f fs) g = Vertex f (HS.delete g fs)

-- checks if a graphs root function is the given function or not
startsWith :: Function -> CallGraph -> Bool
startsWith f (Vertex g _) = f == g

-- converts a Type data to String
showType :: Type -> String
showType (TyList xs) = "[" ++ showType xs ++ "]"
showType (TyApp ty par) = showType ty ++ " " ++ showType par
showType (TyFun from to) = showType from ++ " -> " ++ showType to
showType (TyTuple _ tys) = "(" ++ (tail $ tail $ concat $ map ((", " ++) . showType) tys) ++ ")"
showType (TyVar name) = case name of Ident xs -> xs; Symbol xs -> xs
showType (TyCon name) = case name of Qual (ModuleName m) n -> m ++ '.' : showType (TyVar n); UnQual n -> showType (TyVar n); _ -> ""
showType (TyParen ty) = "(" ++ showType ty ++ ")"
showType (TyForall _ _ ty) = showType ty
showType _ = ""

-- converts a String data to Type
toType :: String -> Type
toType [x] = TyVar (Ident [x])
toType (x:xs) = TyFun (TyVar (Ident [x])) (toType xs)

-- function names included in the Prelude library
preludeFuns :: [String]
preludeFuns = ["case", "if", "length", "map", "filter", "all", "any", "and", "or", "++", "concat", "concatMap", ":", "scanl", "scanr", "iterate", "cycle", "replicate", "repeat", "take", "takeWhile", "drop", "dropWhile", "span", "break", "splitAt", "notElem", "lookup", "zip", "zipWith", "unzip", "lines", "unlines", "words", "unwords", "head", "tail", "last", "init", "!!", "null", "reverse", "max", "min", "maximum", "minimum", "id", ".", "not", "&&", "||", "==", "/=", "<=", ">=", "<", ">", "fst", "snd", "curry", "uncurry", "fromInteger", "toInteger", "even", "odd", "fromIntegral", "foldl", "foldr", "elem", "sum", "product", "const", "flip", "until", "error", "undefined", "show", "read"]

-- TODO idea: other modules function lists and if they are imported then add them to the known function list

-- creates a type free non-recursive function with the given name
toFun :: String -> Function
toFun xs = F xs "" False

-- gets the string data from the Name
getName :: Name -> String
getName name = case name of Ident xs -> xs; Symbol xs -> xs

-- sets the string data in the Name
setName :: Name -> String -> Name
setName name n = case name of Ident xs -> Ident n; Symbol xs -> Symbol n

---------------- preprocess code files ---------------------------

-- checks if all functions have different names in the list
allDifferent :: [Function] -> Bool
allDifferent [] = True
allDifferent (x:xs) 
    | myelem x xs = False 
    | otherwise = allDifferent xs where
        myelem :: Function -> [Function] -> Bool
        myelem (F fn _ _) fs = not $ null $ filter (\ (F gn _ _) -> fn == gn) fs

-- this function does the preprocessing
-- gives type sig to the functions that doesn't have one
-- renames the local functions having the same name as global or other local functions
prep :: [Decl] -> [Decl]
prep xs = prep' $ addTypeSig xs $ map (\ (F n _ _) -> n) $ parseInFile xs
    
-- this helper does the recursion
prep' :: [Decl] -> [Decl]
prep' xs
    | allDifferent (parseInFile decl) = decl
    | otherwise = prep' decl where
        renames = whatToRename $ parseInFile xs
        decl = doPrep renames xs

-- this does one run of renaming
doPrep :: [(String, String)] -> [Decl] -> [Decl]
doPrep [] decl = decl
doPrep (rs@(f,nf):xs) decl = case getGlobalDecl f decl of
    Just d -> doPrep xs $ map (\ x -> if x == d then renameFirst d rs else x) decl
    _ -> doPrep xs decl

-- collects all functions, that has to be renamed, and creates the renames too
whatToRename :: [Function] -> [(String, String)]
whatToRename xs = concatMap (\ xs -> genRenames (drop 1 xs) 1) $ filter (\ as -> case as of [] -> False; [x] -> False; _ -> True) $ groupBy (\ (F fn _ _) (F gn _ _) -> fn == gn) $ sortBy (\ (F fn _ _) (F gn _ _) -> compare fn gn) xs

-- creates different names for the functions with the same name
genRenames :: [Function] -> Int -> [(String, String)]
genRenames [] _ = []
genRenames ((F x _ _):xs) n = (x, replicate n '.' ++ x) : genRenames xs (n + 1)

-- gives type signature to the functions that doesn't have one
addTypeSig :: [Decl] -> [String] -> [Decl]
addTypeSig [] _ = []
addTypeSig ((FunBind (m@(Match fn patts _ _):ms)):xs) fs 
    | not $ elem (getName fn) fs = (TypeSig [fn] (toType $ createTypeSig (patts) ['a'..])) : (FunBind $ recheckMatches (m:ms) fs) : addTypeSig xs fs
    | otherwise = (FunBind $ recheckMatches (m:ms) fs) : addTypeSig xs fs where
        -- creates a type signature with as many parameters, as many pattern binds it has
        createTypeSig :: [Pat] -> [Char] -> [Char]
        createTypeSig [] (a:as) = [a]
        createTypeSig (_:xs) (a:as) = a : createTypeSig xs as
        -- checks the matches of a FunBind
        recheckMatches :: [Match] -> [String] -> [Match]
        recheckMatches [] _ = []
        recheckMatches ((Match fn pb rhs (Just (BDecls d))):xs) fs = (Match fn pb rhs (Just (BDecls (addTypeSig d fs)))) : recheckMatches xs fs
        recheckMatches (x:xs) fs = x : recheckMatches xs fs
addTypeSig ((PatBind fn rhs (Just (BDecls decl))):xs) fs = PatBind fn rhs (Just (BDecls (addTypeSig decl fs))) : addTypeSig xs fs
addTypeSig (x:xs) fs = x : addTypeSig xs fs

-- checks the righthand side of a function definition if there is anything to be renamed and does the renaming 
rhsChecker :: Rhs -> [(String, String)] -> Rhs
rhsChecker (UnGuardedRhs exp) rs = UnGuardedRhs $ procExp exp rs
rhsChecker (GuardedRhss rhss) rs = GuardedRhss $ procGuarded rhss rs where
    -- if it's a righthand side with a guard, it needs special processing
    procGuarded :: [GuardedRhs] -> [(String, String)] -> [GuardedRhs]
    procGuarded [] rs = []
    procGuarded ((GuardedRhs stmts exp):xs) rs = GuardedRhs (procStmts stmts rs) (procExp exp rs) : procGuarded xs rs

-- this is the function that renames the first occurence of a local function with the given name
-- by locating its parent decl
doFirst :: [Decl] -> (String, String) -> [Decl]
doFirst decls rn@(fn, nfn) = case getGlobalDecl fn decls of
    Just d -> updateDecl decls d $ renameFirst d rn
    Nothing -> decls

-- this function renames the first occurence of a local function to the given new name
-- this works only on one decl element
renameFirst :: Decl -> (String, String) -> Decl
renameFirst (FunBind matches) rn = FunBind $ iteMatches matches rn where
    iteMatches :: [Match] -> (String, String) -> [Match]
    iteMatches [] _ = []
    iteMatches (m@(Match fn patts rhs (Just (BDecls decl))):ms) rn@(f, nf)
        | findTypeSigScope decl f = Match fn patts (rhsChecker rhs [rn]) (Just $ BDecls $ renameInDecl decl [rn]) : ms
        | any (inDecl f) decl = Match fn patts (rhsChecker rhs [rn]) (Just $ BDecls $ doFirst decl rn) : ms 
        | otherwise = m : iteMatches ms rn
    iteMatches (m:ms) rn = m : iteMatches ms rn
renameFirst (PatBind fn rhs (Just (BDecls decl))) rn@(f, nf)
    | findTypeSigScope decl f = PatBind fn (rhsChecker rhs [rn]) (Just $ BDecls $ renameInDecl decl [rn])
    | any (inDecl f) decl = PatBind fn (rhsChecker rhs [rn]) (Just $ BDecls $ doFirst decl rn)
renameFirst x _ = x

-- checks if the current scope contains a type signature for the given function name
findTypeSigScope :: [Decl] -> String -> Bool
findTypeSigScope [] _ = False
findTypeSigScope ((TypeSig names ty):xs) fn = any (\ n -> getName n == fn) names || findTypeSigScope xs fn
findTypeSigScope (x:xs) fn = findTypeSigScope xs fn

-- changes a given element of the decl list to another decl
updateDecl :: [Decl] -> Decl -> Decl -> [Decl]
updateDecl [] _ _ = []
updateDecl (x:xs) d nd
    | x == d = nd : xs
    | otherwise = x : updateDecl xs d nd

-- gets the global scope decl that has the function in its local scope
getGlobalDecl :: String -> [Decl] -> Maybe Decl
getGlobalDecl _ [] = Nothing
getGlobalDecl f (d:ds)
    | inDecl f d && notTypeSigOrThatFunBind f d = Just d
    | otherwise = getGlobalDecl f ds where
        -- checks if the given decl is not just a type signature or just the funbind of the function that has to be found 
        notTypeSigOrThatFunBind :: String -> Decl -> Bool
        notTypeSigOrThatFunBind _ (TypeSig _ _) = False
        notTypeSigOrThatFunBind f (FunBind (m:ms)) = not $ getFunName m == f where
            getFunName :: Match -> String
            getFunName (Match fn _ _ _) = getName fn
        notTypeSigOrThatFunBind _ _ = True

-- checks if a given function name is present in a decl element
inDecl :: String -> Decl -> Bool
inDecl f (TypeSig names _) = elem f (map getName names)
inDecl f (FunBind matches) = inMatches matches f where
    -- looks for the function name inside a function bind list
    inMatches :: [Match] -> String -> Bool
    inMatches [] _ = False
    inMatches ((Match fn _ _ (Just (BDecls decl))):ms) f = getName fn == f || any (inDecl f) decl || inMatches ms f
    inMatches ((Match fn _ _ _):ms) f = getName fn == f || inMatches ms f
    inMatches (m:ms) f = inMatches ms f
inDecl f (PatBind (PVar fn) _ (Just (BDecls decl))) = getName fn == f || any (inDecl f) decl
inDecl f (PatBind (PVar fn) _ _) = getName fn == f
inDecl _ _ = False

-- renames all the functions in the rename list to their new names in a decl list
renameInDecl :: [Decl] -> [(String, String)] -> [Decl]
renameInDecl [] _ = []
renameInDecl ((TypeSig names ty):xs) rs = TypeSig (map (\ n -> case lookup (getName n) rs of Just nname -> setName n nname; Nothing -> n) names) ty : renameInDecl xs rs
renameInDecl ((PatBind (PVar fn) rhs (Just (BDecls binds))):xs) rs = case lookup (getName fn) rs of
    Just nname -> PatBind (PVar $ setName fn nname) (rhsChecker rhs rs) (Just (BDecls (renameInDecl binds rs))) : renameInDecl xs rs
    Nothing -> PatBind (PVar fn) (rhsChecker rhs rs) (Just (BDecls (renameInDecl binds rs))) : renameInDecl xs rs
renameInDecl ((PatBind (PVar fn) rhs x):xs) rs =  case lookup (getName fn) rs of
    Just nname -> PatBind (PVar $ setName fn nname) (rhsChecker rhs rs) x : renameInDecl xs rs
    Nothing -> PatBind (PVar fn) (rhsChecker rhs rs) x : renameInDecl xs rs
renameInDecl ((FunBind matches):xs) rs = FunBind (renameInMatches matches rs) : renameInDecl xs rs where
    -- does the renaming inside the function binds
    renameInMatches :: [Match] -> [(String, String)] -> [Match]
    renameInMatches [] _ = []
    renameInMatches ((Match fn pb rhs (Just (BDecls decl))):xs) rs = case lookup (getName fn) rs of
        Just nfn -> Match (setName fn nfn) pb (rhsChecker rhs rs) (Just (BDecls (renameInDecl decl rs))) : renameInMatches xs rs
        Nothing -> Match fn pb (rhsChecker rhs rs) (Just (BDecls (renameInDecl decl rs))) : renameInMatches xs rs
    renameInMatches ((Match fn pb rhs x):xs) rs = case lookup (getName fn) rs of
        Just nfn -> Match (setName fn nfn) pb (rhsChecker rhs rs) x : renameInMatches xs rs
        Nothing -> Match fn pb (rhsChecker rhs rs) x : renameInMatches xs rs
    renameInMatches (x:xs) rs = x : renameInMatches xs rs
renameInDecl (x:xs) rs = x : renameInDecl xs rs

-- does the renaming inside a statement list
procStmts :: [Stmt] -> [(String, String)] -> [Stmt]
procStmts [] _ = []
procStmts (s:ss) rs = ps s rs : procStmts ss rs where
    -- renames in one statement
    ps :: Stmt -> [(String, String)] -> Stmt
    ps (Generator x exp) kf = Generator x $ procExp exp kf
    ps (Qualifier exp) kf = Qualifier $ procExp exp kf
    ps (LetStmt (BDecls d)) kf = LetStmt (BDecls $ renameInDecl d kf)
    ps (LetStmt x) kf = LetStmt x
    ps (RecStmt stmts) kf = RecStmt $ procStmts stmts kf

-- does the renaming inside a qualified statement list
procQStmts :: [QualStmt] -> [(String, String)] -> [QualStmt]
procQStmts [] _ = []
procQStmts (qs:qss) rs = pqs qs rs : procQStmts qss rs where
    -- renames in one qualified statement
    pqs :: QualStmt -> [(String, String)] -> QualStmt
    pqs (QualStmt stmt) kf = QualStmt $ head $ procStmts [stmt] kf
    pqs (ThenTrans exp) kf = ThenTrans $ procExp exp kf
    pqs (ThenBy exp1 exp2) kf = ThenBy (procExp exp1 kf) (procExp exp2 kf)
    pqs (GroupBy exp) kf = GroupBy $ procExp exp kf
    pqs (GroupUsing exp) kf = GroupUsing $ procExp exp kf
    pqs (GroupByUsing exp1 exp2) kf = GroupByUsing (procExp exp1 kf) (procExp exp2 kf)

-- does the renaming in an expression
procExp :: Exp -> [(String, String)] -> Exp
procExp (Var name) rs = (case lookup n rs of
    Just nname -> Var $ f name nname
    Nothing -> Var name) where
        n = (case name of Qual _ nam -> getName nam; UnQual nam -> getName nam; _ -> "";)
        f n nn = (case n of Qual x nam -> Qual x (setName nam nn); UnQual nam -> UnQual (setName nam nn); x -> x;)
procExp (App exp1 exp2) rs = App (procExp exp1 rs) (procExp exp2 rs)
procExp (InfixApp exp1 (QVarOp name) exp2) rs = (case lookup n rs of
    Just nname -> InfixApp (procExp exp1 rs) (QVarOp (f name nname)) (procExp exp2 rs) 
    Nothing -> InfixApp (procExp exp1 rs) (QVarOp name) (procExp exp2 rs)) where
        n = (case name of Qual _ nam -> getName nam; UnQual nam -> getName nam; _ -> "";)
        f n nn = (case n of Qual x nam -> Qual x (setName nam nn); UnQual nam -> UnQual (setName nam nn); x -> x;)
procExp (InfixApp exp1 x exp2) rs = InfixApp (procExp exp1 rs) x (procExp exp2 rs)
procExp (Lambda x exp) rs = Lambda x $ procExp exp rs
procExp (Paren exp) rs = Paren $ procExp exp rs
procExp (Let (BDecls decl) exp) rs = Let (BDecls (renameInDecl decl rs)) (procExp exp rs)
procExp (Let x exp) rs = Let x $ procExp exp rs
procExp (If exp1 exp2 exp3) rs = If (procExp exp1 rs) (procExp exp2 rs) (procExp exp3 rs)
procExp (MultiIf grhss) rs = MultiIf $ (\ (GuardedRhss rhss) -> rhss) (rhsChecker (GuardedRhss grhss) rs)
procExp (Case exp alts) rs = Case (procExp exp rs) $ map (\ x -> case x of (Alt a rhs (Just (BDecls dcl))) -> Alt a (rhsChecker rhs rs) (Just (BDecls (renameInDecl dcl rs))); (Alt a rhs binds) -> Alt a (rhsChecker rhs rs) binds) alts
procExp (Do stmts) rs = Do (procStmts stmts rs)
procExp (MDo stmts) rs = MDo (procStmts stmts rs)
procExp (Tuple b exps) rs = Tuple b (map (flip procExp rs) exps)
procExp (TupleSection b mexps) rs = TupleSection b (map (\ x -> case x of Just exp -> Just (procExp exp rs); Nothing -> Nothing) mexps)
procExp (List exps) rs = List (map (flip procExp rs) exps)
procExp (ParArray exps) rs = ParArray (map (flip procExp rs) exps)
procExp (NegApp exp) rs = NegApp $ procExp exp rs
procExp (EnumFrom exp) rs = EnumFrom $ procExp exp rs
procExp (EnumFromTo exp1 exp2) rs = EnumFromTo (procExp exp1 rs) (procExp exp2 rs)
procExp (EnumFromThen exp1 exp2) rs = EnumFromThen (procExp exp1 rs) (procExp exp2 rs)
procExp (EnumFromThenTo exp1 exp2 exp3) rs = EnumFromThenTo (procExp exp1 rs) (procExp exp2 rs) (procExp exp3 rs)
procExp (ParArrayFromTo exp1 exp2) rs = ParArrayFromTo (procExp exp1 rs) (procExp exp2 rs)
procExp (ParArrayFromThenTo exp1 exp2 exp3) rs = ParArrayFromThenTo (procExp exp1 rs) (procExp exp2 rs) (procExp exp3 rs)
procExp (ListComp exp qstmts) rs = ListComp (procExp exp rs) (procQStmts qstmts rs)
procExp x _ = x

---------------- function & function call collection ---------------------------

-- parses all the functions present in the code
parseInFile :: [Decl] -> [Function]
parseInFile xs = parseDec xs []

-- collects all the new functions that haven't been found yet
parseDec :: [Decl] -> [Function] -> [Function]
parseDec [] _ = []
parseDec ((TypeSig names ty):xs) fs = news ++ parseDec xs (news ++ fs) where
    news = map (\ name -> case name of Ident xs -> F xs (showType ty) False; Symbol xs -> F xs (showType ty) False) names
parseDec ((PatBind _ _ binds):xs) fs = news ++ parseDec xs (news ++ fs) where
    news = fromBinds binds fs
parseDec ((FunBind (m:atches)):xs) fs = news ++ parseDec xs (news ++ fs) where
    news = foldr (\ m funs -> case m of (Match name patts _ binds) -> fromBinds binds fs ++ funs; _ -> funs) [] (m:atches)
parseDec (_:xs) fs = parseDec xs fs

-- collects the functions from the binds if there is a bind
fromBinds :: Maybe Binds -> [Function] -> [Function]
fromBinds Nothing _ = []
fromBinds (Just (BDecls dl)) fs = parseDec dl fs
fromBinds _ _ = []

-- collects the function calls of all the functions in the list
functionCalls :: [Function] {-which functions we need-} -> [Function] {-all the functions we know of-} -> [Decl] {-the code-} -> [(Function, [Function])] {-which functions they call-}
functionCalls [] _ _ = []
functionCalls (f:fs) kf dl = (f, nub (getCalls f kf dl)) : functionCalls fs kf dl where
    -- collects the function calls of the given function
    getCalls :: Function -> [Function] -> [Decl] -> [Function]
    getCalls _ [] _ = []
    getCalls _ _ [] = []
    getCalls f kf ((FunBind matches):xs) = fromMatches f matches kf ++ getCalls f kf xs where
        -- collects the function calls from function binds
        fromMatches :: Function -> [Match] -> [Function] -> [Function]
        fromMatches _ [] _ = []
        fromMatches f@(F n t r) ((Match fn _ rhs binds):ms) kf
            | n == getName fn = fromRhs rhs kf ++ callsFromBinds binds kf ++ fromMatches f ms kf
            | otherwise = callsFromBindsF binds f kf ++ fromMatches f ms kf where
                fromRhs :: Rhs -> [Function] -> [Function]
                fromRhs (UnGuardedRhs exp) kf = evalExp exp kf
                fromRhs (GuardedRhss rhss) kf = concatMap (\ (GuardedRhs stmt exp) -> evalExp exp kf ++ procStmt (RecStmt stmt) kf) rhss
        fromMatches f (_:ms) kf = fromMatches f ms kf
    getCalls f@(F n t r) kf ((PatBind (PVar name) rhs binds):xs)
        | n == getName name = callsFromBinds binds kf ++ (case rhs of (UnGuardedRhs exp) -> evalExp exp kf; (GuardedRhss rhss) -> concatMap (\ (GuardedRhs stmt exp) -> evalExp exp kf ++ procStmt (RecStmt stmt) kf) rhss;) ++ getCalls f kf xs
        | otherwise = callsFromBindsF binds f kf ++ getCalls f kf xs
    getCalls f kf (_:xs) = getCalls f kf xs

-- collects the calls of a given function from a bind (from function binds)
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
                | n == getName fn = fromRhs rhs kf ++ callsFromBinds binds kf ++ fromMatches f ms kf
                | otherwise = callsFromBindsF binds f kf ++ fromMatches f ms kf where
                    fromRhs :: Rhs -> [Function] -> [Function]
                    fromRhs (UnGuardedRhs exp) kf = evalExp exp kf
                    fromRhs (GuardedRhss rhss) kf = concatMap (\ (GuardedRhs stmt exp) -> evalExp exp kf ++ procStmt (RecStmt stmt) kf) rhss
            fromMatches f (_:ms) kf = fromMatches f ms kf
        getCallsF f kf (_:xs) = getCallsF f kf xs
callsFromBindsF _ _ _ = []

-- collects all function calls from a bind (from the pattern binds)
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

-- collects all the known functions that are present in the expression
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

-- collects all the known functions that are present in the statement
procStmt :: Stmt -> [Function] -> [Function]
procStmt (Generator _ exp) kf = evalExp exp kf
procStmt (Qualifier exp) kf = evalExp exp kf
procStmt (LetStmt binds) kf = fromBinds (Just binds) kf
procStmt (RecStmt stmts) kf = concatMap ((flip procStmt) kf) stmts

-- collects all the known functions that are present in the qualified statement
procQStmt :: QualStmt -> [Function] -> [Function]
procQStmt (QualStmt stmt) kf = procStmt stmt kf
procQStmt (ThenTrans exp) kf = evalExp exp kf
procQStmt (ThenBy exp1 exp2) kf = evalExp exp1 kf ++ evalExp exp2 kf
procQStmt (GroupBy exp) kf = evalExp exp kf
procQStmt (GroupUsing exp) kf = evalExp exp kf
procQStmt (GroupByUsing exp1 exp2) kf = evalExp exp1 kf ++ evalExp exp2 kf

-------------- graph build --------------------------

-- handles recursion flagging
handleRecursion :: [(Function, [Function])] -> [(Function, [Function])]
handleRecursion xs = flagRecursion (findRec xs) xs where
    -- sets the recursion flag to true, if the function calls itself
    flagRecursion :: [Function] -> [(Function, [Function])] -> [(Function, [Function])]
    flagRecursion [] xs = xs
    flagRecursion (f@(F n t r):fs) xs = flagRecursion fs $ map (setRec f) xs where
        -- sets the recursion flag to true, if needed
        setRec :: Function -> (Function, [Function]) -> (Function, [Function])
        setRec f@(F n t r) (g@(F n1 _ _), fs)
            | n == n1 = (f, delete (F n t False) fs)
            | otherwise = (g, map (\ h@(F n2 t2 r2) -> if n == n2 then f else h) fs)
    -- finds the functions that call themselves
    findRec :: [(Function, [Function])] -> [Function]
    findRec [] = []
    findRec ((f@(F n t r), fs):xs)
        | elem f fs = F n t True : findRec xs
        | otherwise = findRec xs

-- generates the call graphs for each function that's in the list
getCallGraphs :: [String] {-name of functions we need-} -> [Decl] -> [CallGraph]
getCallGraphs fsn decl = filter (\ (Vertex (F n t r) fs) -> elem (dropWhile (=='.') n) fsn) $ map (genGraph dict []) dict where
    pdecl = prep decl
    fs = parseInFile pdecl
    d = functionCalls fs (fs ++ map toFun preludeFuns) pdecl
    dict = handleRecursion d

-- generates the call graph of a given function using the function call dictionary
genGraph :: [(Function, [Function])] -> [Function] -> (Function, [Function]) -> CallGraph
genGraph dict procc (f, fs) = Vertex f $ HS.fromList $ genChilds childs dict (f:procc) where
    childs = findRoots $ filter (\ (g, gs) -> elem g fs) dict
    genChilds :: [Function] -> [(Function, [Function])] -> [Function] -> [CallGraph]
    genChilds [] _ _ = []
    genChilds (f@(F n "" r):fs) d p = Vertex f HS.empty : genChilds fs d p
    genChilds (f:fs) dict processed
        | Just fss <- lookup f dict = genGraph dict processed (f, filter (\ g -> not $ elem g processed) fss) : genChilds fs dict processed
        | otherwise = genChilds fs dict processed

-------------- graph build for whole code -------------------------------

-- finds functions that are not called by others
findRoots :: [(Function, [Function])] -> [Function]
findRoots dict = filtered where
        (fsts, snds) = unzip dict
        filtered = filter (\ f -> not $ elem f (concat snds)) fsts

-- merges up all the calls into one big graph
merge :: [(Function, [Function])] -> CallGraph
merge dict
    | null roots = Vertex (F "dummy" "" False) $ HS.fromList $ getAllCircles graphs []
    | null (drop 1 roots) = head $ map snd $ filter (\ (f, g) -> elem f roots) graphs
    | not (null roots) = Vertex (F "dummy" "" False) $ HS.fromList $ map snd $ filter (\ (f, g) -> elem f roots) graphs where
        roots = findRoots dict
        graphs = map (\ (f, fs) -> (f, genGraph dict [] (f, fs))) dict
        getAllCircles :: [(Function, CallGraph)] -> [CallGraph] -> [CallGraph]
        getAllCircles [] found = found
        getAllCircles ((f, g):gs) found
            | any (inG f) found = getAllCircles gs found
            | otherwise = getAllCircles gs (g:found)

-- creates the graph of the whole code
wholeCodeGraph :: [Decl] -> CallGraph
wholeCodeGraph decl = merge dict where
    pdecl = prep decl
    fs = parseInFile pdecl
    d = functionCalls fs (fs ++ map toFun preludeFuns) pdecl
    dict = handleRecursion d
