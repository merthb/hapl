{-# LANGUAGE GADTs #-}
module Graph where
import Language.Haskell.Exts.Simple
import Data.List
import Data.Hashable
import qualified Data.HashSet as HS

-- this module is for the call-graph algebraic data structure

-- the graph implementation we will use to represent the call-graph
data Graph a = Vertex {node :: a, edges :: HS.HashSet (Graph a)}
    deriving (Eq, Ord)

instance Show a => Show (Graph a) where
    show (Vertex node edges) = case HS.null edges of
        True -> show node
        False -> show node ++ " => " ++ show edges

instance (Hashable a) => Hashable (Graph a) where
    hashWithSalt salt (Vertex val children) = hashWithSalt (hashWithSalt salt val) childrenHash where
        childrenHash = HS.foldr hashWithSalt 0 (HS.map (hashWithSalt salt) children)

-- functions have their unique names, their type signatures and a flag for recursiveness
data Function = F String String Bool
    deriving (Eq, Ord)

instance Hashable Function where
    hashWithSalt salt (F name typ boolVal) = hashWithSalt (hashWithSalt (hashWithSalt salt name) typ) boolVal

instance Show Function where
    show (F name ty r) = name ++ " :: " ++ ty ++ " (rec: " ++ show r ++ ")"

-- our call-graph will be a graph that contains functions for we want to represent the function calls in the code
type CallGraph = Graph Function

------------- some graph and parse functions ----------------------------

inG :: Function -> CallGraph -> Bool
inG f (Vertex v vs)
    | f == v = True
    | otherwise = HS.foldr (||) False $ HS.map (inG f) vs

vertNum :: CallGraph -> Int
vertNum (Vertex f fs) = 1 + HS.foldr (+) 0 (HS.map vertNum fs)

edgeNum :: CallGraph -> Int
edgeNum (Vertex f fs) = HS.size fs + HS.foldr (+) 0 (HS.map edgeNum fs)

insertEdge :: CallGraph -> CallGraph -> CallGraph
insertEdge (Vertex f fs) g = Vertex f (HS.insert g fs)

deleteEdge :: CallGraph -> CallGraph -> CallGraph
deleteEdge (Vertex f fs) g = Vertex f (HS.delete g fs)

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

toType :: [Char] -> Type
toType [x] = TyVar (Ident [x])
toType (x:xs) = TyFun (TyVar (Ident [x])) (toType xs)

preludeFuns :: [String]
preludeFuns = ["case", "if", "length", "map", "filter", "all", "any", "and", "or", "++", "concat", "concatMap", ":", "scanl", "scanr", "iterate", "cycle", "replicate", "repeat", "take", "takeWhile", "drop", "dropWhile", "span", "break", "splitAt", "notElem", "lookup", "zip", "zipWith", "unzip", "lines", "unlines", "words", "unwords", "head", "tail", "last", "init", "!!", "null", "reverse", "max", "min", "maximum", "minimum", "id", ".", "not", "&&", "||", "==", "/=", "<=", ">=", "<", ">", "fst", "snd", "curry", "uncurry", "fromInteger", "toInteger", "even", "odd", "fromIntegral", "foldl", "foldr", "elem", "sum", "product", "const", "flip", "until", "error", "undefined", "show", "read"]

toFun :: String -> Function
toFun xs = F xs "" False

getName :: Name -> String
getName name = case name of Ident xs -> xs; Symbol xs -> xs

setName :: Name -> String -> Name
setName name n = case name of Ident xs -> Ident n; Symbol xs -> Symbol n

---------------- preprocess code files ---------------------------

allDifferent :: [Function] -> Bool
allDifferent [] = True
allDifferent (x:xs) 
    | myelem x xs = False 
    | otherwise = allDifferent xs where
        myelem :: Function -> [Function] -> Bool
        myelem (F fn _ _) fs = not $ null $ filter (\ (F gn _ _) -> fn == gn) fs

prep :: [Decl] -> [Decl]
prep xs = prep' $ preprocFunBinds nxs $ map (\ (F n _ _) -> n) $ parseInFile nxs where
    nxs = renameGlobals xs $ globalScopeFuns xs

prep' :: [Decl] -> [Decl]
prep' xs
    | allDifferent (parseInFile decl) = decl
    | otherwise = prep' decl where
        renames = whatToRename $ parseInFile xs
        decl = doPrep renames xs

doPrep :: [(String, String)] -> [Decl] -> [Decl]
doPrep [] decl = decl
doPrep (rs@(f,nf):xs) decl = case getGlobalDecl f decl of
    Just d -> doPrep xs $ map (\ x -> if x == d then head (renameInDecl [d] [rs]) else x) decl
    _ -> doPrep xs decl

whatToRename :: [Function] -> [(String, String)]
whatToRename xs = concatMap (\ xs -> map (\ (F fn ft fr) -> rename fn) (drop 1 xs)) $ filter (\ as -> case as of [] -> False; [x] -> False; _ -> True) $ groupBy (\ (F fn _ _) (F gn _ _) -> fn == gn) $ sortBy (\ (F fn _ _) (F gn _ _) -> compare fn gn) xs

findLocalScope :: String -> [Decl] -> Bool
findLocalScope f [] = False
findLocalScope fn (t@(TypeSig names ty):ds) = elem fn (map getName names) || findLocalScope fn ds
findLocalScope f (m@(FunBind matches):ds) = inLocals f matches || findLocalScope f ds
findLocalScope f ((PatBind _ _ (Just (BDecls decl))):ds) = findLocalScope f decl || findLocalScope f ds
findLocalScope f (d:ds) = findLocalScope f ds

inLocals :: String -> [Match] -> Bool
inLocals _ [] = False
inLocals f ((Match _ _ _ (Just (BDecls decl))):ms) = findLocalScope f decl || inLocals f ms
inLocals f (m:ms) = inLocals f ms

getGlobalDecl :: String -> [Decl] -> Maybe Decl
getGlobalDecl _ [] = Nothing
getGlobalDecl f (b@(FunBind matches):ds)
    | findLocalScope f (foldr (\ m neut -> case m of (Match _ _ _ (Just (BDecls d))) -> d ++ neut; _ -> neut) [] matches) = Just b
    | otherwise = getGlobalDecl f ds
getGlobalDecl f (p@(PatBind _ _ (Just (BDecls decl))):ds)
    | findLocalScope f decl = Just p
    | otherwise = getGlobalDecl f ds
getGlobalDecl f (d:ds) = getGlobalDecl f ds

preprocFunBinds :: [Decl] -> [String] -> [Decl]
preprocFunBinds [] _ = []
preprocFunBinds (f@(FunBind (m@(Match fn patts _ _):ms)):xs) fs 
    | needsTypeSig m fs = (TypeSig [fn] (toType $ createTypeSig patts ['a'..])) : (FunBind $ recheckMatches (m:ms) fs) : preprocFunBinds xs fs
    | otherwise = (FunBind $ recheckMatches (m:ms) fs) : preprocFunBinds xs fs where
        createTypeSig :: [Pat] -> [Char] -> [Char]
        createTypeSig [] (a:as) = [a]
        createTypeSig (_:xs) (a:as) = a : createTypeSig xs as 
preprocFunBinds (x:xs) fs = x : preprocFunBinds xs fs

needsTypeSig :: Match -> [String] -> Bool
needsTypeSig (Match fn _ _ _) fs = not $ elem (getName fn) fs
needsTypeSig x _ = False

recheckMatches :: [Match] -> [String] -> [Match]
recheckMatches [] _ = []
recheckMatches ((Match fn pb rhs (Just (BDecls d))):xs) fs = (Match fn pb rhs (Just (BDecls (preprocFunBinds d fs)))) : recheckMatches xs fs
recheckMatches (x:xs) fs = x : recheckMatches xs fs

rename :: String -> (String, String)
rename xs = (xs, '.':xs)

renameGlobals :: [Decl] -> [String] -> [Decl]
renameGlobals [] _ = []
renameGlobals ((FunBind matches):xs) fs = (FunBind $ matchRn matches (map rename fs)) : renameGlobals xs ((\ (Match fn _ _ _) -> getName fn) (head matches):fs) where
    matchRn :: [Match] -> [(String, String)] -> [Match]
    matchRn [] _ = []
    matchRn ((Match fn pb rhs binds):ms) ffs = Match (setName fn ('.':getName fn)) pb (rhsChecker rhs ffs) (bindsChecker binds ffs) : matchRn ms ffs
    matchRn (m:ms) ffs = m : matchRn ms ffs
renameGlobals ((TypeSig fns ty):xs) fs = TypeSig (rnNames fns) ty : renameGlobals xs fs where
    rnNames :: [Name] -> [Name]
    rnNames [] = []
    rnNames (x:xs) = setName x ('.':getName x) : rnNames xs
renameGlobals (x:xs) fs = x : renameGlobals xs fs

bindsChecker :: Maybe Binds -> [(String, String)] -> Maybe Binds
bindsChecker (Just (BDecls d)) fs = Just $ BDecls $ renameInDecl d fs
bindsChecker x _ = x

globalScopeFuns :: [Decl] -> [String]
globalScopeFuns [] = []
globalScopeFuns ((FunBind (m:ms)):xs) = getFunName m : globalScopeFuns xs where
    getFunName :: Match -> String
    getFunName (Match fn _ _ _) = getName fn
globalScopeFuns ((PatBind (PVar fn) _ _):xs) = getName fn : globalScopeFuns xs
globalScopeFuns (x:xs) = globalScopeFuns xs

rhsChecker :: Rhs -> [(String, String)] -> Rhs
rhsChecker (UnGuardedRhs exp) rs = UnGuardedRhs $ procExp exp rs
rhsChecker (GuardedRhss rhss) rs = GuardedRhss $ procGuarded rhss rs where
    procGuarded :: [GuardedRhs] -> [(String, String)] -> [GuardedRhs]
    procGuarded [] rs = []
    procGuarded ((GuardedRhs stmts exp):xs) rs = GuardedRhs (procStmts stmts rs) (procExp exp rs) : procGuarded xs rs

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

procStmts :: [Stmt] -> [(String, String)] -> [Stmt]
procStmts [] _ = []
procStmts (s:ss) rs = ps s rs : procStmts ss rs where
    ps :: Stmt -> [(String, String)] -> Stmt
    ps (Generator x exp) kf = Generator x $ procExp exp kf
    ps (Qualifier exp) kf = Qualifier $ procExp exp kf
    ps (LetStmt (BDecls d)) kf = LetStmt (BDecls $ renameInDecl d kf)
    ps (LetStmt x) kf = LetStmt x
    ps (RecStmt stmts) kf = RecStmt $ procStmts stmts kf

procQStmts :: [QualStmt] -> [(String, String)] -> [QualStmt]
procQStmts [] _ = []
procQStmts (qs:qss) rs = pqs qs rs : procQStmts qss rs where
    pqs :: QualStmt -> [(String, String)] -> QualStmt
    pqs (QualStmt stmt) kf = QualStmt $ head $ procStmts [stmt] kf
    pqs (ThenTrans exp) kf = ThenTrans $ procExp exp kf
    pqs (ThenBy exp1 exp2) kf = ThenBy (procExp exp1 kf) (procExp exp2 kf)
    pqs (GroupBy exp) kf = GroupBy $ procExp exp kf
    pqs (GroupUsing exp) kf = GroupUsing $ procExp exp kf
    pqs (GroupByUsing exp1 exp2) kf = GroupByUsing (procExp exp1 kf) (procExp exp2 kf)

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

parseInFile :: [Decl] -> [Function]
parseInFile xs = parseDec xs []

parseDec :: [Decl] -> [Function] -> [Function]
parseDec [] _ = []
parseDec ((TypeSig names ty):xs) fs = news ++ parseDec xs (news ++ fs) where
    news = map (\ name -> case name of Ident xs -> F xs (showType ty) False; Symbol xs -> F xs (showType ty) False) names
parseDec ((PatBind _ _ binds):xs) fs = news ++ parseDec xs (news ++ fs) where
    news = fromBinds binds fs
parseDec ((FunBind (m:atches)):xs) fs = news ++ parseDec xs (news ++ fs) where
    news = foldr (\ m funs -> case m of (Match name patts _ binds) -> fromBinds binds fs ++ funs; _ -> funs) [] (m:atches)
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

-------------- graph build --------------------------

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
getCallGraphs fsn decl = filter (\ (Vertex (F n t r) fs) -> elem (dropWhile (=='.') n) fsn) $ map (genGraph dict []) dict where
    pdecl = prep decl
    d = functionCalls (parseInFile pdecl) (parseInFile pdecl ++ map toFun preludeFuns) pdecl
    dict = flagRecursion (findRec d) d

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

merge :: [(Function, [Function])] -> CallGraph
merge dict
    | null roots = error "circle in whole code"
    | null (drop 1 roots) = head $ map snd $ filter (\ (f, g) -> elem f roots) graphs
    | not (null roots) = Vertex (F "dummy" "" False) $ HS.fromList $ map snd $ filter (\ (f, g) -> elem f roots) graphs where
        roots = findRoots dict
        graphs = map (\ (f, fs) -> (f, genGraph dict [] (f, fs))) dict

wholeCodeGraph :: [Decl] -> CallGraph
wholeCodeGraph decl = merge dict where
    pdecl = prep decl
    d = functionCalls (parseInFile pdecl) (parseInFile pdecl ++ map toFun preludeFuns) pdecl
    dict = flagRecursion (findRec d) d
