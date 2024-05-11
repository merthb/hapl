{-# LANGUAGE GADTs #-}
module Graph (Graph (..),Function (..),CallGraph,preludeFuns,toFun,allDifferent,parseInFile,prep,functionCalls,handleRecursion,getCallGraphs,wholeCodeGraph,inG,vertNum,edgeNum,startsWith,deleteEdge,insertEdge) where
import Language.Haskell.Exts.Simple
import Data.List
import Data.Hashable
import qualified Data.HashSet as HS

-- ez a modul a gráfépítés műveleteit tartalmazza


data Graph a = Vertex {node :: a, edges :: HS.HashSet (Graph a)}
    deriving (Show, Eq, Ord)

instance (Hashable a) => Hashable (Graph a) where
    hashWithSalt salt (Vertex val children) = hashWithSalt (hashWithSalt salt val) childrenHash where
        childrenHash = HS.foldr hashWithSalt 0 (HS.map (hashWithSalt salt) children)

data Function = F String String Bool
    deriving (Show, Eq, Ord)

instance Hashable Function where
    hashWithSalt salt (F name typ boolVal) = hashWithSalt (hashWithSalt (hashWithSalt salt name) typ) boolVal

type CallGraph = Graph Function

------------- általánosan hasznos műveletek ----------------------------

-- eldönti, hogy egy adott függvény megtalálható-e a gráf bármely csúcsában
inG :: Function -> CallGraph -> Bool
inG f (Vertex v vs)
    | f == v = True
    | otherwise = HS.foldr (||) False $ HS.map (inG f) vs

-- a gráf csúcsainak számosságát határozza meg
vertNum :: CallGraph -> Int
vertNum (Vertex f fs) = 1 + HS.foldr (+) 0 (HS.map vertNum fs)

-- a gráf éleinek számosságát határozza meg
edgeNum :: CallGraph -> Int
edgeNum (Vertex f fs) = HS.size fs + HS.foldr (+) 0 (HS.map edgeNum fs)

-- beilleszti az első gráf fő csúcsának élei közé a második gráfot
insertEdge :: CallGraph -> CallGraph -> CallGraph
insertEdge (Vertex f fs) g = Vertex f (HS.insert g fs)

-- kitörli az első gráf fő csúcsának élei közül a második gráfot
deleteEdge :: CallGraph -> CallGraph -> CallGraph
deleteEdge (Vertex f fs) g = Vertex f (HS.delete g fs)

-- eldönti, hogy a gráf fő csúcsában az adott függvény szerepel-e
startsWith :: Function -> CallGraph -> Bool
startsWith f (Vertex g _) = f == g

-- egy Type típusú értéket szöveggé alakít
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

-- karakterek listájából előállít egy Type típusú értéket
toType :: String -> Type
toType [x] = TyVar (Ident [x])
toType (x:xs) = TyFun (TyVar (Ident [x])) (toType xs)

-- A Prelude modul függvényeinek nevei listába gyűjtve
preludeFuns :: [String]
preludeFuns = ["case", "if", "length", "map", "filter", "all", "any", "and", "or", "++", "concat", "concatMap", ":", "scanl", "scanr", "iterate", "cycle", "replicate", "repeat", "take", "takeWhile", "drop", "dropWhile", "span", "break", "splitAt", "notElem", "lookup", "zip", "zipWith", "unzip", "lines", "unlines", "words", "unwords", "head", "tail", "last", "init", "!!", "null", "reverse", "max", "min", "maximum", "minimum", "id", ".", "not", "&&", "||", "==", "/=", "<=", ">=", "<", ">", "fst", "snd", "curry", "uncurry", "fromInteger", "toInteger", "even", "odd", "fromIntegral", "foldl", "foldr", "elem", "sum", "product", "const", "flip", "until", "error", "undefined", "show", "read"]

-- előállít egy üres típusszignatúrájú, nem rekurzív függvényt az alábbi névvel
toFun :: String -> Function
toFun xs = F xs "" False

-- kinyeri a szöveget a Name típusú értékből
getName :: Name -> String
getName name = case name of Ident xs -> xs; Symbol xs -> xs

-- átállítja a szöveget a Name típusú értékben a megadott szövegre
setName :: Name -> String -> Name
setName name n = case name of Ident xs -> Ident n; Symbol xs -> Symbol n

---------------- előfeldolgozás ---------------------------

-- eldönti, hogy a lista összes függvényének különbözik-e a neve
allDifferent :: [Function] -> Bool
allDifferent [] = True
allDifferent (x:xs) 
    | myelem x xs = False 
    | otherwise = allDifferent xs where
        myelem :: Function -> [Function] -> Bool
        myelem (F fn _ _) fs = not $ null $ filter (\ (F gn _ _) -> fn == gn) fs

-- ez a függvény végzi el az előfeldolgozást
-- ad típusszignatúrát azoknak a függvényeknek, amelyek még nem rendelkeznek eggyel
-- majd átnevezi a lokális szkópban levő függvények közül azokat, amelyekből több is van a kódban
prep :: [Decl] -> [Decl]
prep xs = prep' $ addTypeSig xs $ map (\ (F n _ _) -> n) $ parseInFile xs where
    -- ez a függvény végzi el mindaddig az átnevezéseket, míg minden függvény neve különböző nem lesz
    -- ez legtöbb esetben nem igényel rekurziót
    prep' :: [Decl] -> [Decl]
    prep' xs
        | allDifferent (parseInFile decl) = decl
        | otherwise = prep' decl where
            renames = whatToRename $ parseInFile xs
            decl = doPrep renames xs
            -- ez a függvény hajtja végre egyszer az átnevezéseket
            doPrep :: [(String, String)] -> [Decl] -> [Decl]
            doPrep [] decl = decl
            doPrep (rs:xs) decl = doPrep xs (doFirst decl rs)
            -- megkeresi azokat a függvényeket, amelyekből egynél több van
            -- majd minden ilyen csoportból egy elemet levesz, a többihez pedig legenerálja az új neveket
            whatToRename :: [Function] -> [(String, String)]
            whatToRename xs = concatMap (\ xs -> genRenames (drop 1 xs) 1) $ filter (\ as -> case as of [] -> False; [x] -> False; _ -> True) $ groupBy (\ (F fn _ _) (F gn _ _) -> fn == gn) $ sortBy (\ (F fn _ _) (F gn _ _) -> compare fn gn) xs where
                -- minden függvény neve elé annyi '.'-ot tesz, ahanyadik elem a listában
                genRenames :: [Function] -> Int -> [(String, String)]
                genRenames [] _ = []
                genRenames ((F x _ _):xs) n = (x, replicate n '.' ++ x) : genRenames xs (n + 1)
    -- beteszi a megfelelő típusszignatúrákat azon függvények elé, amelyek még nem rendelkeznek eggyel
    addTypeSig :: [Decl] -> [String] -> [Decl]
    addTypeSig [] _ = []
    addTypeSig ((FunBind (m@(Match fn patts _ _):ms)):xs) fs 
        | not $ elem (getName fn) fs = (TypeSig [fn] (toType $ createTypeSig (patts) ['a'..])) : (FunBind $ recheckMatches (m:ms) fs) : addTypeSig xs fs
        | otherwise = (FunBind $ recheckMatches (m:ms) fs) : addTypeSig xs fs where
            -- előállít egy típusszignatúrát annyi paraméterrel, ahány eleme van az első listának
            createTypeSig :: [Pat] -> [Char] -> [Char]
            createTypeSig [] (a:as) = [a]
            createTypeSig (_:xs) (a:as) = a : createTypeSig xs as
            -- ellenőrzi a függvénydefiníciókat
            recheckMatches :: [Match] -> [String] -> [Match]
            recheckMatches [] _ = []
            recheckMatches ((Match fn pb rhs (Just (BDecls d))):xs) fs = (Match fn pb rhs (Just (BDecls (addTypeSig d fs)))) : recheckMatches xs fs
            recheckMatches (x:xs) fs = x : recheckMatches xs fs
    addTypeSig ((PatBind fn rhs (Just (BDecls decl))):xs) fs = PatBind fn rhs (Just (BDecls (addTypeSig decl fs))) : addTypeSig xs fs
    addTypeSig (x:xs) fs = x : addTypeSig xs fs
    -- ez a függvény nevezi át az első lokális szkópban elhelyezkedő függvényt, amelynek neve egyezik az átnevezendő függvényével
    -- ehhez megkeresi azt a globális szkópbeli elemet, amelynek lokális szkópjában jelen van
    doFirst :: [Decl] -> (String, String) -> [Decl]
    doFirst decls rn@(fn, nfn) = (case getGlobalDecl fn decls of
        Just d -> updateDecl decls d $ renameFirst d rn
        Nothing -> decls) where
            -- Ez a függvény a megtalált globális szkópbeli elemben végzi el az átnevezést
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
            -- eldönti, hogy az adott szkópban található-e a keresett függvény típusszignatúrája (és ezzel definíciója is egyben)
            findTypeSigScope :: [Decl] -> String -> Bool
            findTypeSigScope [] _ = False
            findTypeSigScope ((TypeSig names ty):xs) fn = any (\ n -> getName n == fn) names || findTypeSigScope xs fn
            findTypeSigScope (x:xs) fn = findTypeSigScope xs fn
            -- visszaadja azt a globális szkópbeli elemet, amelynek valamelyik lokális szkópjában megtalálható az adott függvény, amennyiben van ilyen
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
            -- lecseréli a listában az elsőnek megadott elemet a másodikra
            updateDecl :: [Decl] -> Decl -> Decl -> [Decl]
            updateDecl [] _ _ = []
            updateDecl (x:xs) d nd
                | x == d = nd : xs
                | otherwise = x : updateDecl xs d nd

-- átnevezi az egyenlőség jobb oldalán azokat a függvényeket, amik átnevezésre szorulnak
rhsChecker :: Rhs -> [(String, String)] -> Rhs
rhsChecker (UnGuardedRhs exp) rs = UnGuardedRhs $ procExp exp rs
rhsChecker (GuardedRhss rhss) rs = GuardedRhss $ procGuarded rhss rs where
    -- amennyiben őrfeltételekkel definiált, másképpen kell kezelni
    procGuarded :: [GuardedRhs] -> [(String, String)] -> [GuardedRhs]
    procGuarded [] rs = []
    procGuarded ((GuardedRhs stmts exp):xs) rs = GuardedRhs (procStmts stmts rs) (procExp exp rs) : procGuarded xs rs

-- eldönti, hogy egy adott függvény jelen van-e az adott Decl elemben
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

-- átnevezi az összes átnevezendő függvényt az új nevére egy adott szkópban
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
    -- elvégzi az átnevezést a függvénydefiníciókban és a lokális szkópokban
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

-- elvégzi az átnevezést egy állítás listában
procStmts :: [Stmt] -> [(String, String)] -> [Stmt]
procStmts [] _ = []
procStmts (s:ss) rs = ps s rs : procStmts ss rs where
    -- elvégzi az átnevezést egyetlen állításban
    ps :: Stmt -> [(String, String)] -> Stmt
    ps (Generator x exp) kf = Generator x $ procExp exp kf
    ps (Qualifier exp) kf = Qualifier $ procExp exp kf
    ps (LetStmt (BDecls d)) kf = LetStmt (BDecls $ renameInDecl d kf)
    ps (LetStmt x) kf = LetStmt x
    ps (RecStmt stmts) kf = RecStmt $ procStmts stmts kf

-- elvégzi az átnevezést egy kvalifikált állítás listában
procQStmts :: [QualStmt] -> [(String, String)] -> [QualStmt]
procQStmts [] _ = []
procQStmts (qs:qss) rs = pqs qs rs : procQStmts qss rs where
    -- elvégzi az átnevezést egyetlen kvalifikált állításban
    pqs :: QualStmt -> [(String, String)] -> QualStmt
    pqs (QualStmt stmt) kf = QualStmt $ head $ procStmts [stmt] kf
    pqs (ThenTrans exp) kf = ThenTrans $ procExp exp kf
    pqs (ThenBy exp1 exp2) kf = ThenBy (procExp exp1 kf) (procExp exp2 kf)
    pqs (GroupBy exp) kf = GroupBy $ procExp exp kf
    pqs (GroupUsing exp) kf = GroupUsing $ procExp exp kf
    pqs (GroupByUsing exp1 exp2) kf = GroupByUsing (procExp exp1 kf) (procExp exp2 kf)

-- elvégzi az átnevezést egy kifejezésben
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

---------------- függvények és függvényhívások kigyűjtése ---------------------------

-- kigyűjti az összes típusszignatúrával rendelkező függvényt
parseInFile :: [Decl] -> [Function]
parseInFile xs = parseDec xs []

-- összeszed minden olyan típusszignatúrával rendelkező függvényt, amely még nem szerepel a második listában
parseDec :: [Decl] -> [Function] -> [Function]
parseDec [] _ = []
parseDec ((TypeSig names ty):xs) fs = news ++ parseDec xs (news ++ fs) where
    news = map (\ name -> case name of Ident xs -> F xs (showType ty) False; Symbol xs -> F xs (showType ty) False) names
parseDec ((PatBind _ _ binds):xs) fs = news ++ parseDec xs (news ++ fs) where
    news = fromBinds binds fs
parseDec ((FunBind (m:atches)):xs) fs = news ++ parseDec xs (news ++ fs) where
    news = foldr (\ m funs -> case m of (Match name patts _ binds) -> fromBinds binds fs ++ funs; _ -> funs) [] (m:atches)
parseDec (_:xs) fs = parseDec xs fs

-- amennyiben van lokális szkóp, összeszedi belőle az összes típusszignatúrával rendelkező függvényt, amely még nem szerepel a második listában
fromBinds :: Maybe Binds -> [Function] -> [Function]
fromBinds Nothing _ = []
fromBinds (Just (BDecls dl)) fs = parseDec dl fs
fromBinds _ _ = []

-- összegyűjti minden első listában átadott függvényhez, hogy mely függvényeket hívják meg a második listából
functionCalls :: [Function] {-amiknek a függvényhívásait keressük-} -> [Function] {-minden ismert függvény-} -> [Decl] -> [(Function, [Function])]
functionCalls [] _ _ = []
functionCalls (f:fs) kf dl = (f, nub (getCalls f kf dl)) : functionCalls fs kf dl where
    -- összeszedi egy adott függvényhez, hogy a listából mely függvényeket hívja meg
    getCalls :: Function -> [Function] -> [Decl] -> [Function]
    getCalls _ [] _ = []
    getCalls _ _ [] = []
    getCalls f kf ((FunBind matches):xs) = fromMatches f matches kf ++ getCalls f kf xs where
        -- összeszedi a függvényhívásokat a függvénydefinícióban
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

-- összegyűjti a függvényhívásokat a lokális szkóp függvénydefinícióiból, amennyiben az az adott függvény definíciója
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

-- összegyűjti a függvényhívásokat a lokális szkóp mintaillesztéseiből
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

-- összegyűjti az összes ismert függvényt, amely megtalálható a kifejezésben
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

-- összegyűjti az összes ismert függvényt, amely megtalálható az állításban
procStmt :: Stmt -> [Function] -> [Function]
procStmt (Generator _ exp) kf = evalExp exp kf
procStmt (Qualifier exp) kf = evalExp exp kf
procStmt (LetStmt binds) kf = fromBinds (Just binds) kf
procStmt (RecStmt stmts) kf = concatMap ((flip procStmt) kf) stmts

-- összegyűjti az összes ismert függvényt, amely megtalálható a kvalifikált állításban
procQStmt :: QualStmt -> [Function] -> [Function]
procQStmt (QualStmt stmt) kf = procStmt stmt kf
procQStmt (ThenTrans exp) kf = evalExp exp kf
procQStmt (ThenBy exp1 exp2) kf = evalExp exp1 kf ++ evalExp exp2 kf
procQStmt (GroupBy exp) kf = evalExp exp kf
procQStmt (GroupUsing exp) kf = evalExp exp kf
procQStmt (GroupByUsing exp1 exp2) kf = evalExp exp1 kf ++ evalExp exp2 kf

-- elvégzi a rekurzivitás helyes kezelését
handleRecursion :: [(Function, [Function])] -> [(Function, [Function])]
handleRecursion xs = flagRecursion (findRec xs) xs where
    -- minden önmagára hivatkozó függvénynek igazra állítja a rekurzivitását a teljes szótárban
    flagRecursion :: [Function] -> [(Function, [Function])] -> [(Function, [Function])]
    flagRecursion [] xs = xs
    flagRecursion (f@(F n t r):fs) xs = flagRecursion fs $ map (setRec f) xs where
        -- elvégzi a beállítást egyetlen szótárbeli elemen
        setRec :: Function -> (Function, [Function]) -> (Function, [Function])
        setRec f@(F n t r) (g@(F n1 _ _), fs)
            | n == n1 = (f, delete (F n t False) fs)
            | otherwise = (g, map (\ h@(F n2 t2 r2) -> if n == n2 then f else h) fs)
    -- kigyűjti az önmagukra hivatkozó függvényeket
    findRec :: [(Function, [Function])] -> [Function]
    findRec [] = []
    findRec ((f@(F n t r), fs):xs)
        | elem f fs = F n t True : findRec xs
        | otherwise = findRec xs

-------------- gráfépítés --------------------------

-- előállítja minden listában szereplő függvény függvényhívási gráfját
getCallGraphs :: [String] {-függvénynevek-} -> [Decl] -> [CallGraph]
getCallGraphs fsn decl = filter (\ (Vertex (F n t r) fs) -> elem (dropWhile (=='.') n) fsn) $ map (genGraph dict []) dict where
    pdecl = prep decl
    fs = parseInFile pdecl
    d = functionCalls fs (fs ++ map toFun preludeFuns) pdecl
    dict = handleRecursion d

-- előállítja egy függvény gráfját a szótár segítségével
genGraph :: [(Function, [Function])] -> [Function] -> (Function, [Function]) -> CallGraph
genGraph dict procc (f, fs) = Vertex f $ HS.fromList $ genChilds childs dict (f:procc) where
    childs = findRoots $ filter (\ (g, gs) -> elem g fs) dict
    genChilds :: [Function] -> [(Function, [Function])] -> [Function] -> [CallGraph]
    genChilds [] _ _ = []
    genChilds (f@(F n "" r):fs) d p = Vertex f HS.empty : genChilds fs d p
    genChilds (f:fs) dict processed
        | Just fss <- lookup f dict = genGraph dict processed (f, filter (\ g -> not $ elem g processed) fss) : genChilds fs dict processed
        | otherwise = genChilds fs dict processed

-- megkeresi a fő függvényeket (azokat, amiket nem hív meg egyetlen másik függvény sem)
findRoots :: [(Function, [Function])] -> [Function]
findRoots dict = filtered where
        (fsts, snds) = unzip dict
        filtered = filter (\ f -> not $ elem f (concat snds)) fsts

-- összeilleszti a fő függvények gráfjait egyetlen "dummy root" alá
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

-- előállítja a függvényhívási gráfot a teljes programhoz
wholeCodeGraph :: [Decl] -> CallGraph
wholeCodeGraph decl = merge dict where
    pdecl = prep decl
    fs = parseInFile pdecl
    d = functionCalls fs (fs ++ map toFun preludeFuns) pdecl
    dict = handleRecursion d
