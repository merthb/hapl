module Main (main) where

import System.IO
import System.Exit
import System.Directory
import System.FilePath
import System.FilePath.Glob
import System.Environment
import Graph
import Algorithm
import Language.Haskell.Exts.Simple
import Data.List
import Data.Char
import Control.Exception
import Control.DeepSeq
import Control.Parallel.Strategies

type FunName = String

main :: IO ()
main = do
    args <- getArgs
    case handleArgs args of
        Just ([], []) -> do
            putStrLn "HAPL - Haskell Plágiumellenőrző szoftver \n"
            putStrLn "Ez egy konzolos alkalmazás, amely specifikusan Haskell nyelvben írt programok közt keresi az egyezéseket."
            putStrLn "A programok struktúráját vizsgálja, felépíti azok függvényhívási gráfját, majd részgráf egyezéseket keresve"
            putStrLn "adja meg az egyezési százalékot.\n"
            loop
        Just ([], paths) -> do
            files <- mapM glob paths
            codes' <- parseAllCodesAll (concat files)
            putStrLn "\nProgramok teljes gráfjainak egyezési százalékai:\n"
            putStrLn $ writeResAll (runParallel codes') 1
            putStrLn "Nyomjon ENTER-t a kilépéshez!"
            x <- getLine
            exitSuccess
        Just (fs, paths) -> do
            files <- mapM glob paths
            codes <- parseAllCodes (concat files) fs
            putStrLn "A függvényenként felépített gráfok egyezési százalékai:\n"
            putStrLn $ writeRes (runParallel codes) 1 fs
            putStrLn ""
            codes' <- parseAllCodesAll (concat files)
            putStrLn "\nProgramok teljes gráfjainak egyezési százalékai:\n"
            putStrLn $ writeResAll (runParallel codes') 1
            putStrLn "Nyomjon ENTER-t a kilépéshez!"
            x <- getLine
            exitSuccess
        Nothing -> do
            putStrLn "A paraméter átadás nem megfelelő! A függvényneveket '-functions' paraméter után, a fájlokat '-files' paraméter után kell felsorolni."
            putStrLn "Nyomjon ENTER-t a kilépéshez!"
            x <- getLine
            exitFailure

handleArgs :: [String] -> Maybe ([String], [String])
handleArgs [] = Just ([],[])
handleArgs ("-functions":xs) = Just (takeWhile (\x -> x /= "-files") xs, drop 1 (dropWhile (\x -> x /= "-files") xs))
handleArgs ("-files":xs) = Just (drop 1 (dropWhile (\x -> x /= "-functions") xs), takeWhile (\x -> x /= "-functions") xs)
handleArgs _ = Nothing

loop :: IO ()
loop = do
    oneTimeRunner
    putStrLn "Válasszon az alábbi lehetőségek közül, majd írja be a választott menüpont számát!"
    m <- getMenuItem
    case m of
        1 -> loop
        2 -> exitSuccess

getMenuItem :: IO Int
getMenuItem = do
    putStrLn "[1] - Újabb ellenőrzés futtatása"
    putStrLn "[2] - Kilépés\n"
    x <- getLine
    case x of
        "1" -> pure 1
        "2" -> pure 2
        _ -> do
            putStrLn "\nA megadott érték nem megfelelő, kérem a választott menüpont számát üsse be!"
            getMenuItem

oneTimeRunner :: IO ()
oneTimeRunner = do
    putStrLn "Az ellenőrzés futtatásához szükséges megadni az alábbi imformációkat:"
    putStrLn "* Azon függvények neveit, amelyekre önálló ellenőrzést is futtatni kíván."
    putStrLn "* Az ellenőrizni kívánt fájlok elérési útjait (lehet globális minta is).\n"
    (fs, paths) <- readInput
    case fs of
        [] -> do
            codes' <- parseAllCodesAll paths
            putStrLn "\nProgramok teljes gráfjainak egyezési százalékai:\n"
            putStrLn $ writeResAll (runParallel codes') 1
            putStrLn ""
        _ -> do
            codes <- parseAllCodes paths fs
            putStrLn "\nA függvényenként felépített gráfok egyezési százalékai:\n"
            putStrLn $ writeRes (runParallel codes) 1 fs
            putStrLn ""
            codes' <- parseAllCodesAll paths
            putStrLn "\nProgramok teljes gráfjainak egyezési százalékai:\n"
            putStrLn $ writeResAll (runParallel codes') 1
            putStrLn ""

writeRes :: [Response] -> Int -> [FunName] -> String
writeRes [] _ _ = ""
writeRes (x:xs) n fs = writeResH x n fs ++ "\n" ++ writeRes xs (n + 1) fs where
    writeResH :: Response -> Int -> [FunName] -> String
    writeResH (id1, id2, matches) i fs = show i ++ ". " ++ show id1 ++ " <->\n" ++ show id2 ++ ":\n" ++ showMatches matches fs where
        showMatches :: [(String, MatchNum)] -> [FunName] -> String
        showMatches [] _ = ""
        showMatches _ [] = ""
        showMatches resps (x:xs)
            | Just num <- lookup x resps = '\t' : x ++ ": " ++ show num ++ "%\n" ++ showMatches resps xs
            | otherwise = showMatches resps xs

writeResAll :: [Response] -> Int -> String
writeResAll [] _ = ""
writeResAll (x:xs) n = writeResH x n ++ "\n" ++ writeResAll xs (n + 1) where
    writeResH :: Response -> Int -> String
    writeResH (id1, id2, matches) i = show i ++ ". " ++ show id1 ++ " <->\n" ++ show id2 ++ ":\n" ++ showMatches matches where
        showMatches :: [(String, MatchNum)] -> String
        showMatches [] = ""
        showMatches ((f, num):xs) = '\t' : "egyezés: " ++ show num ++ "%\n" ++ showMatches xs

catchAny :: IO a -> (SomeException -> IO a) -> IO a
catchAny = Control.Exception.catch

runParallel :: [Code] -> [Response]
runParallel = parMap rdeepseq runAlg . makePairs

runAlg :: (Code, Code) -> Response
runAlg ((id1, g), (id2, h)) =
    let matchnums = map (\ (fn, x, y) -> (fn, mainAlgorithm x y)) (zipOnFunName g h)
    in (id1, id2, matchnums)

parseCode :: FilePath -> [FunName] -> IO (Maybe Code)
parseCode p fs = do
    x <- parseFile p
    case x of 
        ParseOk (Module _ _ _ d) -> pure $ Just (p, getCallGraphs fs d)
        _ -> pure Nothing

parseAllCodes :: [FilePath] -> [FunName] -> IO [Code]
parseAllCodes [] _ = pure []
parseAllCodes (x:xs) fs = do
    code <- catchAny (parseCode x fs) $ \e -> do putStrLn (x ++ ": fájl beolvasása sikertelen, a program nem kezeli."); pure Nothing
    cs <- parseAllCodes xs fs
    case code of
        Just c -> pure (c:cs)
        Nothing -> pure cs

-- whole code graph parser
parseCodeAll :: FilePath -> IO (Maybe Code)
parseCodeAll p = do
    x <- parseFile p
    case x of
        ParseOk (Module _ _ _ d) -> pure $ Just (p, [wholeCodeGraph d])
        _ -> pure Nothing

-- whole code graph parser
parseAllCodesAll :: [FilePath] -> IO [Code]
parseAllCodesAll [] = pure []
parseAllCodesAll (x:xs) = do
    code <- catchAny (parseCodeAll x) $ \e -> do putStrLn (x ++ ": fájl beolvasása sikertelen, a program nem kezeli."); pure Nothing
    cs <- parseAllCodesAll xs
    case code of
        Just c -> pure (c:cs)
        Nothing -> pure cs

parseFilePath :: IO FilePath
parseFilePath = do
    x <- getLine
    case snd (System.FilePath.splitExtension x) of
        ".txt" -> do
            b <- System.Directory.doesFileExist x
            if b 
                then 
                    pure x
                else 
                    do
                        putStrLn "\nA megadott fájl nem létezik, ellenőrizze az elérési utat, majd próbálkozzon újra:"
                        parseFilePath
        _ -> do
                putStrLn "\nA megadott elrési úton nem szöveges fájl található, ellenőrizze az elérési utat, majd próbálkozzon újra:"
                parseFilePath

readInput :: IO ([FunName], [FilePath])
readInput = do
    fs <- getFuns
    n <- getNum
    putStrLn "Adja meg az elérési utakat egymás után új sorban!"
    files <- getFiles n
    pure (fs, files)

getNum :: IO Int
getNum = do
    putStrLn "Adja meg, hány elérési úton találhatóak az ellenőrizendő fájlok!"
    x <- getLine
    if all isDigit x && not (null x)
        then if x /= ['0'] 
            then pure (read x)
            else do 
                putStrLn "A kapott érték nem pozitív szám!"
                getNum
        else do 
            putStrLn "A kapott érték nem pozitív szám!"
            getNum

getFuns :: IO [FunName]
getFuns = do
    putStrLn "Adja meg a külön összevetendő függvények neveit szóközzel elválasztva!"
    putStrLn "Ügyeljen a helyesírásra, csak azok a függvények kerülnek az eredménybe, amelyek ténylegesen szerepelnek a kódokban."
    x <- getLine
    pure (words x)

getFiles :: Int -> IO [FilePath]
getFiles 0 = pure []
getFiles n = do
    fs <- getFile
    fss <- getFiles (n - 1)
    pure (fs ++ fss)

getFile :: IO [FilePath]
getFile = do
    x <- getLine
    if not (null x)
        then do
            fs <- glob x
            if null fs
                then if System.FilePath.isValid x
                    then pure [x]
                    else do 
                        putStrLn "Invalid elérési út, próbálja újra!"
                        getFile
                else pure fs
        else do 
            putStrLn "Invalid elérési út, próbálja újra!"
            getFile