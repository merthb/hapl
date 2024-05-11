module Main (main) where

import System.IO
import System.Exit
import System.Directory
import Graph
import Algorithm
import Language.Haskell.Exts.Simple
import System.FilePath
import Control.Parallel.Strategies
import Data.List
import System.Environment
import Control.Exception

main :: IO ()
main = do
    args <- getArgs
    case args of
        [] -> do
            putStrLn "HAPL - Haskell Plágiumellenőrző szoftver \n"
            putStrLn "Ez egy konzolos alkalmazás, amely specifikusan Haskell nyelvben írt programok közt keresi az egyezéseket."
            putStrLn "A programok struktúráját vizsgálja, felépíti azok függvényhívási gráfját, majd részgráf egyezéseket keresve"
            putStrLn "adja meg az egyezési százalékot.\n"
            loop
        (file:_) -> do
            b <- System.Directory.doesFileExist file
            case b of
                True -> do
                    content <- readFile file
                    case (words $ head $ lines content) of
                        [] -> do
                            codes' <- parseAllCodes' (filter System.FilePath.isValid $ tail $ lines content)
                            putStrLn "Programok teljes gráfjainak egyezési százalékai:\n"
                            putStrLn $ writeResAll (runOnAll codes') 1
                        _ -> do
                            codes <- parseAllCodes (filter System.FilePath.isValid $ tail $ lines content) (words $ head $ lines content)
                            resps <- runParallel codes
                            putStrLn "A függvényenként felépített gráfok egyezési százalékai:\n"
                            putStrLn $ writeRes resps 1 (words $ head $ lines content)
                            putStrLn ""
                            codes' <- parseAllCodes' (filter System.FilePath.isValid $ tail $ lines content)
                            resps' <- runParallel codes'
                            putStrLn "\nProgramok teljes gráfjainak egyezési százalékai:\n"
                            putStrLn $ writeResAll resps' 1
                False -> do
                    putStrLn "Invalid paraméter: A megadott fájl nem létezik."
                    exitFailure

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
    putStrLn "Az ellenőrzés futtatásához szükséges megadni egy szöveges fájl (.txt) elérési útját, amelyben az alábbi imformációk szerepelnek:"
    putStrLn "* A fájl első sora tartalmazza a függvények nevét, amelyekre önálló ellenőrzést is futtatni kíván."
    putStrLn "  Amennyiben nem kíván önálló ellenőrzéseket is futtatni, úgy ezt a sort hagyja üresen."
    putStrLn "* A fájl második sorától kezdve minden sor egy ellenőrizni kívánt fájl elérési útja kell legyen.\n"
    putStrLn "Adja meg az információkat tartalmazó szöveges fájl elérési útját:"
    (fs, paths) <- filePathReader
    case fs of
        [] -> do
            codes' <- parseAllCodes' paths
            putStrLn "\nProgramok teljes gráfjainak egyezési százalékai:\n"
            putStrLn $ writeResAll (runOnAll codes') 1
            putStrLn ""
        _ -> do
            codes <- parseAllCodes paths fs
            resps <- runParallel codes
            putStrLn "\nA függvényenként felépített gráfok egyezési százalékai:\n"
            putStrLn $ writeRes resps 1 fs
            putStrLn ""
            codes' <- parseAllCodes' paths
            resps' <- runParallel codes'
            putStrLn "\nProgramok teljes gráfjainak egyezési százalékai:\n"
            putStrLn $ writeResAll resps' 1
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
        showMatches ((f, num):xs) = '\t' : "match" ++ ": " ++ show num ++ "%\n" ++ showMatches xs

type FunName = String

runParallel :: [Code] -> IO [Response]
runParallel = sequence . parMap rpar runAlg . makePairs

catchAny :: IO a -> (SomeException -> IO a) -> IO a
catchAny = Control.Exception.catch

runAlg :: (Code, Code) -> IO Response
runAlg ((id1, g), (id2, h)) = do
    matchnums <- sequence $ map (\ (fn, x, y) -> pure $ (fn, mainAlgorithm x y)) (zipOnFunName g h)
    pure (id1, id2, matchnums)

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
parseCode' :: FilePath -> IO (Maybe Code)
parseCode' p = do
    x <- parseFile p
    case x of
        ParseOk (Module _ _ _ d) -> pure $ Just (p, [wholeCodeGraph d])
        _ -> pure Nothing

-- whole code graph parser
parseAllCodes' :: [FilePath] -> IO [Code]
parseAllCodes' [] = pure []
parseAllCodes' (x:xs) = do
    code <- catchAny (parseCode' x) $ \e -> do putStrLn (x ++ ": fájl beolvasása sikertelen, a program nem kezeli."); pure Nothing
    cs <- parseAllCodes' xs
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

filePathReader :: IO ([FunName],[FilePath])
filePathReader = do
    path <- parseFilePath
    content <- readFile path
    pure (words $ head $ lines content, filter System.FilePath.isValid $ tail $ lines content)
