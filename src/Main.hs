module Main where

import System.IO
import Graph
import Algorithm
import Language.Haskell.Exts.Simple
import System.FilePath
import Control.Concurrent.Async
import Control.Parallel.Strategies
import Data.List

main :: IO ()
main = do
    (fs, paths) <- filePathReader -- needs the path to the file with the infos
    codes <- parseAllCodes paths fs
    codes' <- parseAllCodes' paths
    -- putStrLn $ show codes' ++ "\n"
    -- putStrLn $ show codes
    resps <- runParallel codes
    resps' <- runParallel codes'
    putStrLn "By functions:"
    putStrLn $ writeRes resps 1
    putStrLn "Whole codes:"
    putStrLn $ writeRes resps' 1

writeRes :: [Response] -> Int -> String
writeRes [] _ = ""
writeRes (x:xs) n = writeResH x n ++ "\n" ++ writeRes xs (n + 1) where
    writeResH (id1, id2, matches) i = show i ++ ". " ++ show id1 ++ " <-> \n" ++ show id2 ++ ":\n" ++ showMatches matches where
        showMatches [] = ""
        showMatches ((fn, num):xs) = '\t' : fn ++ ": " ++ show num ++ "%\n" ++ showMatches xs

type FunName = String

runAllAsync :: [Code] -> IO [Response]
runAllAsync = mapConcurrently runAlg . makePairs

runParallel :: [Code] -> IO [Response]
runParallel = sequence . parMap rpar runAlg . makePairs

runAlg :: (Code, Code) -> IO Response
runAlg ((id1, g), (id2, h)) = do
    matchnums <- sequence $ map (\ (fn, x, y) -> pure $ (fn, mainAlgorithm x y)) (zipOnFunName g h)
    pure (id1, id2, matchnums)

parseCode :: FilePath -> [FunName] -> IO Code
parseCode p fs = do
    x <- parseFile p
    case x of 
        ParseOk (Module _ _ _ d) -> pure (p, getCallGraphs fs d)
        _ -> error $ "Parse failed for file: " ++ p

parseAllCodes :: [FilePath] -> [FunName] -> IO [Code]
parseAllCodes [] _ = pure []
parseAllCodes (x:xs) fs = do
    c <- parseCode x fs
    cs <- parseAllCodes xs fs
    pure (c:cs)

parseCode' :: FilePath -> IO Code
parseCode' p = do
    x <- parseFile p
    case x of
        ParseOk (Module _ _ _ d) -> pure (p, [wholeCodeGraph d])
        _ -> error $ "Parse failed for file: " ++ p

parseAllCodes' :: [FilePath] -> IO [Code]
parseAllCodes' [] = pure []
parseAllCodes' (x:xs) = do
    c <- parseCode' x
    cs <- parseAllCodes' xs
    pure (c:cs)

parseFilePath :: IO FilePath
parseFilePath = do
    x <- getLine
    case System.FilePath.isValid x of
        True -> pure x
        _ -> pure ""

filePathReader :: IO ([FunName],[FilePath])
filePathReader = do
    path <- parseFilePath
    content <- readFile path
    pure (words $ head $ lines content, filter System.FilePath.isValid $ tail $ lines content)
