module Main where

import System.IO
import Graph
import Algorithm
import Language.Haskell.Exts.Simple
import System.FilePath
import Control.Concurrent.Async
import Control.Parallel.Strategies

main :: IO ()
main = do
    (fs, paths) <- filePathReader -- needs the path to the file with the infos
    codes <- parseAllCodes paths fs
    resps <- runParallel codes
    putStrLn $ show $ map (\ (id1, id2, resps) -> (id1, id2, zip fs resps)) resps

type FunName = String

runAllAsync :: [Code] -> IO [Response]
runAllAsync = mapConcurrently runAlg . makePairs

runParallel :: [Code] -> IO [Response]
runParallel = sequence . parMap rpar runAlg . makePairs

runAlg :: (Code, Code) -> IO Response
runAlg ((id1, g), (id2, h)) = pure (id1, id2, map (\ (x,y) -> mainAlgorithm x y) (zipOnFunName g h))

parseCode :: FilePath -> [FunName] -> IO Code
parseCode p fs = do
    x <- parseFile p
    case x of 
        ParseOk (Module _ _ _ d) -> pure (p, getCallGraphs fs d)
        _ -> error $ "Parse failed for file: " ++ p

parseAllCodes :: [FilePath] -> [FunName] -> IO [Code]
parseAllCodes [] _ = pure []
parseAllCodes (p:ps) fs = do
        c <- parseCode p fs
        cs <- parseAllCodes ps fs
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
