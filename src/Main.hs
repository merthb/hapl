module Main where

import Algorithm
import Control.Concurrent.Async
import Control.Parallel.Strategies
import Graph
import Language.Haskell.Exts.Simple
import System.FilePath
import System.FilePath.Glob
import System.IO

main :: IO ()
main = do
  (fs, paths) <- filePathReader -- needs the path to the file with the infos
  codes <- parseAllCodes paths fs
  resps <- runParallel codes
  putStrLn $ show $ map (\(id1, id2, resps) -> (id1, id2, zip fs resps)) resps

type FunName = String

runAllAsync :: [Code] -> IO [Response]
runAllAsync = mapConcurrently runAlg . makePairs

runParallel :: [Code] -> IO [Response]
runParallel = sequence . parMap rpar runAlg . makePairs

runAlg :: (Code, Code) -> IO Response
runAlg ((id1, g), (id2, h)) = do
  matchnums <- sequence $ parMap rpar (\(x, y) -> pure $ mainAlgorithm x y) (zipOnFunName g h)
  pure (id1, id2, matchnums)

parseCode :: FilePath -> [FunName] -> IO Code
parseCode p fs = do
  x <- parseFile p
  case x of
    ParseOk (Module _ _ _ d) -> pure (p, getCallGraphs fs d)
    _ -> error $ "Parse failed for file: " ++ p

parseAllCodes :: [FilePath] -> [FunName] -> IO [Code]
parseAllCodes [] _ = pure []
parseAllCodes (x : xs) fs = do
  c <- parseCode x fs
  cs <- parseAllCodes xs fs
  pure (c : cs)

parseFilePath :: IO FilePath
parseFilePath = do
  x <- getLine
  case System.FilePath.isValid x of
    True -> pure x
    _ -> pure ""

filePathReader :: IO ([FunName], [FilePath])
filePathReader = do
  path <- parseFilePath
  content <- readFile path
  files <- glob $ head $ tail $ lines content
  pure (words $ head $ lines content, filter System.FilePath.isValid files)
