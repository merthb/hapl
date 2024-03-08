module Main where

import System.IO
import System.Exit
import System.Directory
import Graph
import Algorithm
import Language.Haskell.Exts.Simple
import System.FilePath
import Control.Parallel.Strategies
import Data.List

main :: IO ()
main = do
    putStrLn "This is a console program, that checks plagiarism between Haskell programs by a call-graph matching algorithm. So to say, with this plagiarism checker we check if the logic behind the codes are similar, without looking at the text level of the codes."
    loop

loop :: IO ()
loop = do
    oneTimeRunner
    putStrLn "If you want to run another plagiarism check, push the SPACE and then the ENTER button, if you want to quit, just push the ENTER button."
    x <- getLine
    case x of
        " " -> loop
        _ -> exitSuccess

oneTimeRunner :: IO ()
oneTimeRunner = do
    putStrLn "To run the plagiarism checker the program needs a text file with the following informations:"
    putStrLn "First line must contain the function names, that have to be checked individually. If empty, only whole codes are checked."
    putStrLn "From second line on each line must contain one path to a file (or a global pattern to files), that must be checked."
    putStrLn "Give the info filepath:"
    (fs, paths) <- filePathReader -- needs the path to the file with the infos
    case fs of
        [] -> do
            codes' <- parseAllCodes' paths
            putStrLn "Whole codes:"
            putStrLn $ writeResAll (runOnAll codes') 1
        _ -> do
            codes <- parseAllCodes paths fs
            codes' <- parseAllCodes' paths
            resps <- runParallel codes
            putStrLn "Result of functions:"
            putStrLn $ writeRes resps 1 fs
            putStrLn "Whole codes:"
            putStrLn $ writeResAll (runOnAll codes') 1

writeRes :: [Response] -> Int -> [FunName] -> String
writeRes [] _ _ = ""
writeRes (x:xs) n fs = writeResH x n fs ++ "\n" ++ writeRes xs (n + 1) fs where
    writeResH :: Response -> Int -> [FunName] -> String
    writeResH (id1, id2, matches) i fs = show i ++ ". " ++ show id1 ++ " <-> \n" ++ show id2 ++ ":\n" ++ showMatches matches fs where
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
    writeResH (id1, id2, matches) i = show i ++ ". " ++ show id1 ++ " <-> \n" ++ show id2 ++ ":\n" ++ showMatches matches where
        showMatches :: [(String, MatchNum)] -> String
        showMatches [] = ""
        showMatches ((f, num):xs) = '\t' : "match" ++ ": " ++ show num ++ "%\n" ++ showMatches xs

type FunName = String

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
    b <- System.Directory.doesFileExist x
    if b 
        then 
            pure x
        else 
            do
                putStrLn "File doesn't exist, try again:"
                parseFilePath

filePathReader :: IO ([FunName],[FilePath])
filePathReader = do
    path <- parseFilePath
    content <- readFile path
    pure (words $ head $ lines content, filter System.FilePath.isValid $ tail $ lines content)
