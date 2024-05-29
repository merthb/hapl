module Algorithm_tests where

import Graph
import Language.Haskell.Exts.Simple
import Algorithm
import Data.HashSet
import Test.HUnit
import System.Exit

-- testing the main algorithm
allAlgorithmTests :: Test
allAlgorithmTests = TestLabel "Tests of the main algorithm" $ TestList testList

testList :: [Test]
testList = [
        TestLabel "Testing the heuristics of the A* - different graphs" $ TestList heuristicDiffTests,
        TestLabel "Testing the heuristics of the A* - same graphs" $ TestList heuristicSameTests,
        TestLabel "Testing the neighbors method of the A* - different graphs" $ TestList neighborsNotEmptyTests,
        TestLabel "Testing the neighbors method of the A* - same graphs" $ TestList neighborsEmptyTests,
        TestLabel "Testing the cost method of the A* - different graphs" $ TestList costDiffTests,
        TestLabel "Testing the cost method of the A* - same graphs" $ TestList costSameTests,
        TestLabel "Testing the maxCost method of the A*" $ TestList maxCostTests,
        TestLabel "Testing the main algorithm - different graphs" $ TestList mainAlgorithmDiffTests,
        TestLabel "Testing the main algorithm - same graphs" $ TestList mainAlgorithmSameTests
    ]

heuristicDiffTests :: [Test]
heuristicDiffTests = [
        (testFileEqual "tests/test_files/test1.hs" "tests/test_files/test2.hs"),
        (testFileEqual "tests/test_files/test1.hs" "tests/test_files/test3.hs"),
        (testFileEqual "tests/test_files/test1.hs" "tests/test_files/test4.hs"),
        (testFileEqual "tests/test_files/test1.hs" "tests/test_files/test5.hs"),
        (testFileEqual "tests/test_files/test1.hs" "tests/test_files/test6.hs"),
        (testFileEqual "tests/test_files/test1.hs" "tests/test_files/test7.hs"),
        (testFileEqual "tests/test_files/test1.hs" "tests/test_files/test8.hs"),
        (testFileEqual "tests/test_files/test1.hs" "tests/test_files/test9.hs"),
        (testFileEqual "tests/test_files/test1.hs" "tests/test_files/test10.hs"),
        (testFileEqual "tests/test_files/test2.hs" "tests/test_files/test3.hs"),
        (testFileEqual "tests/test_files/test2.hs" "tests/test_files/test4.hs"),
        (testFileEqual "tests/test_files/test2.hs" "tests/test_files/test5.hs"),
        (testFileEqual "tests/test_files/test2.hs" "tests/test_files/test6.hs"),
        (testFileEqual "tests/test_files/test2.hs" "tests/test_files/test7.hs"),
        (testFileEqual "tests/test_files/test2.hs" "tests/test_files/test8.hs"),
        (testFileEqual "tests/test_files/test2.hs" "tests/test_files/test9.hs"),
        (testFileEqual "tests/test_files/test2.hs" "tests/test_files/test10.hs"),
        (testFileEqual "tests/test_files/test3.hs" "tests/test_files/test4.hs"),
        (testFileEqual "tests/test_files/test3.hs" "tests/test_files/test5.hs"),
        (testFileEqual "tests/test_files/test3.hs" "tests/test_files/test6.hs"),
        (testFileEqual "tests/test_files/test3.hs" "tests/test_files/test7.hs"),
        (testFileEqual "tests/test_files/test3.hs" "tests/test_files/test8.hs"),
        (testFileEqual "tests/test_files/test3.hs" "tests/test_files/test9.hs"),
        (testFileEqual "tests/test_files/test3.hs" "tests/test_files/test10.hs"),
        (testFileEqual "tests/test_files/test4.hs" "tests/test_files/test5.hs"),
        (testFileEqual "tests/test_files/test4.hs" "tests/test_files/test6.hs"),
        (testFileEqual "tests/test_files/test4.hs" "tests/test_files/test7.hs"),
        (testFileEqual "tests/test_files/test4.hs" "tests/test_files/test8.hs"),
        (testFileEqual "tests/test_files/test4.hs" "tests/test_files/test9.hs"),
        (testFileEqual "tests/test_files/test4.hs" "tests/test_files/test10.hs"),
        (testFileEqual "tests/test_files/test5.hs" "tests/test_files/test6.hs"),
        (testFileEqual "tests/test_files/test5.hs" "tests/test_files/test7.hs"),
        (testFileEqual "tests/test_files/test5.hs" "tests/test_files/test8.hs"),
        (testFileEqual "tests/test_files/test5.hs" "tests/test_files/test9.hs"),
        (testFileEqual "tests/test_files/test5.hs" "tests/test_files/test10.hs"),
        (testFileEqual "tests/test_files/test6.hs" "tests/test_files/test7.hs"),
        (testFileEqual "tests/test_files/test6.hs" "tests/test_files/test8.hs"),
        (testFileEqual "tests/test_files/test6.hs" "tests/test_files/test9.hs"),
        (testFileEqual "tests/test_files/test6.hs" "tests/test_files/test10.hs"),
        (testFileEqual "tests/test_files/test7.hs" "tests/test_files/test8.hs"),
        (testFileEqual "tests/test_files/test7.hs" "tests/test_files/test9.hs"),
        (testFileEqual "tests/test_files/test7.hs" "tests/test_files/test10.hs"),
        (testFileEqual "tests/test_files/test8.hs" "tests/test_files/test9.hs"),
        (testFileEqual "tests/test_files/test8.hs" "tests/test_files/test10.hs"),
        (testFileEqual "tests/test_files/test9.hs" "tests/test_files/test10.hs")
    ] where
        testFileEqual path1 path2 =
            TestCase (do
                x <- parseFile path1
                y <- parseFile path2
                case x of
                    ParseOk (Module _ _ _ declx) -> case y of
                        ParseOk (Module _ _ _ decly) -> assertBool "Heuristic value isn't positive but it should be." ((heuristic (wholeCodeGraph declx) (wholeCodeGraph declx) (wholeCodeGraph decly)) > 0)
                        _ -> assertFailure "Test file couldn't be parsed, possibly syntax error in file."
                    _ -> assertFailure "Test file couldn't be parsed, possibly syntax error in file.")

heuristicSameTests :: [Test]
heuristicSameTests = [
        (testFileEqual "tests/test_files/test1.hs" "tests/test_files/test1.hs"),
        (testFileEqual "tests/test_files/test2.hs" "tests/test_files/test2.hs"),
        (testFileEqual "tests/test_files/test3.hs" "tests/test_files/test3.hs"),
        (testFileEqual "tests/test_files/test4.hs" "tests/test_files/test4.hs"),
        (testFileEqual "tests/test_files/test5.hs" "tests/test_files/test5.hs"),
        (testFileEqual "tests/test_files/test6.hs" "tests/test_files/test6.hs"),
        (testFileEqual "tests/test_files/test7.hs" "tests/test_files/test7.hs"),
        (testFileEqual "tests/test_files/test8.hs" "tests/test_files/test8.hs"),
        (testFileEqual "tests/test_files/test9.hs" "tests/test_files/test9.hs"),
        (testFileEqual "tests/test_files/test10.hs" "tests/test_files/test10.hs")
    ] where
        testFileEqual path1 path2 =
            TestCase (do
                x <- parseFile path1
                y <- parseFile path2
                case x of
                    ParseOk (Module _ _ _ declx) -> case y of
                        ParseOk (Module _ _ _ decly) -> assertBool "Heuristic value isn't zero but it should be." ((heuristic (wholeCodeGraph declx) (wholeCodeGraph declx) (wholeCodeGraph decly)) == 0)
                        _ -> assertFailure "Test file couldn't be parsed, possibly syntax error in file."
                    _ -> assertFailure "Test file couldn't be parsed, possibly syntax error in file.")

neighborsNotEmptyTests :: [Test]
neighborsNotEmptyTests = [
        (testFileEqual "tests/test_files/test1.hs" "tests/test_files/test2.hs"),
        (testFileEqual "tests/test_files/test1.hs" "tests/test_files/test3.hs"),
        (testFileEqual "tests/test_files/test1.hs" "tests/test_files/test4.hs"),
        (testFileEqual "tests/test_files/test1.hs" "tests/test_files/test5.hs"),
        (testFileEqual "tests/test_files/test1.hs" "tests/test_files/test6.hs"),
        (testFileEqual "tests/test_files/test1.hs" "tests/test_files/test7.hs"),
        (testFileEqual "tests/test_files/test1.hs" "tests/test_files/test8.hs"),
        (testFileEqual "tests/test_files/test1.hs" "tests/test_files/test9.hs"),
        (testFileEqual "tests/test_files/test1.hs" "tests/test_files/test10.hs"),
        (testFileEqual "tests/test_files/test2.hs" "tests/test_files/test3.hs"),
        (testFileEqual "tests/test_files/test2.hs" "tests/test_files/test4.hs"),
        (testFileEqual "tests/test_files/test2.hs" "tests/test_files/test5.hs"),
        (testFileEqual "tests/test_files/test2.hs" "tests/test_files/test6.hs"),
        (testFileEqual "tests/test_files/test2.hs" "tests/test_files/test7.hs"),
        (testFileEqual "tests/test_files/test2.hs" "tests/test_files/test8.hs"),
        (testFileEqual "tests/test_files/test2.hs" "tests/test_files/test9.hs"),
        (testFileEqual "tests/test_files/test2.hs" "tests/test_files/test10.hs"),
        (testFileEqual "tests/test_files/test3.hs" "tests/test_files/test4.hs"),
        (testFileEqual "tests/test_files/test3.hs" "tests/test_files/test5.hs"),
        (testFileEqual "tests/test_files/test3.hs" "tests/test_files/test6.hs"),
        (testFileEqual "tests/test_files/test3.hs" "tests/test_files/test7.hs"),
        (testFileEqual "tests/test_files/test3.hs" "tests/test_files/test8.hs"),
        (testFileEqual "tests/test_files/test3.hs" "tests/test_files/test9.hs"),
        (testFileEqual "tests/test_files/test3.hs" "tests/test_files/test10.hs"),
        (testFileEqual "tests/test_files/test4.hs" "tests/test_files/test5.hs"),
        (testFileEqual "tests/test_files/test4.hs" "tests/test_files/test6.hs"),
        (testFileEqual "tests/test_files/test4.hs" "tests/test_files/test7.hs"),
        (testFileEqual "tests/test_files/test4.hs" "tests/test_files/test8.hs"),
        (testFileEqual "tests/test_files/test4.hs" "tests/test_files/test9.hs"),
        (testFileEqual "tests/test_files/test4.hs" "tests/test_files/test10.hs"),
        (testFileEqual "tests/test_files/test5.hs" "tests/test_files/test6.hs"),
        (testFileEqual "tests/test_files/test5.hs" "tests/test_files/test7.hs"),
        (testFileEqual "tests/test_files/test5.hs" "tests/test_files/test8.hs"),
        (testFileEqual "tests/test_files/test5.hs" "tests/test_files/test9.hs"),
        (testFileEqual "tests/test_files/test5.hs" "tests/test_files/test10.hs"),
        (testFileEqual "tests/test_files/test6.hs" "tests/test_files/test7.hs"),
        (testFileEqual "tests/test_files/test6.hs" "tests/test_files/test8.hs"),
        (testFileEqual "tests/test_files/test6.hs" "tests/test_files/test9.hs"),
        (testFileEqual "tests/test_files/test6.hs" "tests/test_files/test10.hs"),
        (testFileEqual "tests/test_files/test7.hs" "tests/test_files/test8.hs"),
        (testFileEqual "tests/test_files/test7.hs" "tests/test_files/test9.hs"),
        (testFileEqual "tests/test_files/test7.hs" "tests/test_files/test10.hs"),
        (testFileEqual "tests/test_files/test8.hs" "tests/test_files/test9.hs"),
        (testFileEqual "tests/test_files/test8.hs" "tests/test_files/test10.hs"),
        (testFileEqual "tests/test_files/test9.hs" "tests/test_files/test10.hs")
    ] where
        testFileEqual path1 path2 =
            TestCase (do
                x <- parseFile path1
                y <- parseFile path2
                case x of
                    ParseOk (Module _ _ _ declx) -> case y of
                        ParseOk (Module _ _ _ decly) -> assertBool "Neighbors list empty but it shouldn't be." (not $ Data.HashSet.null (neighbors (wholeCodeGraph declx) (wholeCodeGraph decly)))
                        _ -> assertFailure "Test file couldn't be parsed, possibly syntax error in file."
                    _ -> assertFailure "Test file couldn't be parsed, possibly syntax error in file.")

neighborsEmptyTests :: [Test]
neighborsEmptyTests = [
        (testFileEqual "tests/test_files/test1.hs" "tests/test_files/test1.hs"),
        (testFileEqual "tests/test_files/test2.hs" "tests/test_files/test2.hs"),
        (testFileEqual "tests/test_files/test3.hs" "tests/test_files/test3.hs"),
        (testFileEqual "tests/test_files/test4.hs" "tests/test_files/test4.hs"),
        (testFileEqual "tests/test_files/test5.hs" "tests/test_files/test5.hs"),
        (testFileEqual "tests/test_files/test6.hs" "tests/test_files/test6.hs"),
        (testFileEqual "tests/test_files/test7.hs" "tests/test_files/test7.hs"),
        (testFileEqual "tests/test_files/test8.hs" "tests/test_files/test8.hs"),
        (testFileEqual "tests/test_files/test9.hs" "tests/test_files/test9.hs"),
        (testFileEqual "tests/test_files/test10.hs" "tests/test_files/test10.hs")
    ] where
        testFileEqual path1 path2 =
            TestCase (do
                x <- parseFile path1
                y <- parseFile path2
                case x of
                    ParseOk (Module _ _ _ declx) -> case y of
                        ParseOk (Module _ _ _ decly) -> assertBool "Neighbors list should be empty but it isn't." (Data.HashSet.null (neighbors (wholeCodeGraph declx) (wholeCodeGraph decly)))
                        _ -> assertFailure "Test file couldn't be parsed, possibly syntax error in file."
                    _ -> assertFailure "Test file couldn't be parsed, possibly syntax error in file.")

costDiffTests :: [Test]
costDiffTests = [
        (testFileEqual "tests/test_files/test1.hs" "tests/test_files/test2.hs"),
        (testFileEqual "tests/test_files/test1.hs" "tests/test_files/test3.hs"),
        (testFileEqual "tests/test_files/test1.hs" "tests/test_files/test4.hs"),
        (testFileEqual "tests/test_files/test1.hs" "tests/test_files/test5.hs"),
        (testFileEqual "tests/test_files/test1.hs" "tests/test_files/test6.hs"),
        (testFileEqual "tests/test_files/test1.hs" "tests/test_files/test7.hs"),
        (testFileEqual "tests/test_files/test1.hs" "tests/test_files/test8.hs"),
        (testFileEqual "tests/test_files/test1.hs" "tests/test_files/test9.hs"),
        (testFileEqual "tests/test_files/test1.hs" "tests/test_files/test10.hs"),
        (testFileEqual "tests/test_files/test2.hs" "tests/test_files/test3.hs"),
        (testFileEqual "tests/test_files/test2.hs" "tests/test_files/test4.hs"),
        (testFileEqual "tests/test_files/test2.hs" "tests/test_files/test5.hs"),
        (testFileEqual "tests/test_files/test2.hs" "tests/test_files/test6.hs"),
        (testFileEqual "tests/test_files/test2.hs" "tests/test_files/test7.hs"),
        (testFileEqual "tests/test_files/test2.hs" "tests/test_files/test8.hs"),
        (testFileEqual "tests/test_files/test2.hs" "tests/test_files/test9.hs"),
        (testFileEqual "tests/test_files/test2.hs" "tests/test_files/test10.hs"),
        (testFileEqual "tests/test_files/test3.hs" "tests/test_files/test4.hs"),
        (testFileEqual "tests/test_files/test3.hs" "tests/test_files/test5.hs"),
        (testFileEqual "tests/test_files/test3.hs" "tests/test_files/test6.hs"),
        (testFileEqual "tests/test_files/test3.hs" "tests/test_files/test7.hs"),
        (testFileEqual "tests/test_files/test3.hs" "tests/test_files/test8.hs"),
        (testFileEqual "tests/test_files/test3.hs" "tests/test_files/test9.hs"),
        (testFileEqual "tests/test_files/test3.hs" "tests/test_files/test10.hs"),
        (testFileEqual "tests/test_files/test4.hs" "tests/test_files/test5.hs"),
        (testFileEqual "tests/test_files/test4.hs" "tests/test_files/test6.hs"),
        (testFileEqual "tests/test_files/test4.hs" "tests/test_files/test7.hs"),
        (testFileEqual "tests/test_files/test4.hs" "tests/test_files/test8.hs"),
        (testFileEqual "tests/test_files/test4.hs" "tests/test_files/test9.hs"),
        (testFileEqual "tests/test_files/test4.hs" "tests/test_files/test10.hs"),
        (testFileEqual "tests/test_files/test5.hs" "tests/test_files/test6.hs"),
        (testFileEqual "tests/test_files/test5.hs" "tests/test_files/test7.hs"),
        (testFileEqual "tests/test_files/test5.hs" "tests/test_files/test8.hs"),
        (testFileEqual "tests/test_files/test5.hs" "tests/test_files/test9.hs"),
        (testFileEqual "tests/test_files/test5.hs" "tests/test_files/test10.hs"),
        (testFileEqual "tests/test_files/test6.hs" "tests/test_files/test7.hs"),
        (testFileEqual "tests/test_files/test6.hs" "tests/test_files/test8.hs"),
        (testFileEqual "tests/test_files/test6.hs" "tests/test_files/test9.hs"),
        (testFileEqual "tests/test_files/test6.hs" "tests/test_files/test10.hs"),
        (testFileEqual "tests/test_files/test7.hs" "tests/test_files/test8.hs"),
        (testFileEqual "tests/test_files/test7.hs" "tests/test_files/test9.hs"),
        (testFileEqual "tests/test_files/test7.hs" "tests/test_files/test10.hs"),
        (testFileEqual "tests/test_files/test8.hs" "tests/test_files/test9.hs"),
        (testFileEqual "tests/test_files/test8.hs" "tests/test_files/test10.hs"),
        (testFileEqual "tests/test_files/test9.hs" "tests/test_files/test10.hs")
    ] where
        testFileEqual path1 path2 =
            TestCase (do
                x <- parseFile path1
                y <- parseFile path2
                case x of
                    ParseOk (Module _ _ _ declx) -> case y of
                        ParseOk (Module _ _ _ decly) -> assertBool "Cost value isn't positive but it should be." ((cost (wholeCodeGraph declx) (wholeCodeGraph decly)) > 0)
                        _ -> assertFailure "Test file couldn't be parsed, possibly syntax error in file."
                    _ -> assertFailure "Test file couldn't be parsed, possibly syntax error in file.")

costSameTests :: [Test]
costSameTests = [
        (testFileEqual "tests/test_files/test1.hs" "tests/test_files/test1.hs"),
        (testFileEqual "tests/test_files/test2.hs" "tests/test_files/test2.hs"),
        (testFileEqual "tests/test_files/test3.hs" "tests/test_files/test3.hs"),
        (testFileEqual "tests/test_files/test4.hs" "tests/test_files/test4.hs"),
        (testFileEqual "tests/test_files/test5.hs" "tests/test_files/test5.hs"),
        (testFileEqual "tests/test_files/test6.hs" "tests/test_files/test6.hs"),
        (testFileEqual "tests/test_files/test7.hs" "tests/test_files/test7.hs"),
        (testFileEqual "tests/test_files/test8.hs" "tests/test_files/test8.hs"),
        (testFileEqual "tests/test_files/test9.hs" "tests/test_files/test9.hs"),
        (testFileEqual "tests/test_files/test10.hs" "tests/test_files/test10.hs")
    ] where
        testFileEqual path1 path2 =
            TestCase (do
                x <- parseFile path1
                y <- parseFile path2
                case x of
                    ParseOk (Module _ _ _ declx) -> case y of
                        ParseOk (Module _ _ _ decly) -> assertBool "Cost value should be zero but it isn't." ((cost (wholeCodeGraph declx) (wholeCodeGraph decly)) == 0)
                        _ -> assertFailure "Test file couldn't be parsed, possibly syntax error in file."
                    _ -> assertFailure "Test file couldn't be parsed, possibly syntax error in file.")

maxCostTests :: [Test]
maxCostTests = [
        (testFileEqual "tests/test_files/test1.hs" "tests/test_files/test2.hs"),
        (testFileEqual "tests/test_files/test1.hs" "tests/test_files/test3.hs"),
        (testFileEqual "tests/test_files/test1.hs" "tests/test_files/test4.hs"),
        (testFileEqual "tests/test_files/test1.hs" "tests/test_files/test5.hs"),
        (testFileEqual "tests/test_files/test1.hs" "tests/test_files/test6.hs"),
        (testFileEqual "tests/test_files/test1.hs" "tests/test_files/test7.hs"),
        (testFileEqual "tests/test_files/test1.hs" "tests/test_files/test8.hs"),
        (testFileEqual "tests/test_files/test1.hs" "tests/test_files/test9.hs"),
        (testFileEqual "tests/test_files/test1.hs" "tests/test_files/test10.hs"),
        (testFileEqual "tests/test_files/test2.hs" "tests/test_files/test3.hs"),
        (testFileEqual "tests/test_files/test2.hs" "tests/test_files/test4.hs"),
        (testFileEqual "tests/test_files/test2.hs" "tests/test_files/test5.hs"),
        (testFileEqual "tests/test_files/test2.hs" "tests/test_files/test6.hs"),
        (testFileEqual "tests/test_files/test2.hs" "tests/test_files/test7.hs"),
        (testFileEqual "tests/test_files/test2.hs" "tests/test_files/test8.hs"),
        (testFileEqual "tests/test_files/test2.hs" "tests/test_files/test9.hs"),
        (testFileEqual "tests/test_files/test2.hs" "tests/test_files/test10.hs"),
        (testFileEqual "tests/test_files/test3.hs" "tests/test_files/test4.hs"),
        (testFileEqual "tests/test_files/test3.hs" "tests/test_files/test5.hs"),
        (testFileEqual "tests/test_files/test3.hs" "tests/test_files/test6.hs"),
        (testFileEqual "tests/test_files/test3.hs" "tests/test_files/test7.hs"),
        (testFileEqual "tests/test_files/test3.hs" "tests/test_files/test8.hs"),
        (testFileEqual "tests/test_files/test3.hs" "tests/test_files/test9.hs"),
        (testFileEqual "tests/test_files/test3.hs" "tests/test_files/test10.hs"),
        (testFileEqual "tests/test_files/test4.hs" "tests/test_files/test5.hs"),
        (testFileEqual "tests/test_files/test4.hs" "tests/test_files/test6.hs"),
        (testFileEqual "tests/test_files/test4.hs" "tests/test_files/test7.hs"),
        (testFileEqual "tests/test_files/test4.hs" "tests/test_files/test8.hs"),
        (testFileEqual "tests/test_files/test4.hs" "tests/test_files/test9.hs"),
        (testFileEqual "tests/test_files/test4.hs" "tests/test_files/test10.hs"),
        (testFileEqual "tests/test_files/test5.hs" "tests/test_files/test6.hs"),
        (testFileEqual "tests/test_files/test5.hs" "tests/test_files/test7.hs"),
        (testFileEqual "tests/test_files/test5.hs" "tests/test_files/test8.hs"),
        (testFileEqual "tests/test_files/test5.hs" "tests/test_files/test9.hs"),
        (testFileEqual "tests/test_files/test5.hs" "tests/test_files/test10.hs"),
        (testFileEqual "tests/test_files/test6.hs" "tests/test_files/test7.hs"),
        (testFileEqual "tests/test_files/test6.hs" "tests/test_files/test8.hs"),
        (testFileEqual "tests/test_files/test6.hs" "tests/test_files/test9.hs"),
        (testFileEqual "tests/test_files/test6.hs" "tests/test_files/test10.hs"),
        (testFileEqual "tests/test_files/test7.hs" "tests/test_files/test8.hs"),
        (testFileEqual "tests/test_files/test7.hs" "tests/test_files/test9.hs"),
        (testFileEqual "tests/test_files/test7.hs" "tests/test_files/test10.hs"),
        (testFileEqual "tests/test_files/test8.hs" "tests/test_files/test9.hs"),
        (testFileEqual "tests/test_files/test8.hs" "tests/test_files/test10.hs"),
        (testFileEqual "tests/test_files/test9.hs" "tests/test_files/test10.hs")
    ] where
        testFileEqual path1 path2 =
            TestCase (do
                x <- parseFile path1
                y <- parseFile path2
                case x of
                    ParseOk (Module _ _ _ declx) -> case y of
                        ParseOk (Module _ _ _ decly) -> assertBool "Max cost isn't positive but it should be." ((maxCost (wholeCodeGraph declx) (wholeCodeGraph decly)) > 0)
                        _ -> assertFailure "Test file couldn't be parsed, possibly syntax error in file."
                    _ -> assertFailure "Test file couldn't be parsed, possibly syntax error in file.")

mainAlgorithmDiffTests :: [Test]
mainAlgorithmDiffTests = [
        (testFileEqual "tests/test_files/test1.hs" "tests/test_files/test2.hs"),
        (testFileEqual "tests/test_files/test1.hs" "tests/test_files/test3.hs"),
        (testFileEqual "tests/test_files/test1.hs" "tests/test_files/test4.hs"),
        (testFileEqual "tests/test_files/test1.hs" "tests/test_files/test5.hs"),
        (testFileEqual "tests/test_files/test1.hs" "tests/test_files/test6.hs"),
        (testFileEqual "tests/test_files/test1.hs" "tests/test_files/test7.hs"),
        (testFileEqual "tests/test_files/test1.hs" "tests/test_files/test8.hs"),
        (testFileEqual "tests/test_files/test1.hs" "tests/test_files/test9.hs"),
        (testFileEqual "tests/test_files/test1.hs" "tests/test_files/test10.hs"),
        (testFileEqual "tests/test_files/test2.hs" "tests/test_files/test3.hs"),
        (testFileEqual "tests/test_files/test2.hs" "tests/test_files/test4.hs"),
        (testFileEqual "tests/test_files/test2.hs" "tests/test_files/test5.hs"),
        (testFileEqual "tests/test_files/test2.hs" "tests/test_files/test6.hs"),
        (testFileEqual "tests/test_files/test2.hs" "tests/test_files/test7.hs"),
        (testFileEqual "tests/test_files/test2.hs" "tests/test_files/test8.hs"),
        (testFileEqual "tests/test_files/test2.hs" "tests/test_files/test9.hs"),
        (testFileEqual "tests/test_files/test2.hs" "tests/test_files/test10.hs"),
        (testFileEqual "tests/test_files/test3.hs" "tests/test_files/test4.hs"),
        (testFileEqual "tests/test_files/test3.hs" "tests/test_files/test5.hs"),
        (testFileEqual "tests/test_files/test3.hs" "tests/test_files/test6.hs"),
        (testFileEqual "tests/test_files/test3.hs" "tests/test_files/test7.hs"),
        (testFileEqual "tests/test_files/test3.hs" "tests/test_files/test8.hs"),
        (testFileEqual "tests/test_files/test3.hs" "tests/test_files/test9.hs"),
        (testFileEqual "tests/test_files/test3.hs" "tests/test_files/test10.hs"),
        (testFileEqual "tests/test_files/test4.hs" "tests/test_files/test5.hs"),
        (testFileEqual "tests/test_files/test4.hs" "tests/test_files/test6.hs"),
        (testFileEqual "tests/test_files/test4.hs" "tests/test_files/test7.hs"),
        (testFileEqual "tests/test_files/test4.hs" "tests/test_files/test8.hs"),
        (testFileEqual "tests/test_files/test4.hs" "tests/test_files/test9.hs"),
        (testFileEqual "tests/test_files/test4.hs" "tests/test_files/test10.hs"),
        (testFileEqual "tests/test_files/test5.hs" "tests/test_files/test6.hs"),
        (testFileEqual "tests/test_files/test5.hs" "tests/test_files/test7.hs"),
        (testFileEqual "tests/test_files/test5.hs" "tests/test_files/test8.hs"),
        (testFileEqual "tests/test_files/test5.hs" "tests/test_files/test9.hs"),
        (testFileEqual "tests/test_files/test5.hs" "tests/test_files/test10.hs"),
        (testFileEqual "tests/test_files/test6.hs" "tests/test_files/test7.hs"),
        (testFileEqual "tests/test_files/test6.hs" "tests/test_files/test8.hs"),
        (testFileEqual "tests/test_files/test6.hs" "tests/test_files/test9.hs"),
        (testFileEqual "tests/test_files/test6.hs" "tests/test_files/test10.hs"),
        (testFileEqual "tests/test_files/test7.hs" "tests/test_files/test8.hs"),
        (testFileEqual "tests/test_files/test7.hs" "tests/test_files/test9.hs"),
        (testFileEqual "tests/test_files/test7.hs" "tests/test_files/test10.hs"),
        (testFileEqual "tests/test_files/test8.hs" "tests/test_files/test9.hs"),
        (testFileEqual "tests/test_files/test8.hs" "tests/test_files/test10.hs"),
        (testFileEqual "tests/test_files/test9.hs" "tests/test_files/test10.hs")
    ] where
        testFileEqual path1 path2 =
            TestCase (do
                x <- parseFile path1
                y <- parseFile path2
                case x of
                    ParseOk (Module _ _ _ declx) -> case y of
                        ParseOk (Module _ _ _ decly) -> assertBool "Similarity score not in range." ((\ x -> x > 0 && x < 75) (mainAlgorithm (wholeCodeGraph declx) (wholeCodeGraph decly)))
                        _ -> assertFailure "Test file couldn't be parsed, possibly syntax error in file."
                    _ -> assertFailure "Test file couldn't be parsed, possibly syntax error in file.")

mainAlgorithmSameTests :: [Test]
mainAlgorithmSameTests = [
        (testFileEqual "tests/test_files/test1.hs" "tests/test_files/test1.hs"),
        (testFileEqual "tests/test_files/test2.hs" "tests/test_files/test2.hs"),
        (testFileEqual "tests/test_files/test3.hs" "tests/test_files/test3.hs"),
        (testFileEqual "tests/test_files/test4.hs" "tests/test_files/test4.hs"),
        (testFileEqual "tests/test_files/test5.hs" "tests/test_files/test5.hs"),
        (testFileEqual "tests/test_files/test6.hs" "tests/test_files/test6.hs"),
        (testFileEqual "tests/test_files/test7.hs" "tests/test_files/test7.hs"),
        (testFileEqual "tests/test_files/test8.hs" "tests/test_files/test8.hs"),
        (testFileEqual "tests/test_files/test9.hs" "tests/test_files/test9.hs"),
        (testFileEqual "tests/test_files/test10.hs" "tests/test_files/test10.hs")
    ] where
        testFileEqual path1 path2 =
            TestCase (do
                x <- parseFile path1
                y <- parseFile path2
                case x of
                    ParseOk (Module _ _ _ declx) -> case y of
                        ParseOk (Module _ _ _ decly) -> assertBool "Similarity score not 100." ((mainAlgorithm (wholeCodeGraph declx) (wholeCodeGraph decly)) == 100)
                        _ -> assertFailure "Test file couldn't be parsed, possibly syntax error in file."
                    _ -> assertFailure "Test file couldn't be parsed, possibly syntax error in file.")
