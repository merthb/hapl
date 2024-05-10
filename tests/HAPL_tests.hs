import Graph_tests
import Algorithm_tests
import Test.HUnit
import System.Exit

-- running all the tests defined

main :: IO ()
main = do
    countsG <- runTestTT ( test [
        allGraphBuildTests
        ])
    countsA <- runTestTT ( test [
        allAlgorithmTests
        ])
    if (errors countsG + failures countsG == 0 && errors countsA + failures countsA == 0)
        then exitSuccess
        else exitFailure
