import Test.Tasty
import Test.Tasty.QuickCheck as QC
import Test.Tasty.HUnit

main = defaultMain tests

tests = testGroup "Tests" [examples]

examples = testGroup "Examples"
    [ QC.testProperty "trivial property" $
        \x -> (x :: Int) == x
    , testCase "trivial unit test" $
		1 + 1 @=? 2
    ]

