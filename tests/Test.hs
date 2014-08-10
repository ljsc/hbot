import Test.Tasty
import Test.Tasty.QuickCheck as QC

main = defaultMain tests

tests = testGroup "Tests" [examples]

examples = testGroup "Examples"
    [ QC.testProperty "trivial" $
         \x -> (x :: Int) == x
    ]

