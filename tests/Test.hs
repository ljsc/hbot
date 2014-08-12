import qualified Hbot.MsgParser.Test as MsgParser

import Test.Tasty
import Test.Tasty.QuickCheck as QC
import Test.Tasty.HUnit

main = defaultMain tests

tests = testGroup "All Tests"
    [ QC.testProperty "trivial property" $
        \x -> (x :: Int) == x
	, MsgParser.tests
    ]

