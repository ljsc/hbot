{-# LANGUAGE RecordWildCards #-}
import Hbot.MsgParser

import           Control.Applicative
import           Data.Char
import qualified Data.Text.Lazy as L
import           Text.Parsec

import           Test.Tasty
import           Test.Tasty.QuickCheck as QC

main = defaultMain tests

tests = testGroup "Tests" [parsing]

parsing = testGroup "Parsing"
    [ QC.testProperty "Gets name and message text" $
        \msg -> case parse (prefixParser $ prefix msg) "" (asText msg) of
                    Right parsed -> commandName msg == pluginName parsed &&
                                    remainder msg   == messageText parsed
                    Left _ -> True
    ]

data ServerMsg = ServerMsg
    { spaces1     :: L.Text
    , prefix      :: L.Text
    , spaces2     :: L.Text
    , commandName :: L.Text
    , spaces3     :: L.Text
    , remainder   :: L.Text
    } deriving (Show)

instance Arbitrary ServerMsg where
    arbitrary = ServerMsg <$> spaces <*> anums
                          <*> spaces <*> anums
                          <*> spaces <*> anums --fmap L.pack arbitrary
      where
        spaces = L.pack <$> listOf1 (return ' ')
        anums  = fmap L.pack $ listOf1 . suchThat (choose ('\33', '\122')) $ isAlphaNum

asText :: ServerMsg -> L.Text
asText (ServerMsg {..}) = L.concat $ [spaces1, prefix, spaces2, commandName, spaces3, remainder]
