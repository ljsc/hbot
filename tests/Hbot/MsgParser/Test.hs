{-# LANGUAGE OverloadedStrings #-}
{-
    hbot - a simple Haskell chat bot for Hipchat
    Copyright (C) 2014 Louis J. Scoras

    This program is free software: you can redistribute it and/or modify
    it under the terms of the GNU General Public License as published by
    the Free Software Foundation, either version 3 of the License, or
    (at your option) any later version.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
    GNU General Public License for more details.

    You should have received a copy of the GNU General Public License
    along with this program.  If not, see <http://www.gnu.org/licenses/>.
-}
module Hbot.MsgParser.Test where

import Data.Maybe (fromJust)
import Test.Tasty
import Test.Tasty.HUnit

import Hbot.MsgParser

tests = testGroup "Parser Tests"
    [ testCase "Return nothing if prefix isn't matched" noPrefix
    , prefixWhitespaceTests
    , testCase "Splits the command from the rest of the content" endToEnd
    ]

noPrefix :: Assertion
noPrefix = parseMsg ">>" ": Not prefixed correctly" @=? Nothing

prefixWhitespaceTests :: TestTree
prefixWhitespaceTests = testGroup "Prefix whitespace" $
    [ testCase "Removes leading space"            $ try "    >> foo bar baz"
    , testCase "Removes trailing spaces"          $ try ">>     foo bar baz"
    , testCase "Removes all spaces around prefix" $ try " >>    foo bar baz"
    ]
  where
    try msg = case parseMsg ">>" msg of
        Just command -> pluginName command  @=? "foo"
        Nothing -> fail "Should have parsed correctly"

endToEnd :: Assertion
endToEnd = case parseMsg ":" ":command   the rest " of
    Just cmd -> (pluginName cmd, messageText cmd) @=? ("command", "the rest ")
    Nothing  -> fail "Should have parsed correctly"

