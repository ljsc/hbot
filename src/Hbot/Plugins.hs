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

module Hbot.Plugins where

import Data.Monoid                 ((<>))
import qualified Data.Text.Lazy    as T
import Data.List                   (sort)

import Hbot.MessageEvent
import Hbot.MsgParser
import Paths_hbot                  (getDataFileName)

data Plugin = Plugin {
    runPlugin :: (BotCommand, MessageEvent) -> IO T.Text
  , helpText  :: T.Text
}

dispatch :: [(T.Text, Plugin)] -> Plugin
dispatch table = Plugin
    { runPlugin = \input@(command, _) ->
        let d []                                                 = listCommands table
            d ((cname, plugin):rs) | cname == pluginName command = runPlugin plugin input
                                   | otherwise                   = d rs
        in d table
    , helpText = "Show available hbot commands"
    }

listCommands :: [(T.Text, Plugin)] -> IO T.Text
listCommands = return . ("Available commands: " <>) . T.intercalate ", " . sort . map fst

textPlugin :: T.Text -> (T.Text -> T.Text) -> Plugin
textPlugin help f = Plugin { runPlugin = \(command, _) -> return $ f (messageText command)
                           , helpText  = help
                           }

echoP :: Plugin
echoP = textPlugin "Outputs the input." id

reverseP :: Plugin
reverseP = textPlugin "Print out the reverse of the input string." T.reverse

wakeup :: Plugin
wakeup = textPlugin "Make hbot wake up from its Heroku nap." $ const "I'm up! I'm up!"

contrib :: Plugin
contrib = Plugin
    { runPlugin = \_ -> do
          authorsFile <- getDataFileName "AUTHORS"
          authors <- readFile authorsFile
          return $ T.pack authors
    , helpText = "List contributors to hbot."
    }

