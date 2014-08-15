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

import Data.Monoid ((<>))
import qualified Data.Text.Lazy as T

import Hbot.MessageEvent
import Hbot.MsgParser
import Paths_hbot (getDataFileName)

newtype Plugin = Plugin { runPlugin :: (BotCommand, MessageEvent) -> IO T.Text }

dispatch :: [(T.Text, Plugin)] -> Plugin
dispatch table = Plugin $ \input@(command, _) ->
    let d []                                                 = listCommands table
        d ((cname, plugin):rs) | cname == pluginName command = runPlugin plugin input
                               | otherwise                   = d rs
    in d table

listCommands :: [(T.Text, Plugin)] -> IO T.Text
listCommands table = return . ("Available commands: " <>) . T.intercalate ", " $ map fst table

textPlugin :: (T.Text -> T.Text) -> Plugin
textPlugin f = Plugin $ \(command, _) -> return $ f (messageText command)

echoP :: Plugin
echoP = textPlugin id

reverseP :: Plugin
reverseP = textPlugin T.reverse

wakeup :: Plugin
wakeup = textPlugin $ const "I'm up! I'm up!"

contrib :: Plugin
contrib = Plugin $ \_ -> do
    authorsFile <- getDataFileName "AUTHORS"
    authors <- readFile authorsFile
    return $ T.pack authors

