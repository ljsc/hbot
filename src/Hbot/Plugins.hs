{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleInstances #-}
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

import Data.Monoid                 ((<>), Endo(..))
import qualified Data.Text.Lazy    as T
import Data.List                   (sort)

import Hbot.MessageEvent
import Hbot.MsgParser
import Paths_hbot                  (getDataFileName)

type PluginInput = (BotCommand, MessageEvent)

class Pluggable p where
    plug :: p -> TextAction

newtype TextAction = TextAction { runTextAction :: PluginInput -> IO T.Text }

instance Pluggable TextAction where
    plug = id

instance Pluggable (Endo T.Text) where -- Perhaps should just make a new monomorphic type
    plug (Endo f) = TextAction $ \(command, _) -> (return . f . messageText $ command)

data Plugin = Plugin {
    pluginHandler :: TextAction
  , helpText      :: T.Text
}

runPlugin :: Plugin -> PluginInput -> IO T.Text
runPlugin = runTextAction . pluginHandler

makePlugin :: Pluggable p => p -> T.Text -> Plugin
makePlugin p h = Plugin (plug p) h

dispatch :: [(T.Text, Plugin)] -> Plugin
dispatch table = makePlugin (TextAction handler) "Show available hbot commands"
  where
    handler input@(command, _) = d table
      where d []                              = listCommands table
            d ((cname, plugin):rs)
                | cname == pluginName command = runPlugin plugin input
                | otherwise                   = d rs
    listCommands = return . ("Available commands: " <>) . T.intercalate ", " . sort . map fst

echoP :: Plugin
echoP = makePlugin handler  "Outputs the input."
  where handler = Endo id :: Endo T.Text

reverseP :: Plugin
reverseP = makePlugin handler "Print out the reverse of the input string."
  where handler = Endo T.reverse :: Endo T.Text

wakeup :: Plugin
wakeup = makePlugin handler "Make hbot wake up from its Heroku nap."
  where handler = Endo . const $ "I'm up! I'm up!" :: Endo T.Text

contrib :: Plugin
contrib = makePlugin handler "List contributors to hbot."
  where handler = TextAction $ \_ -> do
          authorsFile <- getDataFileName "AUTHORS"
          authors <- readFile authorsFile
          return $ T.pack authors

