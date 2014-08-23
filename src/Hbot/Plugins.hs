{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ExistentialQuantification #-}
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

import Data.Monoid                     ((<>))
import qualified Data.Text.Lazy        as T
import Data.List                       (sort)
import Text.Blaze.Html                 (Html)
import Text.Blaze.Html.Renderer.Text   (renderHtml)

import Hbot.MessageEvent
import Hbot.MsgParser
import Paths_hbot                      (getDataFileName)

type PluginInput = (BotCommand, MessageEvent)

class Pluggable p where
    plug :: p -> TextAction

newtype TextAction = TextAction { runTextAction :: PluginInput -> IO T.Text }

instance Pluggable TextAction where
    plug = id

newtype HtmlAction = HtmlAction { runHtmlAction :: PluginInput -> IO Html }

instance Pluggable HtmlAction where
    plug action = TextAction $ \input -> fmap renderHtml (runHtmlAction action input)

newtype TextPure = TextPure { runTextPure :: T.Text -> T.Text }

instance Pluggable TextPure where
    plug f = TextAction $ \(command, _) -> (return . runTextPure f . messageText $ command)

data Plugin = forall p . Pluggable p => Plugin {
    helpText      :: T.Text
  , pluginHandler :: p
}

runPlugin :: Plugin -> PluginInput -> IO T.Text
runPlugin (Plugin {pluginHandler=handler}) = runTextAction $ plug handler

dispatch :: [(T.Text, Plugin)] -> Plugin
dispatch table = Plugin "Show available hbot commands" (TextAction handler)
  where
    handler input@(command, _) = d table
      where d []                              = listCommands table
            d ((cname, plugin):rs)
                | cname == pluginName command = runPlugin plugin input
                | otherwise                   = d rs
    listCommands = return . ("Available commands: " <>) . T.intercalate ", " . sort . map fst

echoP :: Plugin
echoP = Plugin "Outputs the input." (TextPure id)

reverseP :: Plugin
reverseP = Plugin "Print out the reverse of the input string." (TextPure T.reverse)

wakeup :: Plugin
wakeup = Plugin "Make hbot wake up from its Heroku nap." . TextPure . const $ "I'm up! I'm up!"

contrib :: Plugin
contrib = Plugin "List contributors to hbot." . TextAction $ \_ -> do
    authorsFile <- getDataFileName "AUTHORS"
    authors <- readFile authorsFile
    return $ T.pack authors

