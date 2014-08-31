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

-- | Input to plugins consists of the parsed BotCommand, as well as the entire JSON
-- message from the Hipchat API.
type PluginInput = (BotCommand, MessageEvent)

-- | Pluggable types are those that can be used as plugin handlers, and should be convertable
-- to functions from PluginInput -> IO T.Text
class Pluggable p where
    plug :: p -> TextAction -- ^ Convert the handler to the canonical TextAction format.

-- | TextAction is the standard format for plugin handlers. It takes the
-- PluginInput and preforms some action in IO and returns the text that should
-- be the message body for the Hipchat room notification.
newtype TextAction = TextAction { runTextAction :: PluginInput -> IO T.Text }

instance Pluggable TextAction where
    plug = id

-- | HtmlAction is a plugin that returns an Html markup computation using Blaze
-- for the message body content.
newtype HtmlAction = HtmlAction { runHtmlAction :: PluginInput -> IO Html }

instance Pluggable HtmlAction where
    plug action = TextAction $ \input -> fmap renderHtml (runHtmlAction action input)

-- | TextPure is a plugin that is a simple function from text to text.
newtype TextPure = TextPure { runTextPure :: T.Text -> T.Text }

instance Pluggable TextPure where
    plug f = TextAction $ \(command, _) -> (return . runTextPure f . messageText $ command)

-- | Type for plugins. Must give text for a help description, as well as
-- a handler that implements the Pluggable typeclass.
data Plugin = forall p . Pluggable p => Plugin {
    helpText      :: T.Text
  , pluginHandler :: p
}

-- | Convert plugin to standard format (TextAction) for a handler.
runPlugin :: Plugin -> PluginInput -> IO T.Text
runPlugin (Plugin {pluginHandler=handler}) = runTextAction $ plug handler

-- | Create a plugin from multiple plugin. Takes a parameter as a list of pairs
-- of keywords to actual plugins. As a plugin, it looks up the child plugin
-- from the table based on keyword and runs it if found. If no plugin is found
-- it calls listCommands
dispatch :: [(T.Text, Plugin)] -> Plugin
dispatch table = Plugin "Show available hbot commands" (TextAction handler)
  where
    handler input@(command, _) = d table
      where d []                              = listCommands table
            d ((cname, plugin):rs)
                | cname == pluginName command = runPlugin plugin input
                | otherwise                   = d rs
    listCommands = return . ("Available commands: " <>) . T.intercalate ", " . sort . map fst

-- | Plugin which prints exactly its own input.
echoP :: Plugin
echoP = Plugin "Outputs the input." (TextPure id)

-- | Plugin which returns its own input reversed.
reverseP :: Plugin
reverseP = Plugin "Print out the reverse of the input string." (TextPure T.reverse)

-- | Plugin printing a canned string to say hello after a Heroku nap.
wakeup :: Plugin
wakeup = Plugin "Make hbot wake up from its Heroku nap." . TextPure . const $ "I'm up! I'm up!"

-- | Plugin listing contributors to the hbot code.
contrib :: Plugin
contrib = Plugin "List contributors to hbot." . TextAction $ \_ -> do
    authorsFile <- getDataFileName "AUTHORS"
    authors <- readFile authorsFile
    return $ T.pack authors

