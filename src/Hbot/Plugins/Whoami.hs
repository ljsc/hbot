{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
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

module Hbot.Plugins.Whoami where

import qualified Data.Text.Lazy as T

import Hbot.MessageEvent
import Hbot.Plugins

whoami :: Plugin
whoami = makePlugin "Show information about message sender." . TextAction $ \(_, event) ->
             return (T.pack $ displayFrom ( from . message . eventItem $ event))

displayFrom :: From -> String
displayFrom FromNull               = "Hell if I know"
displayFrom (FromString s)         = s ++ ", of course."
displayFrom (FromObject (FO {..})) =
    concat [ "You are ", fromFullName, ", better known as \"", fromMentionName, "\""]
