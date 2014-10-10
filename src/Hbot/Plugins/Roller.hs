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
module Hbot.Plugins.Roller (
  rollP
) where

import qualified Data.Text.Lazy as T

import Hbot.Plugins
import Hbot.MsgParser

import Roller.Parse (parse)
import Roller.Core  (roll)

rollP :: Plugin
rollP = Plugin "Roll some dice" . TextAction $ \(command, _) -> do
    case parse (T.unpack . messageText $ command) of
        Just dice -> roll dice >>= return . output
        Nothing   -> return "Parse error: enter valid dice expression"
  where
    output :: [[Int]] -> T.Text
    output = T.pack . show . sum . map sum

