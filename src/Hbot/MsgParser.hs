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
module Hbot.MsgParser
    ( parseMsg
    , BotCommand()
    , pluginName
    , messageText
    , prefixParser
    ) where

--------------------------------------------------------------------------------
import Control.Applicative hiding (many, (<|>))
import Data.Text.Lazy
import Text.Parsec
import Text.Parsec.Text.Lazy

--------------------------------------------------------------------------------
data BotCommand = BotCommand
    { pluginName :: Text
    , messageText :: Text
    } deriving (Show)

parseMsg :: Text -> Text -> Maybe BotCommand
parseMsg p t =
    case parse (prefixParser p) "" t of
        Left _    -> Nothing
        Right res -> Just res

prefixParser :: Text -> Parser BotCommand
prefixParser p =
    BotCommand <$> (skipSpaces *> prefix p *> skipSpaces *> theCommand <* skipSpaces)
               <*> theRest

skipSpaces :: Parser ()
skipSpaces = skipMany space

prefix :: Text -> Parser String
prefix = string . unpack

theCommand :: Parser Text
theCommand = pack <$> many alphaNum

theRest :: Parser Text
theRest = pack <$> many anyChar

