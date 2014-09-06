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
-- | Parse messages that are matched by the webhook for room notifications
module Hbot.MsgParser (
    -- * Parsing function
    parseMsg,
    -- * Command type
    BotCommand(),
    -- * Command acessors
    pluginName, messageText
) where

--------------------------------------------------------------------------------
import Control.Applicative hiding (many, (<|>))
import Data.Text.Lazy
import Text.Parsec
import Text.Parsec.Text.Lazy

--------------------------------------------------------------------------------
-- | Represents the command sent to a given hbot plugin.
data BotCommand = BotCommand
    { pluginName :: Text -- ^ The keyword/name for the plugin to invoke
    , messageText :: Text -- ^ The text of the message sent to the chat room
    } deriving (Show, Eq)

-- | Possibly return a BotCommand based on the given prefix
parseMsg :: Text -- ^ Prefix matched by the webhook. Stripped by the parser
         -> Text -- ^ The Text input from the chat room message
         -> Maybe BotCommand -- ^ Input for the parser
parseMsg p t =
    case parse (prefixParser p) "" t of
        Left _    -> Nothing
        Right res -> Just res

-- | Top level parser
prefixParser :: Text -> Parser BotCommand
prefixParser p =
    BotCommand <$> (skipSpaces *> prefix p *> skipSpaces *> theCommand <* skipSpaces)
               <*> theRest

-- | Remove whitespaces
skipSpaces :: Parser ()
skipSpaces = skipMany space

-- | Parser for the given prefix Text
prefix :: Text -> Parser String
prefix = string . unpack

-- | Parser for the command/plugin name
theCommand :: Parser Text
theCommand = pack <$> many alphaNum

-- | Parser that collects the rest of the input to pass to the plugin
theRest :: Parser Text
theRest = pack <$> many anyChar

