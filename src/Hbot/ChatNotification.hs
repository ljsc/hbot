{-# LANGUAGE OverloadedStrings, RecordWildCards #-}
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
-- | Functions and data types for sending a chat notification event using the
-- Hipchat API.
module Hbot.ChatNotification (
    Color(..),
    MessageFormat(..),
    ChatNotification,
    message, color, notify, messageFormat,
    defaultNotification,
    textMsg, htmlMsg, colorMsg
) where

import           Control.Applicative ((<$>))
import           Data.Aeson
import           Data.Char (toLower)
import           Data.Maybe (catMaybes)
import qualified Data.Text.Lazy as T

-- | Hipchat allows notifications to have differing background colors. This type
-- enumerates the possible color values.
data Color = Yellow | Green | Red | Purple | Gray | Random deriving (Eq, Ord, Enum, Show)

-- | The API allows notifications to be either HTML or Text based. If they are
-- html based, there is a whitelisted bag of html tags that are allowed: consult
-- the API for details. For plain text, they will be processed as normal chat
-- messages are, i.e.: checked for @mentions, etc.
data MessageFormat = Html | Text deriving (Eq, Ord, Enum, Show)

-- | Represents a JSON request to the api for sending a notification to the
-- appropriate room.
data ChatNotification = CN {
  message :: T.Text -- ^ The text data for the message body. Should be plain text or html markup.
, color :: Maybe Color -- ^ Background color for the message. API defaults it to grey.
, notify :: Maybe Bool -- ^ Whether to send notifications to users in the room. Defaults false.
, messageFormat :: Maybe MessageFormat -- ^ Text or HTML. Defaults to text.
} deriving (Show, Eq)

instance ToJSON Color where
  toJSON = toJSON . map toLower . show

instance ToJSON MessageFormat where
  toJSON = toJSON . map toLower . show

instance ToJSON ChatNotification where
  toJSON CN {..} = object . catMaybes $
    [ Just ("message" .= message)
    , ("color".=) . toJSON <$> color
    , ("notify".=) . toJSON <$> notify
    , ("message_format".=) . toJSON <$> messageFormat
    ]

-- | Create a new notification with all defaults other than the message body,
-- supplied as the sole parameter.
defaultNotification :: T.Text -> ChatNotification
defaultNotification msg = CN {
  message = msg
, color = Nothing
, notify = Nothing
, messageFormat = Nothing
}

-- | Transform the notification to set the message type to Text.
textMsg :: ChatNotification -> ChatNotification
textMsg note = note { messageFormat = Just Text }

-- | Transform the notification to set the message type to Html.
htmlMsg :: ChatNotification -> ChatNotification
htmlMsg note = note { messageFormat = Just Html }

-- | Transform the notification to set the background color.
colorMsg :: Color -> ChatNotification -> ChatNotification
colorMsg c note = note { color = Just c }

