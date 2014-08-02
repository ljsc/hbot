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

module Hbot.ChatNotification where

import           Control.Applicative ((<$>))
import           Data.Aeson
import           Data.Char (toLower)
import           Data.Maybe (catMaybes)
import qualified Data.Text.Lazy as T

data Color = Yellow | Green | Red | Purple | Gray | Random deriving (Eq, Ord, Enum, Show)
data MessageFormat = Html | Text deriving (Eq, Ord, Enum, Show)

data ChatNotification = CN {
  message :: T.Text
, color :: Maybe Color
, notify :: Maybe Bool
, messageFormat :: Maybe MessageFormat
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

defaultNotification :: T.Text -> ChatNotification
defaultNotification msg = CN {
  message = msg
, color = Nothing
, notify = Nothing
, messageFormat = Nothing
}

textMsg :: ChatNotification -> ChatNotification
textMsg note = note { messageFormat = Just Text }

htmlMsg :: ChatNotification -> ChatNotification
htmlMsg note = note { messageFormat = Just Html }

colorMsg :: Color -> ChatNotification -> ChatNotification
colorMsg c note = note { color = Just c }

