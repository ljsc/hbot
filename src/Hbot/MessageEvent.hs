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

module Hbot.MessageEvent where

import           Control.Applicative
import           Control.Monad
import           Data.Aeson
import           Data.Map.Lazy
import           Data.Text.Lazy
import           Data.Time

type URL = String

data MessageEvent = MessageEvent {
  eventName :: String
, eventItem :: EventItem
, oauthId :: String
, webhookId :: String
} deriving (Show)

data EventItem = EventItem {
  message :: Message
, room :: Room
} deriving (Show)

data Message = Message {
  date :: UTCTime
, file :: Maybe String
, from :: From
, messageId :: String
, mentions :: [Mention]
, msgText :: Text
} deriving (Show)

data From = From {
  fromObject :: Object
, fromString :: String
} deriving (Show)

data FromObject = FromObject {
  objectId :: String
, objectLinks :: [Link]
, fromMentionName :: String
, fromFullName :: String
} deriving (Show)

data Room = Room {
  roomId :: String
, roomLinks :: [Link]
, name :: String
} deriving (Show)

data Mention = Mention {
  mentionId :: String
, mentionLinks :: Map LinkType URL
, mentionName :: String
, mentionFullName :: String
} deriving (Show)

instance FromJSON Mention where
  parseJSON (Object v) = Mention <$> v .: "id"
                                 <*> fmap (delete InvalidLink . mapKeys parseLinkType)
                                          (v .: "links")
                                 <*> v .: "mention_name"
                                 <*> v .: "name"
  parseJSON _ = mzero

data Link = Link deriving (Show)
data LinkType = InvalidLink | SelfLink | MemberLink | WebhookLink deriving (Show, Eq, Ord)

parseLinkType :: String -> LinkType
parseLinkType s | s == "self" = SelfLink
                | s == "member" = MemberLink
                | s == "webhook" = WebhookLink
                | otherwise = InvalidLink

