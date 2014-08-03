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

type URL   = String
type Links = Map LinkType URL

data MessageEvent = MessageEvent {
  eventName :: String
, eventItem :: EventItem
, oauthId :: String
, webhookId :: String
} deriving (Show)

instance FromJSON MessageEvent where
  parseJSON (Object v) = MessageEvent <$> v .: "event"
                                      <*> v .: "item"
                                      <*> v .: "oauth_client_id"
                                      <*> v .: "webhook_id"
  parseJSON _ = mzero

data EventItem = EventItem {
  message :: Message
, room :: Room
} deriving (Show)

instance FromJSON EventItem where
  parseJSON (Object v) = EventItem <$> v .: "message" <*> v .: "room"
  parseJSON _ = mzero

data Message = Message {
  date :: UTCTime
, file :: Maybe String
, from :: From
, messageId :: String
, mentions :: [Mention]
, msgText :: Text
} deriving (Show)

instance FromJSON Message where
  parseJSON (Object v) = Message <$> v .: "date"
                                 <*> v .:? "file"
                                 <*> v .: "from"
                                 <*> v .: "id"
                                 <*> v .: "mentions"
                                 <*> v .: "message"
  parseJSON _ = mzero

data From = FromObject FromObject | FromString String | FromNull deriving (Show)

instance FromJSON From where
  parseJSON v@(Object _) = FromObject <$> parseJSON v
  parseJSON v@(String _) = FromString <$> parseJSON v
  parseJSON Null         = return FromNull

data FromObject = FO {
  objectId :: String
, objectLinks :: Links
, fromMentionName :: String
, fromFullName :: String
} deriving (Show)

instance FromJSON FromObject where
  parseJSON (Object v) = FO <$> v .: "id"
                            <*> fmap linksFromObject (v .: "links")
                            <*> v .: "mention_name"
                            <*> v .: "name"
  parseJSON _ = mzero

data Room = Room {
  roomId :: String
, roomLinks :: Links
, name :: String
} deriving (Show)

instance FromJSON Room where
  parseJSON (Object v) = Room <$> v .: "id"
                              <*> fmap linksFromObject (v .: "links")
                              <*> v .: "name"
  parseJSON _ = mzero

data Mention = Mention {
  mentionId :: String
, mentionLinks :: Links
, mentionName :: String
, mentionFullName :: String
} deriving (Show)

instance FromJSON Mention where
  parseJSON (Object v) = Mention <$> v .: "id"
                                 <*> fmap linksFromObject (v .: "links")
                                 <*> v .: "mention_name"
                                 <*> v .: "name"
  parseJSON _ = mzero

data LinkType = InvalidLink | SelfLink | MemberLink | WebhookLink deriving (Show, Eq, Ord)

linksFromObject :: Map String URL -> Links
linksFromObject = delete InvalidLink . mapKeys parseLinkType

parseLinkType :: String -> LinkType
parseLinkType s | s == "self" = SelfLink
                | s == "member" = MemberLink
                | s == "webhook" = WebhookLink
                | otherwise = InvalidLink

