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

module Main where

import           Control.Monad.IO.Class
import           Data.Aeson (encode, decode)
import           Data.ByteString.Lazy (ByteString(), toStrict)
import qualified Data.ByteString.Lazy as L
import           Data.Text.Lazy.Encoding (decodeUtf8)
import qualified Data.Text.Lazy as T
import           Data.Monoid (mconcat, (<>))
import           Network.HTTP.Conduit ( simpleHttp, http, parseUrl, method, requestBody
                                      , requestHeaders, withManager, RequestBody(RequestBodyBS))
import           System.Environment (getEnv)
import           Web.Scotty

import           Hbot.ChatNotification
import           Hbot.MessageEvent (MessageEvent, eventMsg)

authorize :: String -> IO String
authorize url = do
  auth_token <- getEnv "AUTH_TOKEN"
  return $ mconcat [url, "?auth_token=", auth_token]

notifyChat :: String -> T.Text -> ActionM ()
notifyChat room msgText =
  let note = colorMsg Gray . textMsg . defaultNotification $ msgText
  in liftIO $ do
       room <- getEnv "ROOM"
       url <- authorize $ mconcat ["https://api.hipchat.com/v2/room/" <> room <> "/notification"]
       req0 <- parseUrl url
       let req = req0 {
                   method = "POST"
                 , requestHeaders = [("Content-Type", "application/json")]
                 , requestBody = RequestBodyBS . toStrict . encode $ note
                 }
       withManager $ \manager -> http req manager
       return ()

getRooms :: ActionM ()
getRooms = do
  response <- liftIO $ authorize "https://api.hipchat.com/v2/room" >>= simpleHttp
  html $ decodeUtf8 response

sendMessage :: String -> ActionM ()
sendMessage room = do
  msg <- param "msg"
  notifyChat room msg
  html $ "Sending message: " <> msg

newtype Plugin = Plugin { runPlugin :: T.Text -> IO T.Text }

echoP :: Plugin
echoP = Plugin $ \msg -> return msg

reverseP :: Plugin
reverseP = Plugin $ \msg -> return $ T.reverse msg

handleHook :: String -> ActionM ()
handleHook room = do
  reqBody <- body
  case decode reqBody :: Maybe MessageEvent of
    Just e -> do result <- liftIO $ runPlugin echoP (eventMsg e)
                 notifyChat room result
    _      -> return ()

app :: Int -> String -> IO ()
app port room =
  scotty port $ do
    get "/"          $ html "This is hbot!"
    get "/rooms"     $ getRooms
    get "/send/:msg" $ sendMessage room
    post "/hook"     $ handleHook room

main :: IO ()
main = do
  port <- getEnv "PORT"
  room <- getEnv "ROOM"
  app (read port) room

