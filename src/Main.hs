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
import           Data.Aeson (encode)
import           Data.ByteString.Lazy (ByteString(), toStrict)
import           Data.Text.Lazy.Encoding (decodeUtf8)
import qualified Data.Text.Lazy as T
import           Data.Monoid (mconcat, (<>))
import           Network.HTTP.Conduit ( simpleHttp, http, parseUrl, method, requestBody
                                      , requestHeaders, withManager, RequestBody(RequestBodyBS))
import           System.Environment (getEnv)
import           Web.Scotty

import           Hbot.ChatNotification
import           Hbot.MessageEvent (MessageEvent)

authorize :: String -> IO String
authorize url = do
  auth_token <- getEnv "AUTH_TOKEN"
  return $ mconcat [url, "?auth_token=", auth_token]

notifyChat :: T.Text -> ActionM ()
notifyChat msgText =
  let note = colorMsg Gray . textMsg . defaultNotification $ msgText
  in liftIO $ do
       url <- authorize "https://api.hipchat.com/v2/room/econify/notification"
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

main :: IO ()
main = scotty 3000 $ do
  get "/" $ html "This is hbot!"
  get "/rooms" $ getRooms
  get "/send/:msg" $ do
    msg <- param "msg"
    notifyChat msg
    html $ "Sending message: " <> msg

