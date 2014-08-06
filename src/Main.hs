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

--------------------------------------------------------------------------------
import           Control.Applicative       ( (<$>), (<*>) )
import           Control.Monad.IO.Class
import           Data.Aeson                ( encode, decode )
import           Data.ByteString.Lazy      ( ByteString(), toStrict )
import qualified Data.ByteString.Lazy      as L
import           Data.Text.Lazy.Encoding   ( decodeUtf8 )
import qualified Data.Text.Lazy            as T
import           Data.Monoid               ( mconcat, (<>) )
import           Network.HTTP.Conduit      ( simpleHttp, http, parseUrl, method
                                           , requestBody , requestHeaders
                                           , withManager
                                           , RequestBody(RequestBodyBS) )
import           System.Environment        ( getEnv )
import           Web.Scotty

--------------------------------------------------------------------------------
import           Hbot.ChatNotification
import           Hbot.MessageEvent         ( MessageEvent, eventMsg )
import           Hbot.Plugins

--------------------------------------------------------------------------------
data AppParams = AppParams
    { port   :: !Int    -- port to run http server on
    , room   :: !String -- hipchat room for bot to hangout in
    , prefix :: !String -- line prefix for messages bot should respond to
    }

authorize :: String -> IO String
authorize url = do
    auth_token <- getEnv "AUTH_TOKEN"
    return $ mconcat [url, "?auth_token=", auth_token]

notifyChat :: String -> T.Text -> ActionM ()
notifyChat room msgText =
    let note = colorMsg Gray . textMsg . defaultNotification $ msgText
    in liftIO $ do
         room <- getEnv "ROOM"
         url <- authorize $ mconcat [ "https://api.hipchat.com/v2/room/"
                                    , room
                                    , "/notification"
                                    ]
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

sendMessage :: AppParams -> ActionM ()
sendMessage (AppParams {room=room}) = do
    msg <- param "msg"
    notifyChat room msg
    html $ "Sending message: " <> msg

handleHook :: AppParams -> ActionM ()
handleHook (AppParams {room=room,prefix=prefix}) = do
    reqBody <- body
    case decode reqBody :: Maybe MessageEvent of
        Nothing -> return ()
        Just e  -> do let eventText  = eventMsg e
                          pluginText = case T.stripPrefix (T.pack prefix) eventText of
                                         Just stripped -> stripped
                                         Nothing       -> eventText
                      result <- liftIO $ runPlugin echoP pluginText
                      notifyChat room result

--------------------------------------------------------------------------------
app :: AppParams -> IO ()
app params@(AppParams {port=port}) =
    scotty port $ do
        get "/"          $ html "This is hbot!"
        get "/rooms"     $ getRooms
        get "/send/:msg" $ sendMessage params
        post "/hook"     $ handleHook params

main :: IO ()
main = app =<< params
  where
    params = AppParams <$> fmap read (getEnv "PORT")
                       <*> getEnv "ROOM"
                       <*> getEnv "PREFIX"

