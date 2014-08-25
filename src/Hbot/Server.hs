{-# LANGUAGE NamedFieldPuns    #-}
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

module Hbot.Server where

--------------------------------------------------------------------------------
import           Control.Monad             ( void )
import           Control.Monad.IO.Class
import           Control.Monad.Reader      ( asks, ReaderT, runReaderT )
import           Control.Monad.Trans       ( lift )
import           Data.Aeson                ( encode, decode )
import           Data.ByteString.Lazy      ( toStrict )
import           Data.Text.Lazy.Encoding   ( decodeUtf8 )
import qualified Data.Text.Lazy            as T
import           Data.Monoid               ( mconcat, (<>) )
import           Network.HTTP.Conduit      ( simpleHttp, http, parseUrl, method
                                           , requestBody, requestHeaders
                                           , withManager
                                           , Request, RequestBody(RequestBodyBS) )
import           Web.Scotty.Trans

--------------------------------------------------------------------------------
import           Hbot.ChatNotification
import           Hbot.MessageEvent         ( MessageEvent, eventMsg )
import           Hbot.Plugins
import           Hbot.Plugins.Whoami       ( whoami )
import           Hbot.MsgParser

--------------------------------------------------------------------------------
-- | ENV parameters required to run the hbot server
data AppParams = AppParams
    { port   :: !Int    -- ^ port to run http server on
    , room   :: !String -- ^ hipchat room for bot to hangout in
    , prefix :: !String -- ^ line prefix for messages bot should respond to
    , token  :: !String -- ^ api key for hipchat
    }

-- | Application monad for running Scotty w/ AppParams reader access
type BotSM = ScottyT T.Text (ReaderT AppParams IO)
-- | Action monad for running http handlers w/ AppParams reader access
type BotAM = ActionT T.Text (ReaderT AppParams IO)

--------------------------------------------------------------------------------
app :: AppParams -> IO ()
app ps@(AppParams {port}) = scottyT port readParams readParams routes
  where readParams reader = runReaderT reader ps

routes :: BotSM ()
routes = do
    get "/" $ html "This is hbot!"
    get "/rooms" getRooms
    post "/hook" handleHook

plugins :: Plugin
plugins = dispatch $
    [ ("contrib" , contrib)
    , ("echo"    , echoP)
    , ("reverse" , reverseP)
    , ("whoami"  , whoami)
    , ("wakeup"  , wakeup)
    ]

--------------------------------------------------------------------------------
-- Route Handlers

getRooms :: BotAM ()
getRooms = do
    response <- simpleHttp =<< authorize "https://api.hipchat.com/v2/room"
    html $ decodeUtf8 response

sendMessage :: BotAM ()
sendMessage = do
    msg <- param "msg"
    notifyChat msg
    html $ "Sending message: " <> msg

handleHook :: BotAM ()
handleHook  = do
    pre <- askPrefix
    reqBody <- body
    let maybeInput = do
            event <- decode reqBody :: Maybe MessageEvent
            command <- parseMsg (T.pack pre) . eventMsg $ event
            return (command, event)
    case maybeInput of
        Just input -> do
            result <- liftIO . runPlugin plugins $ input
            notifyChat result
        Nothing -> return ()

--------------------------------------------------------------------------------
-- Utilities

notifyChat :: T.Text -> BotAM ()
notifyChat msgText = do
    req <- mkNotifyRequest msgText =<< notificationUrl =<< askRoom
    void . liftIO . withManager . http $ req

mkNotifyRequest :: T.Text -> String -> BotAM Request
mkNotifyRequest msgText url = fmap (notifyRequest msgText) (liftIO $ parseUrl url)

notifyRequest :: T.Text -> Request -> Request
notifyRequest msg req =
    req { method = "POST"
        , requestHeaders = [("Content-Type", "application/json")]
        , requestBody = RequestBodyBS . toStrict . encode $ notification
        }
  where
    notification = colorMsg Gray . textMsg . defaultNotification $ msg

authorize :: String -> BotAM String
authorize url = do
    auth_token <- askToken
    return $ mconcat [url, "?auth_token=", auth_token]

notificationUrl :: String -> BotAM String
notificationUrl room =
    authorize $ mconcat ["https://api.hipchat.com/v2/room/", room, "/notification" ]

askRoom :: BotAM String
askRoom = lift $ asks room

askPrefix :: BotAM String
askPrefix = lift $ asks prefix

askToken :: BotAM String
askToken = lift $ asks token
