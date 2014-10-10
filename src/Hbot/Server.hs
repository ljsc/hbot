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

module Hbot.Server (
    -- * Application configuration
    --
    -- | These definitions let you change the bot parameters to configure for your
    -- specific setup on hipchat. The AppParams information is fetched from
    -- environemnt variables, while the routes and plugins setup the mapping for
    -- the actual useful functionalty to be provided by the bot.

    -- ** Environment
    AppParams(..),
    -- ** Server actions
    routes, plugins,

    -- * Starting the server
    app,

    -- * Bot actions
    getRooms, sendMessage, handleHook,

    -- * Utilities
    notifyChat, mkNotifyRequest, notifyRequest, authorize, notificationUrl,
    askRoom, askPrefix, askToken
) where

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
import           Hbot.Plugins.Roller       ( rollP )
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

-- | Main application function. Parses the application parameters and runs the
-- scotty router.
app :: AppParams -> IO ()
app ps@(AppParams {port}) = scottyT port readParams readParams routes
  where readParams reader = runReaderT reader ps

-- | Application routes
routes :: BotSM ()
routes = do
    get "/" $ html "This is hbot!"
    get "/rooms" getRooms
    post "/hook" handleHook

-- | Keyword to plugin mapping for the server.
plugins :: Plugin
plugins = dispatch $
    [ ("contrib" , contrib)
    , ("echo"    , echoP)
    , ("roll"    , rollP)
    , ("reverse" , reverseP)
    , ("whoami"  , whoami)
    , ("wakeup"  , wakeup)
    ]

--------------------------------------------------------------------------------
-- Route Handlers

-- | Displays a list of rooms available to the bot account.
getRooms :: BotAM ()
getRooms = do
    response <- simpleHttp =<< authorize "https://api.hipchat.com/v2/room"
    html $ decodeUtf8 response

-- | Send a message notification directly to the room.
sendMessage :: BotAM ()
sendMessage = do
    msg <- param "msg"
    notifyChat msg
    html $ "Sending message: " <> msg

-- | Handles the webhook fired by the Hipchat API whenever a message matching
-- a given regex is detected within a room. Hbot will run the corresponding
-- plugin to handle the input if the message matches a given keyword in the
-- dispatch table.
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

-- | Send a chat notification with the given message body as the bot to the configured room.
notifyChat :: T.Text -> BotAM ()
notifyChat msgText = do
    req <- mkNotifyRequest msgText =<< notificationUrl =<< askRoom
    void . liftIO . withManager . http $ req

-- | Given notification body text and the authorized url to send to, creates
-- a message notification request for the hipchat API.
mkNotifyRequest :: T.Text -> String -> BotAM Request
mkNotifyRequest msgText url = fmap (notifyRequest msgText) (liftIO $ parseUrl url)

-- | Updates a plain conduit request with the correct headers to send to the
-- hipchat API. Using the provided Text parameter, this function creates
-- a notification as text witht the default gray background.
notifyRequest :: T.Text -> Request -> Request
notifyRequest msg req =
    req { method = "POST"
        , requestHeaders = [("Content-Type", "application/json")]
        , requestBody = RequestBodyBS . toStrict . encode $ notification
        }
  where
    notification = colorMsg Gray . textMsg . defaultNotification $ msg

-- | Adds the auth_token to an API url.
authorize :: String -> BotAM String
authorize url = do
    auth_token <- askToken
    return $ mconcat [url, "?auth_token=", auth_token]

-- | Compute the correct room URI from the room name, and authorize.
notificationUrl :: String -> BotAM String
notificationUrl room =
    authorize $ mconcat ["https://api.hipchat.com/v2/room/", room, "/notification" ]

-- | Get the value of the ROOM env variable from the Reader environment.
askRoom :: BotAM String
askRoom = lift $ asks room

-- | Get the value of the PREFIX env variable from the Reader environment. This
-- value should be at the start of all commands intended for handling by hbot.
askPrefix :: BotAM String
askPrefix = lift $ asks prefix

-- | Get the value of the TOKEN env variable from the Reader environment. This
-- string will be used by the authorize function to create authenticated
-- requests.
askToken :: BotAM String
askToken = lift $ asks token

