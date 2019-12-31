{-# LANGUAGE ScopedTypeVariables #-}
module Api
  ( Api
  , server
  ) where

import App
import Auth
import ClassyPrelude
import Commands
import Control.Monad.Catch (MonadThrow, throwM)
import Data.Has (Has)
import Data.Maybe
import qualified Data.UUID as U
import qualified Data.UUID.V4 as U
import Domain
import qualified Hubs as H
import Network.WebSockets (Connection, PendingConnection, forkPingThread, sendTextData)
import Servant hiding (Unauthorized)
import Servant.API.WebSocket
import Servant.Auth.Server
import Servant.Server hiding (Unauthorized)
import Views
import qualified Handlers
import Data.Aeson (ToJSON, encode)
import Control.Monad.Except (MonadError)
import Handlers (HandlerM)
import Error (AppError(..))
import Servant (ServerError(..))
import Data.CaseInsensitive (mk)

type Api = Protected :<|> Public :<|> Sockets

type AppServer api = ServerT api AppM

server :: AppServer Api
server = protectedServer :<|> publicServer :<|> socketsServer

type JwtAuth = Auth '[ JWT] AuthenticatedUser

type Public =
  "users" :> "create" :> Capture "name" Text :> Post '[ JSON] TokenView

type Protected = JwtAuth :> (
  "users" :> "current" :> Get '[ JSON] UserView :<|>
  "users" :> "available" :> Get '[ JSON] [UserView] :<|>
  "users" :> "invite" :> Capture "userId" U.UUID :> PostNoContent '[ JSON] NoContent :<|>
  "invitation" :> "accept" :> Capture "invitationId" U.UUID :> PostNoContent '[ JSON] NoContent :<|>
  "invitation" :> "decline" :> Capture "invitationId" U.UUID :> PostNoContent '[ JSON] NoContent :<|>
  "game" :> "move" :> ReqBody '[ JSON] SubmitGameMove :> PutNoContent '[ JSON] NoContent)

type Sockets =
  "notifications" :> "subscribe" :> WebSocketPending :<|>
  "game" :> Capture "gameId" U.UUID :> WebSocketPending

publicServer :: AppServer Public
publicServer = createUser
  where
    createUser = runHandlerOp . Handlers.createUser

protectedServer :: AppServer Protected
protectedServer (Authenticated user) =
  getCurrentUser :<|>
  listAvailableUsers :<|>
  inviteUser :<|>
  acceptInvitation :<|>
  declineInvitation :<|>
  submitGameMove
  where
    getCurrentUser = return $ UserView (U.toText . aUserId $ user) (aUserName user)
    listAvailableUsers = runHandlerOp Handlers.listAvailableUsers
    inviteUser userToInviteId = runHandlerOp $ Handlers.inviteUser (aUserId user) userToInviteId >> return NoContent
    -- TODO: Validate who is accepting/declining the invitation
    acceptInvitation id = runHandlerOp $ Handlers.acceptInvitation id >> return NoContent
    declineInvitation id = runHandlerOp $ Handlers.declineInvitation id >> return NoContent
    submitGameMove submitMoveCmd = runHandlerOp $ Handlers.submitGameMove user submitMoveCmd >> return NoContent
-- No throwAll because of the MonadUnliftIO hack - https://harporoeder.com/posts/servant-13-reader-io/
protectedServer _ = throwM err401 :<|> throwM err401 :<|> (\_ -> throwM err401) :<|> (\_ -> throwM err401) :<|> (\_ -> throwM err401) :<|> (\_ -> throwM err401)

socketsServer :: AppServer Sockets
socketsServer = subscribeForNotifications :<|> subscribeForGame
  where
    subscribeForNotifications pConn = void $ runHandlerOp $ Handlers.subscribeToHubAsUser H.NotificationsHub pConn
    subscribeForGame gameId conn = runHandlerOp $ Handlers.subscribeForGameEvents gameId conn

newtype ApiError =
  ApiError
    { message :: Text
    }
  deriving (Generic)

instance ToJSON ApiError

runHandlerOp :: (MonadIO m, MonadReader State m, MonadThrow m, MonadUnliftIO m) => HandlerM a -> m a
runHandlerOp handlerOp =
  Handlers.liftHandler handlerOp `catch` (\(ex :: AppError) -> handleError ex)
  where
    handleError ex = throwM $ toServantError ex

toServantError :: AppError -> ServerError
toServantError (ValidationError msg) = servantErrorWithText err400 msg
toServantError (Unauthorized msg) = servantErrorWithText err401 msg
toServantError (NotFound msg) = servantErrorWithText err404 msg
toServantError (Conflict msg) = servantErrorWithText err409 msg

servantErrorWithText :: ServerError -> Text -> ServerError
servantErrorWithText sErr message = sErr {errBody = errorBody, errHeaders = [jsonHeaders]}
  where
    errorBody = encode $ ApiError message
    jsonHeaders = (mk "Content-Type", "application/json;charset=utf-8")