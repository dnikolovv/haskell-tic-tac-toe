module Handlers where

import App (State)
import Auth (AuthenticatedUser(..), createJwt, withAuthenticatedWsConnection)
import ClassyPrelude
import Commands (SubmitGameMove(..))
import Control.Monad.Catch (MonadThrow, throwM)
import Control.Monad.Except (ExceptT, MonadError, catchError, runExceptT, throwError)
import Data.Aeson (encode)
import Data.Coerce (Coercible)
import Data.Has (Has, getter)
import Data.Maybe (fromJust)
import qualified Data.UUID as UUID
import qualified Data.UUID.V4 as UUID
import Domain
  ( BoardCell(..)
  , Game(..)
  , GameBoard(..)
  , GameId
  , User(..)
  , UserId
  , boardCellValue
  , gameId
  , gamePlayer1
  , gamePlayer2
  )
import Error
import GHC.Base (Constraint)
import Game
import Hubs (HubConnectionId, HubType(..), HubsState, Notification(..), NotificationType(..), sendMessageToUser)
import qualified Hubs as H
import Invitations
import Network.WebSockets (Connection, PendingConnection, rejectRequest, sendTextData)
import Servant.Auth.Server (JWTSettings)
import qualified UserStore as US
import Views
import Common (checkBool)

newtype HandlerM a =
  HandlerM
    { runHandlerM :: ReaderT State IO a
    }
  deriving (Functor, Applicative, Monad, MonadReader State, MonadThrow, MonadIO, MonadUnliftIO)

sendNotification :: Notification -> UserId -> HandlerM ()
sendNotification notification userId =
  sendMessageToUser NotificationsHub userId (decodeUtf8 . toStrict . encode $ notification)

createUser :: Text -> HandlerM TokenView
createUser newUserName = do
  existingUser <- US.getUser newUserName
  case existingUser of
    Nothing -> do
      newUserId <- liftIO UUID.nextRandom
      let user = User newUserId newUserName
      US.addUser user
      createJwt $ toAuthUser user
    Just _ -> throwM $ Conflict $ "User with name " <> newUserName <> " already exists."
  where
    toAuthUser (User id name) = AuthenticatedUser id name

listAvailableUsers :: HandlerM [UserView]
listAvailableUsers = do
  activePlayers <- concatMap (\g -> [gamePlayer1 g, gamePlayer2 g]) <$> getActiveGames
  map toUserView . filter (\u -> domainUserId u `notElem` activePlayers) <$> US.listUsers
  where
    toUserView (User id name) = UserView (tshow id) name

inviteUser :: UserId -> UserId -> HandlerM ()
inviteUser initiatorId toBeInvitedId = do
  notificationId <- liftIO UUID.nextRandom
  let invitation = PendingInvitation notificationId initiatorId toBeInvitedId
  addInvitation invitation
  sendNotification (Notification notificationId InvitedToGame Nothing) toBeInvitedId

submitGameMove :: AuthenticatedUser -> SubmitGameMove -> HandlerM ()
submitGameMove (AuthenticatedUser userId _) (SubmitGameMove submittedCellNumber submittedGameId) = do
  oldGame <- getGame submittedGameId
  newGame <- fillCell userId submittedCellNumber oldGame
  let messageContent = decodeUtf8 . toStrict . encode . toGameView $ newGame
  H.broadcastMessage (GameHub $ gameId newGame) Nothing messageContent

acceptInvitation :: InvitationId -> HandlerM ()
acceptInvitation invitationId = do
  invitationMay <- getInvitation invitationId
  case invitationMay of
    Just invitation -> do
      game <- startNewGame (invitingUserId invitation) (invitedUserId invitation)
      newNotifId <- liftIO UUID.nextRandom
      let gameStartedNotif = Notification newNotifId GameStarted (Just . UUID.toText $ gameId game)
      sendNotification gameStartedNotif (invitingUserId invitation)
      sendNotification gameStartedNotif (invitedUserId invitation)
      removeInvitationFromPending invitationId
    Nothing -> return ()

declineInvitation :: InvitationId -> HandlerM ()
declineInvitation invitationId = do
  invitationMay <- getInvitation invitationId
  case invitationMay of
    Just invitation -> do
      newNotifId <- liftIO UUID.nextRandom
      sendNotification (Notification newNotifId InvitationDeclined Nothing) (invitingUserId invitation)
      removeInvitationFromPending invitationId
    Nothing -> return ()

subscribeForGameEvents :: GameId -> PendingConnection -> HandlerM ()
subscribeForGameEvents gameId connRequest = do
  gameMay <- getGameMay gameId
  case gameMay of
    Just game -> do
      withAuthenticatedWsConnection connRequest $ \user conn -> do
        liftIO $ sendTextData conn (encode . toGameView $ game)
        void $ subscribeToHub (GameHub gameId) (Just user) conn
      return ()
    Nothing -> liftIO $ rejectRequest connRequest mempty

subscribeToHubAsUser :: HubType -> PendingConnection -> HandlerM Connection
subscribeToHubAsUser hub pConn =
  withAuthenticatedWsConnection pConn $ \user conn -> void $ subscribeToHub hub (Just user) conn

subscribeToHub :: HubType -> Maybe AuthenticatedUser -> Connection -> HandlerM HubConnectionId
subscribeToHub hub userMay conn = do
  connId <- liftIO UUID.nextRandom
  H.subscribeToHub hub userMay conn connId
  H.keepConnectionAlive conn connId
  return connId

liftHandler :: (MonadIO m, MonadReader State m, MonadThrow m) => HandlerM a -> m a
liftHandler operation = do
  state <- ask
  liftIO $ flip runReaderT state $ runHandlerM operation