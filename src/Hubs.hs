module Hubs where

import Auth (AuthenticatedUser(..))
import ClassyPrelude
import Control.Monad.Trans (MonadTrans)
import Data.Aeson (FromJSON(..), ToJSON(..), Value(String))
import Data.Has
import qualified Data.Map as M
import qualified Data.Maybe as May (fromJust)
import Data.UUID (UUID, toText)
import Domain (UserId, GameId)
import Json
import Network.WebSockets

data HubType
  = NotificationsHub
  | GameHub GameId
  deriving (Eq, Ord, Show)

data NotificationType
  = InvitedToGame
  | GameStarted
  | InvitationDeclined

  deriving (Generic, Show)

instance ToJSON NotificationType where
  toJSON val = String (tshow val)

data Notification =
  Notification
    { notificationId :: UUID
    , notificationType :: NotificationType
    , notificationPayload :: Maybe Text
    }
  deriving (Generic, Show)

instance ToJSON Notification where
  toJSON = genericToJSONNoPrefix "notification"

type HubsState = TVar Hubs

type Hubs = M.Map HubType [HubConnection]

type HubConnectionId = UUID

data HubConnection =
  HubConnection
    { hubConnId :: UUID
    , hubConnConn :: Connection
    , hubConnUser :: Maybe AuthenticatedUser
    }

instance Show HubConnection where
  show (HubConnection id _ user) = "Connection Id: " <> show id <> " User: " <> show user

type HubOperation r m = (Has HubsState r, MonadReader r m, MonadIO m)

subscribeToHub :: HubOperation r m => HubType -> Maybe AuthenticatedUser -> Connection -> HubConnectionId -> m () -- <-- TODO: Either?
subscribeToHub hub userMay conn connId =
  withHubsDo $ \(hubs, hState) ->
    atomically $ modifyTVar hState (M.insertWith (<>) hub [HubConnection connId conn userMay])

removeConnection :: HubOperation r m => HubConnectionId -> m ()
removeConnection connectionId =
  withHubsDo $ \(hubs, hState) ->
    atomically $ modifyTVar hState (M.map (filter (\(HubConnection id _ _) -> id /= connectionId)))

keepConnectionAlive :: (MonadUnliftIO m, HubOperation r m) => Connection -> HubConnectionId -> m ()
keepConnectionAlive conn connId = do
  liftIO $ forkPingThread conn 30
  keepListening `catch` handleClosed
  where
    keepListening = liftIO $ forever $ void (receiveData conn :: IO Text)
    handleClosed (CloseRequest _ _) = removeConnection connId
    handleClosed ConnectionClosed = removeConnection connId

broadcastMessage :: HubOperation r m => HubType -> Maybe (AuthenticatedUser -> Bool) -> Text -> m ()
broadcastMessage hub predicateMay message =
  withHubsDo $ \(hubs, _) -> do
    let hubConnections = map hubConnConn <$> M.lookup hub hubs
    forM_ hubConnections (mapM_ (liftIO . (`sendTextData` message)))

sendMessageToUser :: HubOperation r m => HubType -> UserId -> Text -> m ()
sendMessageToUser hub uId message =
  withHubsDo $ \(hubs, hState) -> do
    let predicate u = aUserId u == uId
        hubConns = fromMaybe [] $ M.lookup hub hubs
        userConnection = getConnectionsForUsers hubConns predicate
    putStr "Connections count: " -- TODO: Debugging purposes
    print $ length userConnection
    mapM_ (liftIO . (`sendTextData` message)) userConnection

getConnectionsForUsers :: [HubConnection] -> (AuthenticatedUser -> Bool) -> [Connection]
getConnectionsForUsers conns predicate = map hubConnConn . filterByPredicate $ conns
  where
    filterByPredicate = filter (\(HubConnection _ _ user) -> maybe False predicate user)

withHubsDo :: HubOperation r m => ((Hubs, HubsState) -> m a) -> m a
withHubsDo action = do
  hubsState <- asks getter
  hubsMap <- liftIO . readTVarIO $ hubsState
  action (hubsMap, hubsState)