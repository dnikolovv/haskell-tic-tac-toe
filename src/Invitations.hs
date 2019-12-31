module Invitations where

import ClassyPrelude
import Data.Has (Has, getter)
import qualified Data.List as L
import Data.UUID (UUID)
import Domain (UserId)

type InvitationsState = TVar [PendingInvitation]

type InvitationId = UUID

data PendingInvitation =
  PendingInvitation
    { invitationId :: UUID
    , invitingUserId :: UserId
    , invitedUserId :: UserId
    }
  deriving (Show)

type InvitationsOperation r m = (Has InvitationsState r, MonadReader r m, MonadIO m)

getInvitation :: InvitationsOperation r m => InvitationId -> m (Maybe PendingInvitation)
getInvitation invitationId = do
  invitationsState <- asks getter
  invitations <- liftIO . readTVarIO $ invitationsState :: MonadIO m => m [PendingInvitation]
  return $ L.find (\(PendingInvitation id _ _) -> id == invitationId) invitations

addInvitation :: InvitationsOperation r m => PendingInvitation -> m ()
addInvitation invitation = do
  invitations <- asks getter
  atomically $ modifyTVar invitations (<> [invitation])

removeInvitationFromPending :: InvitationsOperation r m => InvitationId -> m ()
removeInvitationFromPending invitationId = do
  invitations <- asks getter
  atomically $ modifyTVar invitations (L.filter (\(PendingInvitation id _ _) -> id /= invitationId))