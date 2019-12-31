module UserStore where

import ClassyPrelude
import Data.Has (Has, getter)
import Domain (User(..))

type UserStore = TVar [User]

type UserStoreOperation r m = (Has UserStore r, MonadReader r m, MonadIO m)

getUser :: UserStoreOperation r m => Text -> m (Maybe User)
getUser name = do
  store <- asks getter
  users <- readTVarIO store :: MonadIO m => m [User]
  return $ find (\(User _ userName) -> userName == name) users

addUser :: (Has UserStore r, MonadReader r m, MonadIO m) => User -> m ()
addUser user = do
  store <- asks getter
  atomically $ modifyTVar store (<> [user])

listUsers :: (Has UserStore r, MonadReader r m, MonadIO m) => m [User]
listUsers = do
  store <- asks getter
  readTVarIO store