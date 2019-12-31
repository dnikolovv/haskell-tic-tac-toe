module App where

import ClassyPrelude
import Control.Monad.Catch (MonadThrow)
import Hubs
import Servant.Auth.Server (JWTSettings)
import UserStore (UserStore)
import Invitations (InvitationsState)
import Game (GamesState)

type State = (UserStore, InvitationsState, GamesState, HubsState, JWTSettings)

newtype AppM a =
  AppM
    { runAppM :: ReaderT State IO a
    }
  deriving (Functor, Applicative, Monad, MonadIO, MonadReader State, MonadThrow, MonadUnliftIO)