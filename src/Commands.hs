module Commands where

import ClassyPrelude
import Data.Aeson
import Domain
import Json
import Data.UUID (UUID)

data InviteUser =
  InviteUser
    { invitingUserId :: UserId
    , invitedUserId :: UserId
    }
  deriving (Generic)

instance FromJSON InviteUser where
  parseJSON = genericParseWithPrefix "inviteUser"

data SubmitGameMove =
  SubmitGameMove
    { submitGameMoveCellNumber :: Int
    , submitGameMoveGameId :: UUID
    }
  deriving (Generic)

instance FromJSON SubmitGameMove where
  parseJSON = genericParseWithPrefix "submitGameMove"