module Views where

import ClassyPrelude
import Data.Aeson (ToJSON(..))
import Data.UUID (UUID)
import Json

type Token = Text

newtype TokenView =
  TokenView
    { tokenViewToken :: Token
    }
  deriving (Generic)

instance ToJSON TokenView where
  toJSON = genericToJSONNoPrefix "tokenView"

data UserView =
  UserView
    { userViewId :: Text
    , userViewName :: Text
    }
  deriving (Generic)

instance ToJSON UserView where
  toJSON = genericToJSONNoPrefix "userView"

data BoardCellView =
  BoardCellView
    { boardCellViewValue :: Maybe Int
    , boardCellViewNumber :: Int
    }
  deriving (Generic)

instance ToJSON BoardCellView where
  toJSON = genericToJSONNoPrefix "boardCellView"

newtype GameBoardView =
  GameBoardView
    { gameBoardViewCells :: [BoardCellView]
    }
  deriving (Generic)

instance ToJSON GameBoardView where
  toJSON = genericToJSONNoPrefix "gameBoardView"

data GameView =
  GameView
    { gameViewId :: UUID
    , gameViewBoard :: GameBoardView
    , gameViewPlayer1 :: UUID
    , gameViewPlayer2 :: UUID
    , gameViewIsOver :: Bool
    , gameViewNextPlayer :: UUID
    , gameViewWinner :: Maybe UUID
    }
  deriving (Generic)

instance ToJSON GameView where
  toJSON = genericToJSONNoPrefix "gameView"