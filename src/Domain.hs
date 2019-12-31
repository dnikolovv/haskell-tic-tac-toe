module Domain where

import ClassyPrelude
import Data.UUID (UUID)
import Servant.Auth.Server (ToJWT)

type UserId = UUID

data User =
  User
    { domainUserId :: UserId
    , domainUserName :: Text
    }
  deriving (Show)

data BoardCellValue
  = X
  | O
  deriving (Eq, Show)

data BoardCell =
  BoardCell
    { boardCellValue :: Maybe BoardCellValue
    , boardCellNumber :: Int
    } deriving (Eq, Show)

newtype GameBoard =
  GameBoard
    { gameBoardCells :: [BoardCell]
    } deriving (Show)

type GameId = UUID

data Game =
  Game
    { gameId :: GameId
    , gameBoard :: GameBoard
    , gamePlayer1 :: UserId
    , gamePlayer2 :: UserId
    , gameIsOver :: Bool
    , gameNextPlayer :: UserId
    , gameWinner :: Maybe UserId
    } deriving (Show)