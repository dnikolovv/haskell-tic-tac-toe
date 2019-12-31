module Game where

import ClassyPrelude
import Control.Monad.Except (ExceptT, MonadError)
import Data.Has (Has, getter)
import Data.List (transpose)
import Data.List.Split (chunksOf)
import qualified Data.UUID.V4 as UUID
import Domain
import Views (BoardCellView(..), GameBoardView(..), GameView(..))
import Common
import Error
import Control.Monad.Catch (MonadThrow, throwM)

type GamesState = TVar [Game]

type GameOperation r m = (Has GamesState r, MonadReader r m, MonadIO m, MonadThrow m)

startNewGame :: GameOperation r m => UserId -> UserId -> m Game
startNewGame playerX playerO = do
  gameId <- liftIO UUID.nextRandom
  let game = Game gameId emptyGameBoard playerX playerO False playerX Nothing
  games <- asks getter
  atomically $ modifyTVar games (<> [game])
  return game

changeGameState :: GameOperation r m => Game -> Game -> m ()
changeGameState oldG newG = do
  checkBool (gameId oldG == gameId newG) $ ValidationError "Game ids must match."
  game <- getGame $ gameId oldG
  removeGame $ gameId game
  addGame newG

addGame :: GameOperation r m => Game -> m ()
addGame gameToAdd = do
  gamesState <-
    asks getter :: (Has GamesState r, MonadReader r m) =>
                     m GamesState
  gameMay <- getGameFromState (gameId gameToAdd) gamesState
  checkBool (isNothing gameMay) $ Conflict "Game already exists."
  atomically $ modifyTVar gamesState (<> [gameToAdd])

removeGame :: GameOperation r m => GameId -> m ()
removeGame gameToRemoveId = do
  game <- getGame gameToRemoveId
  modifyState $ removeFromState game
  where
    removeFromState game = filter (\g -> gameId g /= gameId game)

getGame :: GameOperation r m => GameId -> m Game
getGame searchedId = do
  gameMay <- getGameMay searchedId
  checkMaybe gameMay $ NotFound $ "Game with id " <> tshow searchedId <> " was not found."

getGameMay :: GameOperation r m => GameId -> m (Maybe Game)
getGameMay searchedId = do
  games <- asks getter
  getGameFromState searchedId games

getActiveGames :: GameOperation r m => m [Game]
getActiveGames = do
  games <- asks getter
  filter (not . gameIsOver) <$> readTVarIO games

fillCell :: GameOperation r m => UserId -> Int -> Game -> m Game
fillCell userId submittedCellNumber oldGame = do
  checkBool (not $ gameIsOver oldGame) $ ValidationError "Game is already over."
  checkBool (userId == gameNextPlayer oldGame) $ ValidationError "You are not the next player."
  playerNumber <- checkMaybe (getPlayerNumber userId oldGame) $ ValidationError "You are not a participant in the game."
  let takenCells = map boardCellNumber $ filter (isJust . boardCellValue) $ gameBoardCells $ gameBoard oldGame
      isCellTaken = submittedCellNumber `elem` takenCells
  checkBool (not isCellTaken) $ ValidationError $ "Cell " <> tshow submittedCellNumber <> " is already taken."
  let cells = gameBoardCells . gameBoard $ oldGame
      newCell = BoardCell (Just $ fromPlayerNum playerNumber) submittedCellNumber
      newCells = newCell : filter (\c -> boardCellNumber c /= submittedCellNumber) cells
      newBoard = (gameBoard oldGame) {gameBoardCells = newCells}
      (_, newGame) =
        checkGameWonOrDraw userId $
        oldGame {gameBoard = newBoard, gameNextPlayer = getNextPlayer playerNumber oldGame}
  changeGameState oldGame newGame
  return newGame

getGameFromState :: GameOperation r m => GameId -> GamesState -> m (Maybe Game)
getGameFromState gId gamesState = find (\g -> gameId g == gId) <$> readTVarIO gamesState

modifyState :: GameOperation r m => ([Game] -> [Game]) -> m ()
modifyState f = do
  gamesState <- asks getter
  atomically $ modifyTVar gamesState f

emptyGameBoard :: GameBoard
emptyGameBoard = GameBoard $ map (BoardCell Nothing) [0 .. 8]

toGameView :: Game -> GameView
toGameView (Game id (GameBoard cells) player1 player2 isOver nextPlayer winner) =
  GameView id (GameBoardView $ map toCellView cells) player1 player2 isOver nextPlayer winner
  where
    toCellView (BoardCell val num) = BoardCellView (toPlayerNum <$> val) num

getNextPlayer :: Int -> Game -> UserId
getNextPlayer 1 (Game _ _ p1 p2 _ _ _) = p2
getNextPlayer 2 (Game _ _ p1 p2 _ _ _) = p1
getNextPlayer _ (Game _ _ _ _ _ next _) = next

checkGameWonOrDraw :: UserId -> Game -> (Bool, Game)
checkGameWonOrDraw lastPlayer game
  | isWon game = (True, game {gameIsOver = True, gameWinner = Just lastPlayer})
  | isADraw game = (True, game {gameIsOver = True, gameWinner = Nothing})
  | otherwise = (False, game)

isWon :: Game -> Bool
isWon (Game _ (GameBoard board) _ _ _ _ _) = do
  let boardToRows =
        chunksOf 3 . map boardCellValue . sortBy (\c c' -> boardCellNumber c `compare` boardCellNumber c') $ board
  any full $ diagonals boardToRows ++ rows boardToRows ++ cols boardToRows
  where
    full [Just a, Just b, Just c] = a == b && b == c
    full _ = False
    diagonals [[a1, _, b1], [_, c, _], [b2, _, a2]] = [[a1, c, a2], [b1, c, b2]]
    rows = id
    cols = transpose

isADraw :: Game -> Bool
isADraw (Game _ (GameBoard board) _ _ _ _ _) = all (isJust . boardCellValue) board

getPlayerNumber :: UserId -> Game -> Maybe Int
getPlayerNumber userId (Game _ _ p1 p2 _ _ _)
  | p1 == userId = Just 1
  | p2 == userId = Just 2
  | otherwise = Nothing

toPlayerNum :: BoardCellValue -> Int
toPlayerNum X = 1
toPlayerNum O = 2

fromPlayerNum :: Int -> BoardCellValue
fromPlayerNum 1 = X
fromPlayerNum 2 = O
fromPlayerNum _ = error "TODO: Fix boundaries"