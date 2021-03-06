{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE TemplateHaskell #-}

module Game.Game
  ( newGame,
    makeMove,
    GameState (..),
    GameStatus (..),
    Move (..),
    board,
    moves,
    bombCount,
    seed,
    status,
    gameId,
    createdAt,
    updatedAt,
    lastStartedAt,
    timeElapsed,
    isGameOver,
    isGameOverStatus,
    getDimensions,
    channel,
    calculateTimeElapsed,
    isMoveInBounds,
    isValidGameConfig,
  )
where

import Control.Concurrent.Chan
import Control.Lens
import Data.Time (UTCTime, diffUTCTime)
import Game.Board
import Network.Wai.EventSource (ServerEvent (..))

data Move = Reveal Coordinate UTCTime | RevealAllNonFlagged UTCTime | Flag Coordinate UTCTime deriving (Show, Eq, Read)

data GameStatus = Ongoing | Won | Lost | Paused deriving (Show, Eq, Read)

data GameState = GameState
  { _board :: Board,
    _moves :: [Move],
    _bombCount :: Int,
    _seed :: Int,
    _status :: GameStatus,
    _gameId :: String,
    _createdAt :: UTCTime,
    _updatedAt :: UTCTime,
    _lastStartedAt :: Maybe UTCTime,
    _timeElapsed :: Int,
    _channel :: Chan ServerEvent
  }

makeLenses ''GameState

instance Show GameState where
  show (GameState b m bombs s st _ _ _ _ _ _) = "Bombcount: " ++ show bombs ++ " Seed: " ++ show s ++ " Status: " ++ show st ++ "\n" ++ show m ++ "\n" ++ show b

-- Creates a new game for a given Dimension, bombCount & seed
newGame :: Dimension -> Int -> Int -> String -> UTCTime -> Chan ServerEvent -> GameState
newGame (h, w) b s gId now channel_ =
  GameState
    { _board = generateBoard (h, w) b s,
      _moves = [],
      _bombCount = b,
      _seed = s,
      _status = Ongoing,
      _gameId = gId,
      _createdAt = now,
      _updatedAt = now,
      _lastStartedAt = Nothing,
      _timeElapsed = 0,
      _channel = channel_
    }

-- Executes a move on a given GameState
makeMove :: GameState -> Move -> GameState
makeMove state m =
  state & board .~ boardAfterMove
    & moves .~ state ^. moves ++ [m]
    & status .~ st
    & updatedAt .~ time
    & timeElapsed .~ elapsed
  where
    (boardAfterMove, time) = case m of
      (Flag c t) -> (flagCell (state ^. board) c, t)
      (Reveal c t) -> (reveal (state ^. board) c, t)
      (RevealAllNonFlagged t) -> (revealAllNonFlaggedCells (state ^. board), t)
    finishGame = calculateTimeElapsed (state ^. lastStartedAt) (state ^. timeElapsed) time
    st = checkStatus boardAfterMove
    elapsed = if isGameOverStatus st then finishGame else state ^. timeElapsed

-- Returns the Status of a given board
isGameOverStatus :: GameStatus -> Bool
isGameOverStatus st = case st of
  Ongoing -> False
  Paused -> False
  Lost -> True
  Won -> True

-- Returns the Status of a given board
isGameOver :: GameState -> Bool
isGameOver state = isGameOverStatus $ state ^. status 

-- Returns the Status of a given board
checkStatus :: Board -> GameStatus
checkStatus b = case (checkWon b, checkLost b) of
  (_, True) -> Lost
  (True, False) -> Won
  (False, False) -> Ongoing

-- Returns the Dimension of the board contained by a given GameState
getDimensions :: GameState -> Dimension
getDimensions state = getDimensionsForBoard $ state ^. board

calculateTimeElapsed :: Maybe UTCTime -> Int -> UTCTime -> Int
calculateTimeElapsed lastStartedAt_ timePrevElapsed now = case lastStartedAt_ of
  Just lsa -> timePrevElapsed + fst (properFraction $ diffUTCTime now lsa)
  Nothing -> 0

isMoveInBounds :: Move -> GameState -> Bool
isMoveInBounds (Reveal c _) gameState = inBounds c (getDimensions gameState)
isMoveInBounds (Flag c _) gameState = inBounds c (getDimensions gameState)
isMoveInBounds _ _ = True


isValidGameConfig :: Dimension -> Int -> Bool
isValidGameConfig (h, w) bombCount_ = h > 0 && w > 0 && bombCount_ > 0 && h * w > bombCount_