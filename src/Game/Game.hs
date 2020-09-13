{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE TemplateHaskell #-}

module Game.Game (newGame,
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
                  getDimensions,
                  channel,
                  calculateTimeElapsed,
                  isMoveInBounds) where

import           Game.Board
import           Control.Lens
import           Data.Time (diffUTCTime, UTCTime)
import           Network.Wai.EventSource (ServerEvent (..))
import           Control.Concurrent.Chan

data Move = Reveal Coordinate UTCTime | RevealAllNonFlagged UTCTime | Flag Coordinate UTCTime deriving (Show, Eq, Read)
data GameStatus = Ongoing | Won | Lost | Paused deriving (Show, Eq, Read)
--derivePersistField "Status"

data GameState = GameState { _board          :: Board,
                             _moves          :: [Move],
                             _bombCount      :: Int,
                             _seed           :: Int,
                             _status         :: GameStatus,
                             _gameId         :: String,
                             _createdAt      :: UTCTime,
                             _updatedAt      :: UTCTime,
                             _lastStartedAt  :: UTCTime,
                             _timeElapsed    :: Int,
                             _channel        :: Chan ServerEvent
                            }
makeLenses ''GameState

instance Show GameState where
    show (GameState b m bombs s st _ _ _ _ _ _) = "Bombcount: " ++ show bombs ++ " Seed: " ++ show s ++ " Status: " ++ show st ++ "\n" ++ show m ++ "\n" ++ show b


-- Creates a new game for a given Dimension, bombCount & seed
newGame :: Dimension -> Int -> Int -> String -> UTCTime -> Chan ServerEvent -> GameState
newGame (h,w) b s gId now channel_ = GameState {
                                        _board         = generateBoard (h,w) b s,
                                        _moves         = [],
                                        _bombCount     = b,
                                        _seed          = s,
                                        _status        = Ongoing,
                                        _gameId        = gId,
                                        _createdAt     = now,
                                        _updatedAt     = now,
                                        _lastStartedAt = now,
                                        _timeElapsed   = 0,
                                        _channel       = channel_
                                     }

-- Executes a move on a given GameState
makeMove :: GameState -> Move -> GameState
makeMove state m  = state & board       .~ boardAfterMove
                          & moves       .~ state ^. moves ++ [m]
                          & status      .~ st
                          & updatedAt   .~ time
                          & timeElapsed .~ elapsed
                          where (boardAfterMove, time)  = case m of (Flag c t) -> (flagCell (state ^. board) c,t)
                                                                    (Reveal c t) -> (revealCell (state ^. board ) c,t)
                                                                    (RevealAllNonFlagged t) -> (revealAllNonFlaggedCells (state ^. board),t)
                                finishGame              = calculateTimeElapsed (state ^. lastStartedAt) (state ^. timeElapsed) time
                                st                      = checkStatus boardAfterMove
                                elapsed                 = case st of Won   -> finishGame
                                                                     Lost  -> finishGame
                                                                     _     -> state ^. timeElapsed
   
   
-- Returns the Status of a given board
isGameOver :: GameState -> Bool
isGameOver state = case state ^. status of Ongoing -> False
                                           Paused -> False
                                           Lost -> True
                                           Won -> True
                                       
                                                                  
                                                                     
-- Returns the Status of a given board
checkStatus :: Board -> GameStatus
checkStatus b = case (checkWon b, checkLost b) of  (_,True)      -> Lost
                                                   (True,False)  -> Won
                                                   (False,False) -> Ongoing                      

-- Returns the Dimension of the board contained by a given GameState
getDimensions :: GameState -> Dimension
getDimensions state = getDimensionsForBoard $ state ^. board

calculateTimeElapsed :: UTCTime -> Int -> UTCTime -> Int
calculateTimeElapsed lastStartedAt_ timePrevElapsed now = do
  let (timeElapsed_, _) = properFraction $ diffUTCTime now lastStartedAt_
  timePrevElapsed + timeElapsed_
  
isMoveInBounds:: Move -> GameState -> Bool
isMoveInBounds (Reveal c _) gameState = inBounds c (getDimensions gameState)
isMoveInBounds (Flag c _) gameState = inBounds c (getDimensions gameState)
isMoveInBounds _ _ = True


 