{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE TemplateHaskell #-}

module Game.Game (newGame,
                  makeMove,
                  GameState (..),
                  GameStatus (..),
                  Move (..)
                  ) where

import           ClassyPrelude.Conduit (UTCTime)
import           Game.Board
import           Game.Util
import           Control.Lens

data Move = Reveal Coordinate UTCTime | RevealAllNonFlagged UTCTime | Flag Coordinate UTCTime deriving (Show, Eq, Read) -- TODO maybe add unflag
data GameStatus = Ongoing | Won | Lost | Paused deriving (Show, Eq, Read)
--derivePersistField "Status"

data GameState = GameState { board          :: Board,
                             moves          :: [Move],
                             bombCount      :: Int,
                             seed           :: Int,
                             status         :: GameStatus,
                             gameId         :: String,
                             createdAt      :: UTCTime,
                             updatedAt      :: UTCTime,
                             lastStartedAt  :: UTCTime,
                             timeElapsed    :: Int
                            }

instance Show GameState where
    show (GameState b m bombs s st _ _ _ _ _) = "Bombcount: " ++ show bombs ++ " Seed: " ++ show s ++ " Status: " ++ show st ++ "\n" ++ show m ++ "\n" ++ show b


-- Creates a new game for a given Dimension, bombCount & seed
newGame :: Dimension -> Int -> Int -> String -> UTCTime -> GameState
newGame (h,w) b s gId now = GameState { board = generateBoard (h,w) b s,
                                moves = [],
                                bombCount = b,
                                seed = s,
                                status = Ongoing,
                                gameId = gId,       
                                createdAt = now,    
                                updatedAt = now,    
                                lastStartedAt = now, 
                                timeElapsed = 0
                               }

-- Executes a move on a given GameState
-- TODO handle illegal moves e.g. outOfBounds Action, action on a board with status Won || Lost
makeMove :: GameState -> Move -> GameState
makeMove state m  = state { board   = boardAfterMove,
                            moves   = moves state ++ [m],
                            status  = st,
                            updatedAt = time,
                            timeElapsed = elapsed
                          } where (boardAfterMove, time)  = case m of (Flag c t) -> (flagCell (board state) c,t)
                                                                      (Reveal c t) -> (revealCell (board state) c,t)
                                                                      (RevealAllNonFlagged t) -> (revealAllNonFlaggedCells (board state),t) 
                                  finishGame              = calculateTimeElapsed (lastStartedAt state) (timeElapsed state) time
                                  st                      = checkStatus boardAfterMove
                                  elapsed                 = case st of Won   -> finishGame
                                                                       Lost  -> finishGame
                                                                       _     -> timeElapsed state
checkStatus :: Board -> GameStatus
checkStatus b = case (checkWon b, checkLost b) of  (_,True)      -> Lost
                                                   (True,False)  -> Won
                                                   (False,False) -> Ongoing
