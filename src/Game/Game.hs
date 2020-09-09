{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiParamTypeClasses #-} 
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}

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
                  timeElapsed,  ) where

import           ClassyPrelude.Conduit (UTCTime)
import           Game.Board
import           Game.Util
import           Control.Lens

data Move = Reveal Coordinate UTCTime | RevealAllNonFlagged UTCTime | Flag Coordinate UTCTime deriving (Show, Eq, Read) -- TODO maybe add unflag
data GameStatus = Ongoing | Won | Lost | Paused deriving (Show, Eq, Read)
--derivePersistField "Status"

data GameState = GameState { _gameStateBoard          :: Board,
                             _gameStateMoves          :: [Move],
                             _gameStateBombCount      :: Int,
                             _gameStateSeed           :: Int,
                             _gameStateStatus         :: GameStatus,
                             _gameStateGameId         :: String,
                             _gameStateCreatedAt      :: UTCTime,
                             _gameStateUpdatedAt      :: UTCTime,
                             _gameStateLastStartedAt  :: UTCTime,
                             _gameStateTimeElapsed    :: Int
                            }
makeFields ''GameState                                     

instance Show GameState where
    show (GameState b m bombs s st _ _ _ _ _) = "Bombcount: " ++ show bombs ++ " Seed: " ++ show s ++ " Status: " ++ show st ++ "\n" ++ show m ++ "\n" ++ show b


-- Creates a new game for a given Dimension, bombCount & seed
newGame :: Dimension -> Int -> Int -> String -> UTCTime -> GameState
newGame (h,w) b s gId now = GameState { 
                                _gameStateBoard         = generateBoard (h,w) b s,
                                _gameStateMoves         = [],
                                _gameStateBombCount     = b,
                                _gameStateSeed          = s,
                                _gameStateStatus        = Ongoing,
                                _gameStateGameId        = gId,       
                                _gameStateCreatedAt     = now,    
                                _gameStateUpdatedAt     = now,    
                                _gameStateLastStartedAt = now, 
                                _gameStateTimeElapsed   = 0
                               }

-- Executes a move on a given GameState
-- TODO handle illegal moves e.g. outOfBounds Action, action on a board with status Won || Lost
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
checkStatus :: Board -> GameStatus
checkStatus b = case (checkWon b, checkLost b) of  (_,True)      -> Lost
                                                   (True,False)  -> Won
                                                   (False,False) -> Ongoing
