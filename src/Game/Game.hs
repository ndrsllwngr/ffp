module Game.Game (newGame,
                  makeMove,
                  GameState) where

import Game.Board
import Data.Matrix

data Move = Reveal Coordinate | Flag Coordinate deriving Show -- TODO maybe add unflag
data Status = Ongoing | Won | Lost deriving Show

data GameState = GameState { board :: Board,
                             moves :: [Move],
                             bombCount :: Int,
                             seed :: Int,
                             status :: Status}

instance Show GameState where
   show (GameState board moves bombs seed status) = "Bombcount: " ++ show bombs ++ " Seed: " ++ show seed ++ " Status: " ++ show status ++ "\n" ++ show moves ++ "\n"++ show board

-- Creates a new game for a given Dimension, bombCount & seed
newGame :: Dimension -> Int -> Int -> GameState
newGame (h,w) b s = GameState { board = generateBoard (h,w) b s,
                                moves = [],
                                bombCount = b,
                                seed = s,
                                status = Ongoing}

-- Executes a move on a given GameState
-- TODO handle illegal moves e.g. outOfBounds Action, action on a board with status Won || Lost
makeMove :: GameState -> Move -> GameState
makeMove state m  = state { board   = boardAfterMove,
                            moves   = moves state ++ [m],
                            status  = checkStatus boardAfterMove}
                              where boardAfterMove = case m of (Flag c)   -> flagCell (board state) c
                                                               (Reveal c) -> revealCell (board state) c


checkStatus :: Board -> Status
checkStatus board = case (checkWon board, checkLost board) of  (_,True)      -> Lost
                                                               (True,False)  -> Won
                                                               (False,False) -> Ongoing