module Game.Game where

import Game.Board
import Data.Matrix

data Move = Reveal Coordinate | Flag Coordinate deriving Show -- TODO maybe add unflag

data GameState = GameState { board :: Board, 
                             moves :: [Move], 
                             bombCount :: Int,
                             seed :: Int }

instance Show GameState where
   show (GameState board moves bombs seed) = "Bombcount: " ++ show bombs ++ " Seed: " ++ show seed ++ "\n" ++ show moves ++ "\n"++ show board

                             
data Status = Ongoing | Won | Lost deriving Show

newGame :: Dimension -> Int -> Int -> GameState
newGame (h,w) b s = GameState { board = generateBoard (h,w) b s,
                                moves = [],
                                bombCount = b,
                                seed = s }

makeMove :: GameState -> Move -> GameState
makeMove state m  = state { board = boardAfterMove,
                            moves = moves state ++ [m] } 
                              where boardAfterMove = case m of (Flag c)   -> flagCell (board state) c
                                                               (Reveal c) -> revealCell (board state) c
                                                                                    

checkStatus :: GameState -> Status
checkStatus (GameState board _ _ _ ) = case (checkWon board, checkLost board) of  (_,True)      -> Lost
                                                                                  (True,False)  -> Won
                                                                                  (False,False) -> Ongoing