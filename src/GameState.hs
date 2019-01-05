module GameState where

import Board
import Piece(Color) 
import Control.Monad.State.Lazy

data GameState = GameState
  { key :: String,
    player1 :: Player,
    player2 :: Player,
    board :: Board}

data Player = Player {
    name :: String,
    color :: Color,
    castellingState :: CastellingState,
    takenPieces :: [Piece]
}

data CastellingState = CastellingState {
    kingMoved :: Bool,
    leftTowerMoved :: Bool,
    rightTowerMoved :: Bool
}