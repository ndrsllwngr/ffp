module GameState where

import Board

data GameState = GameState
  { key :: String,
    player1 :: Player,
    player2 :: Player,
    board :: Board}

data Player = Player {
    name :: String,
    castellingState :: CastellingState,
    takenPieces :: [Piece]
}

data CastellingState = CastellingState {
    kingMoved :: Bool,
    leftTowerMoved :: Bool,
    rightTowerMoved :: Bool,
}