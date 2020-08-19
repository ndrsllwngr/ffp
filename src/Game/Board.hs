module Game.Board where

import Data.Matrix

type Coordinate = (Int,Int)

data Cell = Cell { isRevealed :: Bool  
                 , isFlagged :: Bool  
                 , hasBomb :: Bool  
                 , neighboringBombs :: Int
                 } deriving (Show)  

type Field = Matrix Cell
