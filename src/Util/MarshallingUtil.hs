module Util.MarshallingUtil (getRemainingFlags,
                             getCellAssetId) where

import           Game.Board
import           Game.Game
import           Data.Matrix

getRemainingFlags :: Board -> Int -> Int
getRemainingFlags _board _bombCount = _bombCount - length (filter _isFlagged $ toList _board)

getCellAssetId :: GameStatus -> Cell -> String
getCellAssetId gameStatus_ (Cell isFlagged_ isRevealed_ hasBomb_ neighboringBombs_ _) = 
  case gameStatus_ of Ongoing -> getCellAssetIdOngoing isFlagged_ isRevealed_ hasBomb_ neighboringBombs_
                      Won -> getCellAssetIdWon isFlagged_ isRevealed_ hasBomb_ neighboringBombs_
                      Lost -> getCellAssetIdLost isFlagged_ isRevealed_ hasBomb_ neighboringBombs_
                      Paused -> "closed"
                      
-- isFlagged Bool
-- isRevealed Bool
-- hasBomb Bool
-- neighboringBombs Int
getCellAssetIdOngoing :: Bool -> Bool -> Bool -> Int -> String
getCellAssetIdOngoing False True False 0 = "type0"
getCellAssetIdOngoing False True False 1 = "type1"
getCellAssetIdOngoing False True False 2 = "type2"
getCellAssetIdOngoing False True False 3 = "type3"
getCellAssetIdOngoing False True False 4 = "type4"
getCellAssetIdOngoing False True False 5 = "type5"
getCellAssetIdOngoing False True False 6 = "type6"
getCellAssetIdOngoing False True False 7 = "type7"
getCellAssetIdOngoing False True False 8 = "type8"
getCellAssetIdOngoing True True True _   = "mine"
getCellAssetIdOngoing True True False _  = "mine_wrong"
getCellAssetIdOngoing False True True _  = "mine_red"
getCellAssetIdOngoing True False _ _     = "flag"
getCellAssetIdOngoing _ False _ _        = "closed"
getCellAssetIdOngoing _ _ _ _            = error "Invalid CellState"

getCellAssetIdWon :: Bool -> Bool -> Bool -> Int -> String
getCellAssetIdWon False _ False 0    = "type0"
getCellAssetIdWon False _ False 1    = "type1"
getCellAssetIdWon False _ False 2    = "type2"
getCellAssetIdWon False _ False 3    = "type3"
getCellAssetIdWon False _ False 4    = "type4"
getCellAssetIdWon False _ False 5    = "type5"
getCellAssetIdWon False _ False 6    = "type6"
getCellAssetIdWon False _ False 7    = "type7"
getCellAssetIdWon False _ False 8    = "type8"
getCellAssetIdWon True _ True _      = "flag"
getCellAssetIdWon _ _ _ _            = "closed"

getCellAssetIdLost :: Bool -> Bool -> Bool -> Int -> String
getCellAssetIdLost False True False 0 = "type0"
getCellAssetIdLost False True False 1 = "type1"
getCellAssetIdLost False True False 2 = "type2"
getCellAssetIdLost False True False 3 = "type3"
getCellAssetIdLost False True False 4 = "type4"
getCellAssetIdLost False True False 5 = "type5"
getCellAssetIdLost False True False 6 = "type6"
getCellAssetIdLost False True False 7 = "type7"
getCellAssetIdLost False True False 8 = "type8"
getCellAssetIdLost True False False _ = "mine_wrong"
getCellAssetIdLost False True True _  = "mine_red"
getCellAssetIdLost _ False True _     = "mine"
getCellAssetIdLost _ _ _ _            = "closed"