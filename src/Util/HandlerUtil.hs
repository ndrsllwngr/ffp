module Util.HandlerUtil
  ( getGameStateEntityMaybe,
    getHeightAndWidthFromBoard,
    getTimeElapsed,
  )
where

import Control.Lens
import Game.Game (calculateTimeElapsed)
import Import.NoFoundation (Entity, UTCTime, entityVal, headEx)
import Model

getHeightAndWidthFromBoard :: [Row] -> (Int, Int)
getHeightAndWidthFromBoard rows = (length rows, length $ headEx rows ^. rowCells)

getGameStateEntityMaybe :: [Entity GameStateEntity] -> Maybe GameStateEntity
getGameStateEntityMaybe (x : _) = Just $ entityVal x
getGameStateEntityMaybe _ = Nothing

getTimeElapsed :: Maybe UTCTime -> Int -> UTCTime -> String -> Int
getTimeElapsed lastStartedAt_ timeElapsed_ now status_ = case status_ of
  "Won" -> timeElapsed_
  "Lost" -> timeElapsed_
  _ -> calculateTimeElapsed lastStartedAt_ timeElapsed_ now
