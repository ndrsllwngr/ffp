module Game.Util (calculateTimeElapsed,
                  getGameStateEntityAndKey,
                  getHeightAndWidthFromBoard) where

import           Data.Matrix
import           Model
import           Game.Board  (Dimension, coordinateToCellNumber)
import           Import (headEx, Entity, entityKey, entityVal)
import Data.Time (UTCTime, diffUTCTime)


getHeightAndWidthFromBoard :: [Row] -> (Int, Int)
getHeightAndWidthFromBoard rows = (length rows, length $ rowCells $ headEx rows )

calculateTimeElapsed :: UTCTime -> Int -> UTCTime -> Int
calculateTimeElapsed lastStartedAt timePrevElapsed now = do
  let (timeElapsed, _) = properFraction $ diffUTCTime now lastStartedAt
  timePrevElapsed + timeElapsed
  
getGameStateEntityAndKey :: [Entity GameStateEntity] -> (GameStateEntity, Key GameStateEntity)
getGameStateEntityAndKey (x:_) = (entityVal x, entityKey x)
getGameStateEntityAndKey _     = error "HELP ME!"