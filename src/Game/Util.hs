module Game.Util (calculateTimeElapsed,
                  getGameStateEntityMaybe,
                  getHeightAndWidthFromBoard) where

import           Model
import           Import.NoFoundation (headEx, Entity, entityVal)
import           Data.Time (UTCTime, diffUTCTime)
import           Control.Lens


getHeightAndWidthFromBoard :: [Row] -> (Int, Int)
getHeightAndWidthFromBoard rows = (length rows, length $ headEx rows ^. rowCells)

calculateTimeElapsed :: UTCTime -> Int -> UTCTime -> Int
calculateTimeElapsed lastStartedAt timePrevElapsed now = do
  let (timeElapsed, _) = properFraction $ diffUTCTime now lastStartedAt
  timePrevElapsed + timeElapsed
  
getGameStateEntityMaybe :: [Entity GameStateEntity] -> Maybe GameStateEntity
getGameStateEntityMaybe (x:_) = Just $ entityVal x
getGameStateEntityMaybe _     = Nothing
