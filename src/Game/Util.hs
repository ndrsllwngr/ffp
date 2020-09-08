module Game.Util (calculateTimeElapsed
                 ) where

import Data.Time (UTCTime, diffUTCTime)


calculateTimeElapsed :: UTCTime -> Int -> UTCTime -> Int
calculateTimeElapsed lastStartedAt timePrevElapsed now = do
  let (timeElapsed, _) = properFraction $ diffUTCTime now lastStartedAt
  timePrevElapsed + timeElapsed