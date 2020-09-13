{-# LANGUAGE BlockArguments        #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NoImplicitPrelude     #-}
{-# LANGUAGE OverloadedStrings     #-}
module Handler.PauseR where

import           Util.StateUtil
import           Game.Game
import           Marshalling
import           Import
import           Control.Lens
import           Handler.ChannelR(broadcast)

-- PAUSE GAME
postPauseR :: Text -> Handler Value
postPauseR gameIdText = do
    app <- getYesod
    -- Get the in-memory state of ongoing games
    let tGames = games app
    let gameId_ = unpack gameIdText

    -- Try to get the game from the in-memory state
    gameStateMaybe <- liftIO $ getGameById tGames gameId_ 
    
    case gameStateMaybe of 
      Just gameState -> do 
          now <- liftIO getCurrentTime
          let gsEntity = gameStateToGameStateEntity gameState
          -- remove game from in memory state
          _ <- liftIO $ removeGameById tGames gameId_
          -- Insert GameState to DB with status Paused & updated timestamps
          let  updatedGameStateEntity = gsEntity & gameStateEntityStatus .~ "Paused"
                                                 & gameStateEntityUpdatedAt .~ now
                                                 & gameStateEntityTimeElapsed .~ calculateTimeElapsed (gsEntity ^. gameStateEntityLastStartedAt) (gsEntity ^. gameStateEntityTimeElapsed) now
          _ <- runDB $ insert updatedGameStateEntity

          broadcast (gameState ^. channel) updatedGameStateEntity
          -- return GameState
          returnJson updatedGameStateEntity   
      Nothing -> notFound                                 

