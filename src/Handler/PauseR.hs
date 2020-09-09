{-# LANGUAGE BlockArguments        #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NoImplicitPrelude     #-}
{-# LANGUAGE OverloadedStrings     #-}
module Handler.PauseR where



import           Game.Game
import           Game.Util
import           Import

-- PAUSE GAME
postPauseR :: Text -> Handler Value
postPauseR gameIdText = do
    -- Prepare required variables
    let gameId = unpack gameIdText
    now <- liftIO getCurrentTime
    -- Read GameState from DB
    gameStateDBEntities <- runDB $ selectList [GameStateEntityGameId ==. gameId] [Desc GameStateEntityUpdatedAt, LimitTo 1]
    let (gsEntity, gsKey) = getGameStateEntityAndKey gameStateDBEntities
    -- Set GameState to "Paused" and and set timeElapsed as the time that elapsed playing the game until now
    let updatedGameStateEntity = gsEntity {
                                    gameStateEntityStatus = "Paused",
                                    gameStateEntityUpdatedAt = now,
                                    gameStateEntityTimeElapsed = calculateTimeElapsed (gameStateEntityLastStartedAt gsEntity) (gameStateEntityTimeElapsed gsEntity) now
                                  }
    -- Insert GameState to DB, return GameState                       
    insertedGameStateEntity <- runDB $ repsert gsKey updatedGameStateEntity
    returnJson insertedGameStateEntity -- TODO check if return is working                                   

