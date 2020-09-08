{-# LANGUAGE BlockArguments        #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NoImplicitPrelude     #-}
{-# LANGUAGE OverloadedStrings     #-}
module Handler.ResetR where



import           Game.Game
import           Marshalling
import           Handler.GameR
import           Import


-- RESET GAME
postResetR :: Text -> Handler Value
postResetR gameIdText = do
    -- Prepare required variables
    let gameId = unpack gameIdText
    now <- liftIO getCurrentTime
    -- Read GameState from DB
    gameStateDBEntities <- runDB $ selectList [GameStateEntityGameId ==. gameId] [Desc GameStateEntityUpdatedAt, LimitTo 1]
    let (gsEntity, gsKey) = getGameStateEntityAndKey gameStateDBEntities
    -- Create new game with current properties
    let resetGameState = newGame (getHeightAndWidthFromBoard $ gameStateEntityBoard gsEntity) (gameStateEntityBombCount gsEntity) (gameStateEntitySeed gsEntity)
    -- Keep old game id and createdAt
    let newGameStateEntity = gameStateToGameStateEntity resetGameState gameId (gameStateEntityCreatedAt gsEntity) now now 0
    -- Insert GameState to DB, return GameState
    insertedGameStateEntity <- runDB $ repsert gsKey newGameStateEntity
    returnJson insertedGameStateEntity


getHeightAndWidthFromBoard :: [Row] -> (Int, Int)
getHeightAndWidthFromBoard rows = (length rows, length $ rowCells $ headEx rows )