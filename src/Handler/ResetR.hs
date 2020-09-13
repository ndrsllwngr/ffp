{-# LANGUAGE BlockArguments        #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NoImplicitPrelude     #-}
{-# LANGUAGE OverloadedStrings     #-}
module Handler.ResetR where

import           Game.Game 
import           Util.StateUtil
import           Marshalling
import           Util.HandlerUtil
import           Import
import           Control.Lens
import           Handler.ChannelR(broadcast)
import           System.Random (randomIO)
import           Text.StringRandom

-- RESET GAME
postResetR :: Text -> Handler Value
postResetR gameIdText = do
    app <- getYesod    
    -- Get the in-memory state of ongoing games
    let tGames = games app
    let gameId_ = unpack gameIdText
    -- Try to get the game from the in-memory state
    gameStateMaybe <- liftIO $ getGameById tGames gameId_
    
    -- TODO can this be written more elegantly so we dont have to have two subsequent case matches?
    gameMaybe <- case gameStateMaybe of
                      -- if game was in in-memory state return it
                      Just gameState -> return $ Just gameState
                      -- if game was not in in-memory try to fetch it from the Database
                      Nothing -> do
                          -- Try to fetch game from Database  
                          gameStateDBEntities <- runDB $ selectList [GameStateEntityGameId ==. unpack gameIdText] [Desc GameStateEntityUpdatedAt, LimitTo 1]
                          -- Delete game with given id from Database (if present)
                          _ <- runDB $ deleteBy $ UniqueGameStateEntity gameId_ 
                          -- Create new channel
                          channel_ <- newChan
                          -- Map the Maybe GameStateEntity to a Maybe GameState and return it
                          return $ (`gameStateEntityToGameState` channel_) <$> getGameStateEntityMaybe gameStateDBEntities
                      
    case gameMaybe of
         Just gameState -> do
                now <- liftIO getCurrentTime
               
                -- Generate a new 5 character random game id
                randomString <- liftIO $ stringRandomIO "^[A-Z1-9]{5}$"
                
                -- Generate a new random seed
                newSeed <- liftIO (randomIO :: IO Int)
                
                -- if the game was already over use a new game id otherwise use the same game id for the new game
                let gameId_ = if isGameOver gameState then unpack randomString else gameState ^.gameId
                -- create a new game with the same parameters
                let resetGameState = newGame (getDimensions gameState) (gameState ^. bombCount) newSeed gameId_ now (gameState ^. channel)
                -- Store/Override the GameState for the given ID in the in-memory state
                _ <- liftIO $ setGameStateForGameId tGames gameId_ resetGameState
                let gameStateEntity = gameStateToGameStateEntity resetGameState
                broadcast (resetGameState ^. channel) gameStateEntity
                returnJson gameStateEntity
         -- If game was neither in Memory (Ongoing) nor in Database (Paused/Won/Lost) return 404             
         Nothing -> notFound