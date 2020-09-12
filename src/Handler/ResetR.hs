{-# LANGUAGE BlockArguments        #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NoImplicitPrelude     #-}
{-# LANGUAGE OverloadedStrings     #-}
module Handler.ResetR where

import           Game.Game 
import           Game.StateUtil
import           Marshalling
import           Game.Util
import           Import
import           Control.Lens
import           Handler.ChannelR(broadcast)
import           System.Random (randomIO)

-- RESET GAME
postResetR :: Text -> Handler Value
postResetR gameIdText = do
    app <- getYesod    -- Get the in-memory state of ongoing games
    let tGames = games app
    let gameId_ = unpack gameIdText
    newSeed <- liftIO (randomIO :: IO Int)
    -- Try to get the game from the in-memory state
    gameStateMaybe <- liftIO $ getGameById tGames gameId_
    
    -- todo can this be written more elegantly so we dont have to have two  subsequent case matches?
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
                          -- Map the Maybe GameStateEntity to a Maybe Gamestate and return it
                          return $ (`gameStateEntityToGameState` channel_) <$> getGameStateEntityMaybe gameStateDBEntities
                       
                         
    
    case gameMaybe of
         Just gameState -> do
                now <- liftIO getCurrentTime
                -- create a new game with the same parameters
                let resetGameState = newGame (getDimensions gameState) (gameState ^. bombCount) newSeed (gameState ^.gameId) now (gameState ^. channel)
                -- Store/Override the gamestate for the given ID in the in-memory state
                _ <- liftIO $ setGameStateForGameId tGames gameId_ resetGameState
                let gameStateEntity = gameStateToGameStateEntity resetGameState
                broadcast (resetGameState ^. channel) gameStateEntity
                returnJson gameStateEntity
         -- If game was neither in Memory (Ongoing) nor in Database (Paused/Won/Lost) return 404             
         Nothing -> notFound