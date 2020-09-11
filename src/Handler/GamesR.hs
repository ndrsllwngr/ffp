{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NoImplicitPrelude     #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE TypeFamilies          #-}
module Handler.GamesR where

import           Game.Game
import           Game.StateUtil
import           Marshalling
import           Game.Util
import           Import
import           Text.Julius           (RawJS (..))
import           Control.Lens
import System.Random (random, randomIO)


getGamesR :: Handler Html
getGamesR = do
    app <- getYesod
    -- Get the in-memory state of ongoing games
    let tGames = games app
    -- Get games from database
    gameStateDBEntities <- runDB $ selectList [] [Desc GameStateEntityUpdatedAt]
    let gameStateEntities = map entityVal gameStateDBEntities
    -- List of paused games
    let gameStateEntitiesPaused = filter (\gs -> gs ^. gameStateEntityStatus == "Paused") gameStateEntities
    -- List of finished games
    let gameStateEntitiesWonOrLost = filter (\gs -> gs ^. gameStateEntityStatus == "Lost" || gs ^. gameStateEntityStatus == "Won") gameStateEntities
    -- Get ongoing games from in-memory State
    gamesOngoing <- liftIO $ getAllGames tGames
    -- Parse List of GameStates to a List of GameStateEntities
    let gameStateEntitiesOngoing = map gameStateToGameStateEntity gamesOngoing
    defaultLayout $ do
            let (newGameFormId, gameIdField, bombCountField, widthField, heightField, randomSeedField) = variables
            setTitle "Create New Game"
            $(widgetFile "games")

-- INIT NEW GAME
postGamesR :: Handler Value
postGamesR = do
    app <- getYesod
    now <- liftIO getCurrentTime
    -- Get the in-memory state of ongoing games
    let tGames = games app
    
    -- Parse the newGameRequest
    newGameRequest <- (requireCheckJsonBody :: Handler NewGameRequest)
    let newGameId = newGameRequest ^. newGameRequestGameId
    
    seed_ <- case newGameRequest ^. newGameRequestSeed of Just s -> return s
                                                          Nothing -> liftIO $ (randomIO :: IO Int)
    
    -- create new game
    let newGameState = newGame (newGameRequest ^. newGameRequestHeight, newGameRequest ^. newGameRequestWidth) (newGameRequest ^. newGameRequestBombCount) seed_ newGameId now
    -- write the new game into the in-memory state
    _ <- liftIO $ setGameStateForGameId tGames newGameId newGameState
    returnJson $ gameStateToGameStateEntity newGameState 

variables :: (Text, Text, Text, Text, Text, Text)
variables = ("js-newGameFormId", "js-gameIdField", "js-bombCountField", "js-widthField", "js-heightField", "js-randomSeedField")

showSize :: [Row] -> String
showSize b = showS (getHeightAndWidthFromBoard b) where showS (h,w) = "width: " ++ show w ++ ", height: " ++ show h