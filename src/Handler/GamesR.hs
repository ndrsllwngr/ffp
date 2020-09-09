{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NoImplicitPrelude     #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE TypeFamilies          #-}
module Handler.GamesR where

import           Game.Game
import           Marshalling
import           Game.Util
import           Import
import           Text.Julius           (RawJS (..))
import           Yesod.Form.Bootstrap3 (BootstrapFormLayout (..),
                                        renderBootstrap3)

getGamesR :: Handler Html
getGamesR = do

    gameStateDBEntities <- runDB $ selectList [] [Desc GameStateEntityUpdatedAt]
    let gameStateEntities = map entityVal gameStateDBEntities
    let gameStateEntitiesOngoingOrPaused = filter (\x -> gameStateEntityStatus x == "Ongoing" || gameStateEntityStatus x == "Paused") gameStateEntities
    let gameStateEntitiesWonOrLost = filter (\x -> gameStateEntityStatus x == "Lost" || gameStateEntityStatus x == "Won") gameStateEntities
    defaultLayout $ do
            let (newGameFormId, gameIdField, bombCountField, widthField, heightField) = variables
            setTitle "Create New Game"
            $(widgetFile "games")

-- INIT NEW GAME
postGamesR :: Handler Value
postGamesR = do
    -- requireCheckJsonBody will parse the request body into the appropriate type, or return a 400 status code if the request JSON is invalid.
    -- (The ToJSON and FromJSON instances are derived in the config/models file).
    newGameRequest <- (requireCheckJsonBody :: Handler NewGameRequest)
    now <- liftIO getCurrentTime
    print newGameRequest
    let newGameState = newGame (newGameRequestHeight newGameRequest, newGameRequestWidth newGameRequest) (newGameRequestBombCount newGameRequest) (newGameRequestSeed newGameRequest) (newGameRequestGameId newGameRequest) now
    let newGameStateEntity = gameStateToGameStateEntity newGameState 
    print newGameStateEntity

    insertedGameStateEntity <- runDB $ insertEntity newGameStateEntity
    returnJson insertedGameStateEntity

variables :: (Text, Text, Text, Text, Text)
variables = ("js-newGameFormId", "js-gameIdField", "js-bombCountField", "js-widthField", "js-heightField")

showSize :: [Row] -> String
showSize b = showS (getHeightAndWidthFromBoard b) where showS (h,w) = "width: " ++ show w ++ ", height: " ++ show h