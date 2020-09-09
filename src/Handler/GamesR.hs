{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NoImplicitPrelude     #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE TypeFamilies          #-}
module Handler.GamesR where

import           Game.Game
import           Marshalling
import           Import
import           Text.Julius           (RawJS (..))
import           Yesod.Form.Bootstrap3 (BootstrapFormLayout (..),
                                        renderBootstrap3)

getGamesR :: Handler Html
getGamesR = do
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
    let newGameState = newGame (newGameRequestHeight newGameRequest, newGameRequestWidth newGameRequest) (newGameRequestBombCount newGameRequest) (newGameRequestSeed newGameRequest)
    let newGameStateEntity = gameStateToGameStateEntity newGameState (newGameRequestGameId newGameRequest) now now now 0
    print newGameStateEntity

    insertedGameStateEntity <- runDB $ insertEntity newGameStateEntity
    returnJson insertedGameStateEntity

variables :: (Text, Text, Text, Text, Text)
variables = ("js-newGameFormId", "js-gameIdField", "js-bombCountField", "js-widthField", "js-heightField")