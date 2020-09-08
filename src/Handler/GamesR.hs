{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NoImplicitPrelude     #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE TypeFamilies          #-}
module Handler.GamesR where

import           Game.Game
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
    newGameEntity <- (requireCheckJsonBody :: Handler NewGameEntity)
    now <- liftIO getCurrentTime
    print newGameEntity
    let newGameState = newGame (newGameEntityHeight newGameEntity, newGameEntityWidth newGameEntity) (newGameEntityBombCount newGameEntity) (newGameEntitySeed newGameEntity)
    let newGameStateEntity = gameStateToGameStateEntity newGameState (newGameEntityGameId newGameEntity) now now
    print newGameStateEntity
  
    insertedGameStateEntity <- runDB $ insertEntity newGameStateEntity
    returnJson insertedGameStateEntity

variables :: (Text, Text, Text, Text, Text)
variables = ("js-newGameFormId", "js-gameIdField", "js-bombCountField", "js-widthField", "js-heightField")