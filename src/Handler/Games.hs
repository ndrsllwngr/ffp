{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NoImplicitPrelude     #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE TypeFamilies          #-}
module Handler.Games where

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
    print $ newGameEntity
    let gameState = newGame (newGameEntityHeight newGameEntity, newGameEntityWidth newGameEntity) (newGameEntityBombCount newGameEntity) (newGameEntitySeed newGameEntity)
    let gameStateEntity = gameStateToGameStateEntity gameState (newGameEntityGameId newGameEntity) (newGameEntityCreatedAt newGameEntity) (newGameEntityCreatedAt newGameEntity)
    print $ gameStateEntity
    -- The YesodAuth instance in Foundation.hs defines the UserId to be the type used for authentication.
    --maybeCurrentUserId <- maybeAuthId
    --let newGameEntity' = newGameEntity { newGameEntityUserId = maybeCurrentUserId }
    insertedGameState <- runDB $ insertEntity gameStateEntity
    returnJson insertedGameState

variables :: (Text, Text, Text, Text, Text)
variables = ("js-newGameFormId", "js-gameIdField", "js-bombCountField", "js-widthField", "js-heightField")