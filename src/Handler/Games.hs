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

-- Define our data that will be used for creating the form.
data FileForm = FileForm
    { fileInfo        :: FileInfo
    , fileDescription :: Text
    }

getGamesR :: Handler Html
getGamesR = do
    allComments <- runDB $ getAllGames
    defaultLayout $ do
        setTitle "Welcome To Yesod!"

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

getAllGames :: DB [Entity Game]
getAllGames = selectList [] []
