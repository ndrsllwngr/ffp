{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
module Handler.Games where

import Import
import Yesod.Form.Bootstrap3 (BootstrapFormLayout (..), renderBootstrap3)
import Text.Julius (RawJS (..))
import Game.Game

-- Define our data that will be used for creating the form.
data FileForm = FileForm
    { fileInfo :: FileInfo
    , fileDescription :: Text
    }

getGamesR :: Handler Html
getGamesR = do
    allComments <- runDB $ getAllGames

    defaultLayout $ do
        --let (commentFormId, commentTextareaId, commentListId) = commentIds
        --aDomId <- newIdent
        setTitle "Welcome To Yesod!"
        -- $(widgetFile "homepage")


-- NEW GAME
postGamesR :: Handler Value
postGamesR = do
    -- requireCheckJsonBody will parse the request body into the appropriate type, or return a 400 status code if the request JSON is invalid.
    -- (The ToJSON and FromJSON instances are derived in the config/models file).
    newGameEntity <- (requireCheckJsonBody :: Handler NewGameEntity)
    print $ newGameEntity
    let gameState = newGame (newGameEntityHeight newGameEntity, newGameEntityWidth newGameEntity) (newGameEntityBombCount newGameEntity) (newGameEntitySeed newGameEntity)
    let gameStateEntity = gameStateToGameStateEntity gameState (newGameEntityGameId newGameEntity)
    print $ gameStateEntity
    -- The YesodAuth instance in Foundation.hs defines the UserId to be the type used for authentication.
    --maybeCurrentUserId <- maybeAuthId
    --let newGameEntity' = newGameEntity { newGameEntityUserId = maybeCurrentUserId }
    insertedGameState <- runDB $ insertEntity gameStateEntity
    returnJson insertedGameState

-- postGameR :: Handler Value
-- postGameR = do
--     -- requireCheckJsonBody will parse the request body into the appropriate type, or return a 400 status code if the request JSON is invalid.
--     -- (The ToJSON and FromJSON instances are derived in the config/models file).
--     cell <- (requireCheckJsonBody :: Handler Game)
--     print $ cell
--     -- The YesodAuth instance in Foundation.hs defines the UserId to be the type used for authentication.
--     maybeCurrentUserId <- maybeAuthId
--     let cell' = cell { gameUserId = maybeCurrentUserId }
--     insertedCell <- runDB $ insertEntity cell'
--     returnJson insertedCell

getAllGames :: DB [Entity Game]
getAllGames = selectList [] []