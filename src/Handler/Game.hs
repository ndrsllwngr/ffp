{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
module Handler.Game where

import Import
import Yesod.Form.Bootstrap3 (BootstrapFormLayout (..), renderBootstrap3)
import Text.Julius (RawJS (..))
import Game.Game

-- Define our data that will be used for creating the form.
data FileForm = FileForm
    { fileInfo :: FileInfo
    , fileDescription :: Text
    }

getGameR :: Text -> Handler Value
getGameR gameId = do
    print $ unpack gameId
    gameState <- runDB $ selectList [GameStateEntityGameId ==. unpack gameId] [LimitTo 1]
    print gameState
    returnJson gameState

-- MAKE MOVE
putGameR :: Handler Value
putGameR = do
    -- requireCheckJsonBody will parse the request body into the appropriate type, or return a 400 status code if the request JSON is invalid.
    -- (The ToJSON and FromJSON instances are derived in the config/models file).
    moveEntity <- (requireCheckJsonBody :: Handler MoveEntity)
    print $ moveEntity
    -- let gameState = makeMove state moveEntityAction moveEntity (moveEntityCoordX moveEntity, moveEntityCoordY moveEntity)
    -- print $ gameState
    -- -- The YesodAuth instance in Foundation.hs defines the UserId to be the type used for authentication.
    -- maybeCurrentUserId <- maybeAuthId
    -- --let newGameEntity' = newGameEntity { newGameUserId = maybeCurrentUserId }
    -- insertedGameState <- runDB $ insertEntity gameState
    returnJson moveEntity

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

getAllGames :: DB [Entity GameStateEntity]
getAllGames = selectList [] []