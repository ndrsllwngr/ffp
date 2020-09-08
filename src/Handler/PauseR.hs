{-# LANGUAGE BlockArguments        #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NoImplicitPrelude     #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE TypeFamilies          #-}
module Handler.PauseR where



import           Game.Game
import           Handler.GameR
import           Import
import           Text.Julius           (RawJS (..))
import           Yesod.Form.Bootstrap3 (BootstrapFormLayout (..),
                                        renderBootstrap3)
import Data.Maybe
import Data.Time.Clock (diffUTCTime)
import Data.Fixed

-- Define our data that will be used for creating the form.
data FileForm = FileForm
    { fileInfo        :: FileInfo
    , fileDescription :: Text
    }

-- GET GAME VIEW
postPauseR :: Text -> Handler Value
postPauseR gameIdText = do
    let gameId = unpack gameIdText
    now <- liftIO getCurrentTime
    gameStateDBEntities <- runDB $ selectList [GameStateEntityGameId ==. gameId] [Desc GameStateEntityUpdatedAt, LimitTo 1]
    let gameStateEntity = getGameStateEntity gameStateDBEntities
    let gameStateEntityKey = getGameStateEntityKey gameStateDBEntities
    let updatedGameStateEntity = gameStateEntity {
                                          gameStateEntityStatus = "Paused",
                                          gameStateEntityUpdatedAt = now,
                                          gameStateEntityTimeElapsed = calculateTimeElapsed (gameStateEntityLastStartedAt gameStateEntity) (gameStateEntityTimeElapsed gameStateEntity) now
                                          }
    insertedGameStateEntity <- runDB $ repsert gameStateEntityKey updatedGameStateEntity
    returnJson insertedGameStateEntity                                    

