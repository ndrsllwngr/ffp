{-# LANGUAGE BlockArguments        #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NoImplicitPrelude     #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE TypeFamilies          #-}
module Handler.GameR where

import           Game.Game
import           Import
import           Text.Julius           (RawJS (..))
import           Yesod.Form.Bootstrap3 (BootstrapFormLayout (..),
                                        renderBootstrap3)

-- Define our data that will be used for creating the form.
data FileForm = FileForm { 
                  fileInfo        :: FileInfo, 
                  fileDescription :: Text
                }

-- GET GAME VIEW
getGameR :: Text -> Handler Html
getGameR gameIdText = do
    let gameId = unpack gameIdText
    gameStateDBEntities <- runDB $ selectList [GameStateEntityGameId ==. gameId] [Desc GameStateEntityUpdatedAt, LimitTo 1]
    let gameStateEntity = getGameStateEntity gameStateDBEntities
    defaultLayout $ do
            let (gameTableId, cellId) = gameIds
            setTitle "Game"
            $(widgetFile "game")

-- MAKE MOVE
putGameR :: Text -> Handler Value
putGameR gameIdText = do
    -- requireCheckJsonBody will parse the request body into the appropriate type, or return a 400 status code if the request JSON is invalid.
    -- (The ToJSON and FromJSON instances are derived in the config/models file).
    moveRequest <- (requireCheckJsonBody :: Handler MoveRequest)
    let gameId = unpack gameIdText
    now <- liftIO getCurrentTime

    gameStateDBEntities <- runDB $ selectList [GameStateEntityGameId ==. gameId] [Desc GameStateEntityUpdatedAt, LimitTo 1]
    let gameStateEntity = getGameStateEntity gameStateDBEntities
    let newGameState = makeMove (gameStateEntityToGameState gameStateEntity) $ moveEntityToMove 
                          MoveEntity {moveEntityAction    = moveRequestAction moveRequest, 
                                      moveEntityCoordX    = moveRequestCoordX moveRequest, 
                                      moveEntityCoordY    = moveRequestCoordY moveRequest,
                                      moveEntityTimeStamp = now}-- TODO ERROR maybe here?

    let createdAt = gameStateEntityCreatedAt gameStateEntity
    let gameStateEntityKey = getGameStateEntityKey gameStateDBEntities

    let updatedGameStateEntity = gameStateToGameStateEntity newGameState gameId createdAt now
    insertedGameStateEntity <- runDB $ repsert gameStateEntityKey updatedGameStateEntity
    returnJson insertedGameStateEntity


getGameStateEntity :: [Entity GameStateEntity] -> GameStateEntity
getGameStateEntity (x:_) = entityVal x
getGameStateEntity _     = error "HELP ME!"

getGameStateEntityKey :: [Entity GameStateEntity] -> Key GameStateEntity
getGameStateEntityKey (x:_) = entityKey x
getGameStateEntityKey _     = error "HELP ME!"

gameIds :: (Text, Text)
gameIds = ("js-gameTableId", "js-cellId")

-- isFlagged Bool
-- isRevealed Bool
-- hasBomb Bool
-- neighboringBombs Int
getCellTile :: Bool -> Bool -> Bool -> Int -> String
getCellTile False True False 0 = "/static/assets/type0.svg"
getCellTile False True False 1 = "/static/assets/type1.svg"
getCellTile False True False 2 = "/static/assets/type2.svg"
getCellTile False True False 3 = "/static/assets/type3.svg"
getCellTile False True False 4 = "/static/assets/type4.svg"
getCellTile False True False 5 = "/static/assets/type5.svg"
getCellTile False True False 6 = "/static/assets/type6.svg"
getCellTile False True False 7 = "/static/assets/type7.svg"
getCellTile False True False 8 = "/static/assets/type8.svg"
getCellTile True True True _   = "/static/assets/mine.svg"
getCellTile True True False _  = "/static/assets/mine_wrong.svg"
getCellTile False True True _  = "/static/assets/mine_red.svg"
getCellTile True False _ _     = "/static/assets/flag.svg"
getCellTile _ False _ _        = "/static/assets/closed.svg"

getCellTileWon :: Bool -> Bool -> Bool -> Int -> String
getCellTileWon False _ False 0    = "/static/assets/type0.svg"
getCellTileWon False _ False 1    = "/static/assets/type1.svg"
getCellTileWon False _ False 2    = "/static/assets/type2.svg"
getCellTileWon False _ False 3    = "/static/assets/type3.svg"
getCellTileWon False _ False 4    = "/static/assets/type4.svg"
getCellTileWon False _ False 5    = "/static/assets/type5.svg"
getCellTileWon False _ False 6    = "/static/assets/type6.svg"
getCellTileWon False _ False 7    = "/static/assets/type7.svg"
getCellTileWon False _ False 8    = "/static/assets/type8.svg"
getCellTileWon True _ True _      = "/static/assets/flag.svg"
getCellTileWon _ _ _ _           = "/static/assets/closed.svg"

getCellTileLost :: Bool -> Bool -> Bool -> Int -> String
getCellTileLost False True False 0 = "/static/assets/type0.svg"
getCellTileLost False True False 1 = "/static/assets/type1.svg"
getCellTileLost False True False 2 = "/static/assets/type2.svg"
getCellTileLost False True False 3 = "/static/assets/type3.svg"
getCellTileLost False True False 4 = "/static/assets/type4.svg"
getCellTileLost False True False 5 = "/static/assets/type5.svg"
getCellTileLost False True False 6 = "/static/assets/type6.svg"
getCellTileLost False True False 7 = "/static/assets/type7.svg"
getCellTileLost False True False 8 = "/static/assets/type8.svg"
getCellTileLost True False False _ = "/static/assets/mine_wrong.svg"
getCellTileLost False True True _  = "/static/assets/mine_red.svg"
getCellTileLost _ False True _     = "/static/assets/mine.svg"
getCellTileLost _ _ _ _            = "/static/assets/closed.svg"