{-# LANGUAGE BlockArguments        #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NoImplicitPrelude     #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE TypeFamilies          #-}
module Handler.Game where

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

-- GET LATEST GAME
getGameR :: Text -> Handler Html
getGameR gameId = do
    -- print $ unpack gameId
    gameStateDBEntity <- runDB $ selectList [GameStateEntityGameId ==. unpack gameId] [Desc GameStateEntityUpdatedAt, LimitTo 1]
    -- print gameStateDBEntity
    let maybeGameStateEntity = getGameStateEntity gameStateDBEntity
    let gameStateEntity = case maybeGameStateEntity of
                  Just entity -> entity -- ERROR maybe here?
                  Nothing -> error "HELP ME!"
    defaultLayout $ do
            let (gameTableId, cellId) = gameIds
            setTitle "Game"
            $(widgetFile "game")

-- MAKE MOVE
putGameR :: Text -> Handler Value
putGameR gameId = do
    timeStamp <- liftIO getCurrentTime
    -- requireCheckJsonBody will parse the request body into the appropriate type, or return a 400 status code if the request JSON is invalid.
    -- (The ToJSON and FromJSON instances are derived in the config/models file).
    moveRequest <- (requireCheckJsonBody :: Handler MoveRequest)
    -- print moveEntity
    state <- runDB $ selectList [GameStateEntityGameId ==. unpack gameId] [Desc GameStateEntityUpdatedAt, LimitTo 1]
    -- print state
    let gameStateEntity = getGameStateEntity state
    let gameState = case gameStateEntity of
              Just entity -> makeMove (gameStateEntityToGameState entity) $ moveEntityToMove MoveEntity {moveEntityAction=moveRequestAction moveRequest, moveEntityCoordX=moveRequestCoordX moveRequest, moveEntityCoordY=moveRequestCoordY moveRequest,moveEntityTimeStamp=timeStamp} -- ERROR maybe here?
              Nothing -> error "HELP ME!"
    let createdAt = case gameStateEntity of
              Just entity -> gameStateEntityCreatedAt entity
              Nothing     -> error "HELP ME!"
    -- print gameState
    let gameStateEntityKey = getGameStateEntityKey state
    let gameStateKey = case gameStateEntityKey of 
              Just key -> key
              Nothing -> error "HELP ME"
    let updatedGameStateEntity = gameStateToGameStateEntity gameState (unpack gameId) createdAt timeStamp
    insertedGameState <- runDB $ repsert gameStateKey updatedGameStateEntity
    -- print $ gameState
    -- -- The YesodAuth instance in Foundation.hs defines the UserId to be the type used for authentication.
    -- maybeCurrentUserId <- maybeAuthId
    -- --let newGameEntity' = newGameEntity { newGameUserId = maybeCurrentUserId }
    -- insertedGameState <- runDB $ insertEntity gameState
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

getGameStateEntity :: [Entity GameStateEntity] -> Maybe GameStateEntity
getGameStateEntity (x:_) = Just $ entityVal x
getGameStateEntity _     = Nothing

getGameStateEntityKey :: [Entity GameStateEntity] -> Maybe (Key GameStateEntity)
getGameStateEntityKey (x:_) = Just $ entityKey x
getGameStateEntityKey _     = Nothing

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