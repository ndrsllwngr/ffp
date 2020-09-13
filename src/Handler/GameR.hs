{-# LANGUAGE BlockArguments        #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NoImplicitPrelude     #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE TypeFamilies          #-}
module Handler.GameR where

import           Game.Game
import           Util.StateUtil
import           Util.HandlerUtil
import           Import
import           Marshalling
import           Control.Lens
import           Handler.ChannelR(broadcast)


-- GET GAME VIEW
getGameR :: Text -> Handler Html
getGameR gameIdText = do
    app <- getYesod
    now <- liftIO getCurrentTime
    -- Get the in-memory state of ongoing games
    let tGames = games app
    let gameId_ = unpack gameIdText

    -- Try to get the game from the in-memory state
    gameStateMaybe <- liftIO $ getGameById tGames gameId_
    
    case gameStateMaybe of
      -- If a game was found in the in-memory state return it
      Just gameState -> do
          let gameStateEntity = gameStateToGameStateEntity gameState
          defaultLayout $ do
                  let gameTableId = gameIds
                  setTitle $ toHtml $ gameId_ ++ " | Minesweepskell"
                  $(widgetFile "game")

      -- If no game was found in the in-memory state check database if a Paused/Won/Lost game with the given ID is present
      Nothing -> do
          gameStateDBEntities <- runDB $ selectList [GameStateEntityGameId ==. gameId_] [Desc GameStateEntityUpdatedAt, LimitTo 1]
          case getGameStateEntityMaybe gameStateDBEntities of 
              Just gsEntity -> do
                  let status_ = gsEntity ^. gameStateEntityStatus
                  -- If the game was Paused before, move it from the database back into the in memory storage and set the state to Ongoing
                  gameStateEntity <- if status_ == "Paused" then do let updateEntity = gsEntity & gameStateEntityStatus .~ "Ongoing"
                                                                                                & gameStateEntityLastStartedAt .~ (if null (gsEntity ^. gameStateEntityMoves) then Nothing else Just now)
                                                                    channel_ <- newChan
                                                                    let gameState = gameStateEntityToGameState updateEntity channel_
                                                                    -- Load game back into in-memory state
                                                                    _ <- liftIO $ setGameStateForGameId tGames gameId_ gameState

                                                                    -- Remove it from the database
                                                                    runDB $ deleteBy $ UniqueGameStateEntity gameId_
                                                                    return updateEntity
                                                            -- In any other case (game was Won or Lost) just return the fetched entity
                                                            else do return gsEntity
                  defaultLayout $ do
                          let gameTableId = gameIds
                          setTitle $ toHtml $ gameId_ ++ " | Minesweepskell"
                          $(widgetFile "game")
              -- If game was neither in Memory (Ongoing) nor in Database (Paused/Won/Lost) return 404
              Nothing -> notFound                                               

-- MAKE MOVE
putGameR :: Text -> Handler Value
putGameR gameIdText = do
    app <- getYesod
    -- Get the in-memory state of ongoing games
    let tGames = games app
    let gameId_ = unpack gameIdText

    -- Try to get the game from the in-memory state
    gameStateMaybe <- liftIO $ getGameById tGames gameId_
    
    case gameStateMaybe of
      -- If a game was found in the in-memory state perform the requested move on it
      Just gameState -> do 
          moveRequest <- (requireCheckJsonBody :: Handler MoveRequest)
          now <- liftIO getCurrentTime
          let move = moveRequestToMove moveRequest now
          -- Check if move was illegal move
          unless (isMoveInBounds move gameState) $ error "Move out of bounds"
          -- Set lastStartedAt if it was the first move
          let updatedGameState = if null (gameState ^. moves) then gameState & (lastStartedAt ?~ now) else gameState
          -- Perform move
          let gameStateAfterMove = makeMove updatedGameState move
          -- Check the new status of the game after the move has been Executed
          _ <- case gameStateAfterMove ^. status of
                                              -- if game is ongoing update it in Memory
                                              Ongoing -> do liftIO $ setGameStateForGameId tGames gameId_ gameStateAfterMove
                                              -- if game is Won/Lost delete from Memory & store in db
                                              -- Paused is not a possible state after a move but would still have the same logic
                                              _       -> do _ <- runDB $ insert $ gameStateToGameStateEntity gameStateAfterMove
                                                            liftIO $ removeGameById tGames gameId_
          let gameStateEntity = gameStateToGameStateEntity gameStateAfterMove
          broadcast (gameStateAfterMove ^. channel) gameStateEntity
          returnJson gameStateEntity
      -- If the game was not the in-memory state return 404 since no game which moves can be executed on was found
      Nothing -> notFound


gameIds :: Text
gameIds = "js-gameTableId"
