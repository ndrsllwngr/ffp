{-# LANGUAGE BlockArguments        #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NoImplicitPrelude     #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE TypeFamilies          #-}
module Handler.GameR where

import           Game.Game
import           Game.StateUtil
import           Game.Util
import           Import
import           Marshalling
import           Control.Lens


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
                  let (gameTableId, cellId) = gameIds
                  aDomId <- newIdent
                  setTitle "Game"
                  $(widgetFile "game")

      -- If no game was found in the in-memory state check database if a Paused/Won/Lost game with the given ID is present
      Nothing -> do
          gameStateDBEntities <- runDB $ selectList [GameStateEntityGameId ==. gameId_] [Desc GameStateEntityUpdatedAt, LimitTo 1]
          case getGameStateEntityMaybe gameStateDBEntities of 
              Just gsEntity -> do
                  let status_ = gsEntity ^. gameStateEntityStatus
                  -- If the game was Paused before, move it from the database back into the in memory storage and set the state to Ongoing
                  gameStateEntity <- if status_ == "Paused" then do let updateEntity = gsEntity & gameStateEntityStatus .~ "Ongoing"
                                                                                                & gameStateEntityLastStartedAt .~ now
                                                                    channel <- newChan
                                                                    let gameState = gameStateEntityToGameState updateEntity channel
                                                                    -- Load game back into in-memory state
                                                                    _ <- liftIO $ setGameStateForGameId tGames gameId_ gameState

                                                                    -- Remove it from the database
                                                                    runDB $ deleteBy $ UniqueGameStateEntity gameId_
                                                                    return updateEntity
                                                            -- In any other case (game was Won or Lost) just return the fetched entity
                                                            else do return gsEntity
                  defaultLayout $ do
                          let (gameTableId, cellId) = gameIds
                          aDomId <- newIdent
                          setTitle "Game"
                          $(widgetFile "game")
              -- If game was neither in Memory (Ongoing) nor in Database (Paused/Won/Lost) return 404
              Nothing -> notFound                                               

-- MAKE MOVE
putGameR :: Text -> Handler Html
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
          -- Perform move
          let gameStateAfterMove = makeMove gameState $ moveRequestToMove moveRequest now
          -- Check the new status of the game after the move has been Executed
          _ <- case gameStateAfterMove ^. status of
                                              -- if game is ongoing update it in Memory
                                              Ongoing -> do liftIO $ setGameStateForGameId tGames gameId_ gameStateAfterMove
                                              -- if game is Won/Lost delete from Memory & store in db
                                              -- Paused is not a possible state after a move but would still have the same logic
                                              _       -> do _ <- runDB $ insert $ gameStateToGameStateEntity gameStateAfterMove
                                                            liftIO $ removeGameById tGames gameId_

          let gameStateEntity = gameStateToGameStateEntity gameStateAfterMove
          defaultLayout $ do
                  let (gameTableId, cellId) = gameIds
                  aDomId <- newIdent
                  setTitle "Game"
                  $(widgetFile "game")
      -- If the game was not the in-memory state return 404 since no game which moves can be executed on was found
      Nothing -> notFound


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
getCellTile _ _ _ _            = error "Invalid CellState"

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
getCellTileWon _ _ _ _            = "/static/assets/closed.svg"

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

getRemainingFlags :: [Row] -> Int -> Int
getRemainingFlags rows bombCount_ = bombCount_ - sum (concatMap mapCells rows)
                                    where mapCells row = map cellToInt $ row ^. rowCells
                                                        where cellToInt cell = fromEnum $ cell ^. cellEntityIsFlagged 


getTimeElapsed :: UTCTime -> Int -> UTCTime -> String -> Int
getTimeElapsed lastStartedAt_ timeElapsed_ now status_ = case status_ of
                                                          "Won"   -> timeElapsed_
                                                          "Lost"  -> timeElapsed_
                                                          _       -> calculateTimeElapsed lastStartedAt_ timeElapsed_ now
