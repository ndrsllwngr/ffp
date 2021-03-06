module Util.StateUtil
  ( removeGameById,
    getAllGames,
    getGameById,
    setGameStateForGameId,
  )
where

import Control.Lens
import Data.Map as Map
import Game.Game
import Import.NoFoundation

-- Removes a game with a given ID from the in-memory state
removeGameById :: TVar (Map String GameState) -> String -> IO (Map String GameState)
removeGameById tGames gameId_ = atomically $ do
  games <- readTVar tGames
  let filteredMap = Map.filter (\g -> (g ^. gameId) /= gameId_) games
  swapTVar tGames filteredMap

-- Returns a list of all ongoing games contained by the in-memory state
getAllGames :: TVar (Map String GameState) -> IO [GameState]
getAllGames tGames = do
  games <- readTVarIO tGames
  return $ Map.elems games

-- Returns an Optional of a GameState for a given gameID
getGameById :: TVar (Map String GameState) -> String -> IO (Maybe GameState)
getGameById tGames gameId_ = do
  games <- readTVarIO tGames
  return $ Map.lookup gameId_ games

-- Sets the GameState for a given gameID
setGameStateForGameId :: TVar (Map String GameState) -> String -> GameState -> IO (Map String GameState)
setGameStateForGameId tGames gameId_ game = atomically $ do
  games <- readTVar tGames
  let newMap = Map.insert gameId_ game games
  swapTVar tGames newMap
