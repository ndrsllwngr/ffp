module Game.Sessions (Sessions,
                      addNewGame, 
                      makeMoveOnGame) where

import qualified Data.Map as Map
import Game.Util
import Game.Game
import Game.Board
import Control.Monad.State  
import ClassyPrelude.Conduit (getCurrentTime)
import Data.Time.Clock (UTCTime)

type Sessions = Map.Map String GameState

addNewGame :: String -> Dimension -> Int -> Int -> UTCTime -> State Sessions GameState
addNewGame gameID (h,w) b s now = do 
                                currentState <- get
                                let game = newGame (h,w) b s gameID now
                                put (Map.insert gameID game currentState)
                                return game

makeMoveOnGame :: String -> Move -> UTCTime -> State Sessions GameState
makeMoveOnGame gameID move now = do 
                                currentState <- get
                                case Map.lookup gameID currentState of
                                  Just game -> do 
                                                let gameAfterMove = makeMove game move
                                                let elapsed = case status gameAfterMove of  Won   -> finishGame
                                                                                            Lost  -> finishGame
                                                                                            _     -> timeElapsed gameAfterMove
                                                                                            where finishGame = calculateTimeElapsed (lastStartedAt gameAfterMove) (timeElapsed gameAfterMove) now
                                                let returnGame = gameAfterMove {timeElapsed = elapsed}
                                                put (Map.insert gameID returnGame currentState)
                                                return returnGame
                                  Nothing -> do --todo get game from DB
                                        error "Game not found"
                   