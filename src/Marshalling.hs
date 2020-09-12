{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns        #-}
{-# LANGUAGE NoImplicitPrelude     #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE BlockArguments #-}

module Marshalling (
                    gameStateToGameStateEntity,
                    gameStateEntityToGameState,
                    statusEntityToStatus,
                    cellToCellEntity,
                    cellEntityToCell,
                    moveToMoveEntity,
                    moveRequestToMove
                    ) where

import           Data.Matrix
import           Import 
import           Game.Board
import           Game.Game
import           Control.Lens

gameStateToGameStateEntity :: GameState -> GameStateEntity
gameStateToGameStateEntity state = GameStateEntity {
                                       _gameStateEntityBoard         = boardToRows $ state ^. board,
                                       _gameStateEntityMoves         = map moveToMoveEntity $ state ^. moves,
                                       _gameStateEntityBombCount     = state ^. bombCount,
                                       _gameStateEntitySeed          = state ^. seed,
                                       _gameStateEntityGameId        = state ^. gameId,
                                       _gameStateEntityCreatedAt     = state ^. createdAt,
                                       _gameStateEntityUpdatedAt     = state ^. updatedAt,
                                       _gameStateEntityStatus        = show (state ^. status),
                                       _gameStateEntityLastStartedAt = state ^. lastStartedAt,
                                       _gameStateEntityTimeElapsed   = state ^. timeElapsed
                                   } where boardToRows board_ = map (Row . map cellToCellEntity) (Data.Matrix.toLists board_)

gameStateEntityToGameState :: GameStateEntity -> IO GameState
gameStateEntityToGameState entity = do
                                       channel_ <- newChan
                                       return GameState {
                                            _board         = rowsToBoard $ entity ^. gameStateEntityBoard,
                                            _moves         = map moveEntityToMove $ entity ^. gameStateEntityMoves,
                                            _bombCount     = entity ^. gameStateEntityBombCount,
                                            _seed          = entity ^. gameStateEntitySeed,
                                            _status        = statusEntityToStatus $ entity ^. gameStateEntityStatus,
                                            _gameId        = entity ^. gameStateEntityGameId,
                                            _createdAt     = entity ^. gameStateEntityCreatedAt,
                                            _updatedAt     = entity ^. gameStateEntityUpdatedAt,
                                            _lastStartedAt = entity ^. gameStateEntityLastStartedAt,
                                            _timeElapsed   = entity ^. gameStateEntityTimeElapsed,
                                            _channel       = channel_
                                        } where rowsToBoard rows = Data.Matrix.fromLists $ map (map cellEntityToCell . _rowCells) rows --TODO how to use lense here?

statusEntityToStatus :: [Char] -> GameStatus
statusEntityToStatus "Ongoing" = Ongoing
statusEntityToStatus "Won"     = Won
statusEntityToStatus "Lost"    = Lost
statusEntityToStatus "Paused"  = Paused
statusEntityToStatus _         = error "Invalid StatusEntity"

cellToCellEntity :: Cell -> CellEntity
cellToCellEntity (Cell flagged revealed hasBomb neighbors (x,y)) = CellEntity {
                                                                     _cellEntityCoordX = x,
                                                                     _cellEntityCoordY = y,
                                                                     _cellEntityIsFlagged = flagged,
                                                                     _cellEntityIsRevealed = revealed,
                                                                     _cellEntityHasBomb = hasBomb,
                                                                     _cellEntityNeighboringBombs = neighbors
                                                                   }

cellEntityToCell :: CellEntity -> Cell
cellEntityToCell cellEntity = Cell {
                                _isFlagged         = cellEntity ^. cellEntityIsFlagged ,
                                _isRevealed        = cellEntity ^. cellEntityIsRevealed,
                                _hasBomb           = cellEntity ^. cellEntityHasBomb ,
                                neighboringBombs  = cellEntity ^. cellEntityNeighboringBombs ,
                                coordinate        = (cellEntity ^. cellEntityCoordX, cellEntity ^. cellEntityCoordY)
                              }

moveToMoveEntity :: Move -> MoveEntity
moveToMoveEntity (Flag (x,y) timeStamp)           = MoveEntity "Flag" (Just x) (Just y) timeStamp
moveToMoveEntity (Reveal (x,y) timeStamp)         = MoveEntity "Reveal" (Just x) (Just y) timeStamp
moveToMoveEntity (RevealAllNonFlagged timeStamp)  = MoveEntity "RevealAllNonFlagged" Nothing Nothing timeStamp

moveEntityToMove :: MoveEntity -> Move
moveEntityToMove (MoveEntity "Flag" (Just x) (Just y) timeStamp)    = Flag (x,y) timeStamp
moveEntityToMove (MoveEntity "Reveal" (Just x) (Just y) timeStamp)  = Reveal (x,y) timeStamp
moveEntityToMove (MoveEntity "RevealAllNonFlagged" _ _ timeStamp)   = RevealAllNonFlagged timeStamp
moveEntityToMove _ = error "Invalid MoveEntity"

moveRequestToMove :: MoveRequest -> UTCTime -> Move
moveRequestToMove (MoveRequest "RevealAllNonFlagged" _ _) timeStamp = RevealAllNonFlagged timeStamp
moveRequestToMove (MoveRequest "Flag" (Just x) (Just y)) timeStamp  = Flag (x,y) timeStamp
moveRequestToMove (MoveRequest "Reveal"(Just x) (Just y)) timeStamp = Reveal (x,y) timeStamp
moveRequestToMove _ _ = error "Invalid MoveRequest"
