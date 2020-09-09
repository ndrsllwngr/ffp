{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns        #-}
{-# LANGUAGE NoImplicitPrelude     #-}
{-# LANGUAGE TypeFamilies          #-}
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

gameStateToGameStateEntity :: GameState -> GameStateEntity
gameStateToGameStateEntity state = GameStateEntity {
                                       _gameStateEntityBoard = boardToRows $ board state,
                                       _gameStateEntityMoves = map moveToMoveEntity $ moves state,
                                       _gameStateEntityBombCount = bombCount state,
                                       _gameStateEntitySeed = seed state,
                                       _gameStateEntityGameId = gameId state,
                                       _gameStateEntityCreatedAt = createdAt state,
                                       _gameStateEntityUpdatedAt = updatedAt state,
                                       _gameStateEntityStatus = show (status state),
                                       _gameStateEntityLastStartedAt = lastStartedAt state,
                                       _gameStateEntityTimeElapsed = timeElapsed state
                                   } where boardToRows board = map (Row . map cellToCellEntity) (Data.Matrix.toLists board)

gameStateEntityToGameState :: GameStateEntity -> GameState
gameStateEntityToGameState entity = GameState {
                                      board = rowsToBoard $ _gameStateEntityBoard entity,
                                      moves = map moveEntityToMove $ _gameStateEntityMoves entity,
                                      bombCount = _gameStateEntityBombCount entity,
                                      seed = _gameStateEntitySeed entity,
                                      status = statusEntityToStatus $ _gameStateEntityStatus entity,
                                      gameId = _gameStateEntityGameId entity,
                                      createdAt = _gameStateEntityCreatedAt entity,
                                      updatedAt = _gameStateEntityUpdatedAt entity,
                                      lastStartedAt = _gameStateEntityLastStartedAt entity,
                                      timeElapsed = _gameStateEntityTimeElapsed entity
                                    } where rowsToBoard rows = Data.Matrix.fromLists $ map (map cellEntityToCell . _rowCells) rows

statusEntityToStatus :: [Char] -> GameStatus
statusEntityToStatus "Ongoing" = Ongoing
statusEntityToStatus "Won"     = Won
statusEntityToStatus "Lost"    = Lost
statusEntityToStatus "Paused"  = Paused
statusEntityToStatus _         = undefined

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
                                isFlagged         = _cellEntityIsFlagged cellEntity,
                                isRevealed        = _cellEntityIsRevealed cellEntity,
                                hasBomb           = _cellEntityHasBomb cellEntity,
                                neighboringBombs  = _cellEntityNeighboringBombs cellEntity,
                                coordinate        = (_cellEntityCoordX cellEntity, _cellEntityCoordY cellEntity)
                              }

moveToMoveEntity :: Move -> MoveEntity
moveToMoveEntity (Flag (x,y) timeStamp)           = MoveEntity "Flag" (Just x) (Just y) timeStamp
moveToMoveEntity (Reveal (x,y) timeStamp)         = MoveEntity "Reveal" (Just x) (Just y) timeStamp
moveToMoveEntity (RevealAllNonFlagged timeStamp)  = MoveEntity "RevealAllNonFlagged" Nothing Nothing timeStamp

moveEntityToMove :: MoveEntity -> Move
moveEntityToMove (MoveEntity "Flag" (Just x) (Just y) timeStamp)    = Flag (x,y) timeStamp
moveEntityToMove (MoveEntity "Reveal" (Just x) (Just y) timeStamp)  = Reveal (x,y) timeStamp
moveEntityToMove (MoveEntity "RevealAllNonFlagged" _ _ timeStamp)   = RevealAllNonFlagged timeStamp
moveEntityToMove _ = undefined

moveRequestToMove :: MoveRequest -> UTCTime -> Move
moveRequestToMove (MoveRequest "RevealAllNonFlagged" _ _) timeStamp = RevealAllNonFlagged timeStamp
moveRequestToMove (MoveRequest "Flag" (Just x) (Just y)) timeStamp  = Flag (x,y) timeStamp
moveRequestToMove (MoveRequest "Reveal"(Just x) (Just y)) timeStamp = Reveal (x,y) timeStamp
moveRequestToMove _ _ = undefined