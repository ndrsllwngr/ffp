{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns        #-}
{-# LANGUAGE NoImplicitPrelude     #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE TemplateHaskell #-}

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
                                   } where boardToRows board = map (Row . map cellToCellEntity) (Data.Matrix.toLists board)

gameStateEntityToGameState :: GameStateEntity -> GameState
gameStateEntityToGameState entity = GameState {
                                       _gameStateBoard         = rowsToBoard $ entity ^. gameStateEntityBoard,
                                       _gameStateMoves         = map moveEntityToMove $ entity ^. gameStateEntityMoves,
                                       _gameStateBombCount     = entity ^. gameStateEntityBombCount,
                                       _gameStateSeed          = entity ^. gameStateEntitySeed,
                                       _gameStateStatus        = statusEntityToStatus $ entity ^. gameStateEntityStatus,
                                       _gameStateGameId        = entity ^. gameStateEntityGameId,
                                       _gameStateCreatedAt     = entity ^. gameStateEntityCreatedAt,
                                       _gameStateUpdatedAt     = entity ^. gameStateEntityUpdatedAt,
                                       _gameStateLastStartedAt = entity ^. gameStateEntityLastStartedAt,
                                       _gameStateTimeElapsed   = entity ^. gameStateEntityTimeElapsed
                                    } where rowsToBoard rows = Data.Matrix.fromLists $ map (map cellEntityToCell . _rowCells) rows --TODO how to use lense here?

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
                                _cellIsFlagged         = cellEntity ^. cellEntityIsFlagged ,
                                _cellIsRevealed        = cellEntity ^. cellEntityIsRevealed,
                                _cellHasBomb           = cellEntity ^. cellEntityHasBomb ,
                                _cellNeighboringBombs  = cellEntity ^. cellEntityNeighboringBombs ,
                                _cellCoordinate        = (cellEntity ^. cellEntityCoordX, cellEntity ^. cellEntityCoordY)
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