{-# LANGUAGE BlockArguments #-}

module Game.Game (newGame,
                  makeMove,
                  GameState,
                  gameStateToGameStateEntity,
                  gameStateEntityToGameState,
                  moveEntityToMove,
                  Move(..)
                  ) where

import           ClassyPrelude.Conduit (UTCTime)
import           Data.Matrix
import           Game.Board
import           Model

data Move = Reveal Coordinate UTCTime | RevealAllNonFlagged UTCTime | Flag Coordinate UTCTime deriving (Show, Eq, Read) -- TODO maybe add unflag
data GameStatus = Ongoing | Won | Lost deriving (Show, Eq, Read)
--derivePersistField "Status"

data GameState = GameState { board     :: Board,
                             moves     :: [Move],
                             bombCount :: Int,
                             seed      :: Int,
                             status    :: GameStatus}

instance Show GameState where
   show (GameState b m bombs s st) = "Bombcount: " ++ show bombs ++ " Seed: " ++ show s ++ " Status: " ++ show st ++ "\n" ++ show m ++ "\n" ++ show b
--instance ToResponse GameState where
--   show (GameState board moves bombs seed status) = "Bombcount: " ++ show bombs ++ " Seed: " ++ show seed ++ " Status: " ++ show status ++ "\n" ++ show moves ++ "\n"++ show board


--type GameStateEntity = [[Cell]] [Move] Int Int GameStatus

gameStateToGameStateEntity :: GameState -> [Char] -> UTCTime -> UTCTime -> GameStateEntity
gameStateToGameStateEntity state gameId createdAt updatedAt = GameStateEntity {
                                                                gameStateEntityBoard = matrixToRows $ board state,
                                                                gameStateEntityMoves = map moveToMoveEntity $ moves state,
                                                                gameStateEntityBombCount = bombCount state,
                                                                gameStateEntitySeed = seed state,
                                                                gameStateEntityGameId = gameId,
                                                                gameStateEntityCreatedAt = createdAt,
                                                                gameStateEntityUpdatedAt = updatedAt,
                                                                gameStateEntityStatus = show (status state)
                                                              } where matrixToRows b = map (Row . map cellToCellEntity) (Data.Matrix.toLists b)

gameStateEntityToGameState :: GameStateEntity -> GameState
gameStateEntityToGameState entity = GameState {
                                      board = rowsToMatrix $ gameStateEntityBoard entity,
                                      moves = map moveEntityToMove $ gameStateEntityMoves entity,
                                      bombCount = gameStateEntityBombCount entity,
                                      seed = gameStateEntitySeed entity,
                                      status = statusEntityToStatus $ gameStateEntityStatus entity
                                    } where rowsToMatrix rows = Data.Matrix.fromLists $ map (map cellEntityToCell . rowCells) rows

statusEntityToStatus :: [Char] -> GameStatus
statusEntityToStatus "Ongoing" = Ongoing
statusEntityToStatus "Won"     = Won
statusEntityToStatus _         = Lost

cellToCellEntity :: Cell -> CellEntity
cellToCellEntity (Cell flagged revealed bomb neighbors (x,y)) = CellEntity {
                                                                      cellEntityCoordX = x,
                                                                      cellEntityCoordY = y,
                                                                      cellEntityIsFlagged = flagged,
                                                                      cellEntityIsRevealed = revealed,
                                                                      cellEntityHasBomb = bomb,
                                                                      cellEntityNeighboringBombs = neighbors
                                                                    }

cellEntityToCell :: CellEntity -> Cell
cellEntityToCell cellEntity = Cell {
                                isFlagged         = cellEntityIsFlagged cellEntity,
                                isRevealed        = cellEntityIsRevealed cellEntity,
                                hasBomb           = cellEntityHasBomb cellEntity,
                                neighboringBombs  = cellEntityNeighboringBombs cellEntity,
                                coordinate        = (cellEntityCoordX cellEntity, cellEntityCoordY cellEntity)
                              }

moveToMoveEntity :: Move -> MoveEntity
moveToMoveEntity (Flag (x,y) timeStamp)           = MoveEntity "Flag" (Just x) (Just y) timeStamp
moveToMoveEntity (Reveal (x,y) timeStamp)         = MoveEntity "Reveal" (Just x) (Just y) timeStamp
moveToMoveEntity (RevealAllNonFlagged timeStamp)  = MoveEntity "RevealAllNonFlagged" Nothing Nothing timeStamp

moveEntityToMove :: MoveEntity -> Move
moveEntityToMove (MoveEntity "Flag" (Just x) (Just y) timeStamp)    = Flag (x,y) timeStamp
moveEntityToMove (MoveEntity "Reveal" (Just x) (Just y) timeStamp)  = Reveal (x,y) timeStamp
moveEntityToMove (MoveEntity "RevealAllNonFlagged" _ _ timeStamp)   = RevealAllNonFlagged timeStamp
moveEntityToMove _   = undefined -- todo maybe errorhande?

-- Creates a new game for a given Dimension, bombCount & seed
newGame :: Dimension -> Int -> Int -> GameState
newGame (h,w) b s = GameState { board = generateBoard (h,w) b s,
                                moves = [],
                                bombCount = b,
                                seed = s,
                                status = Ongoing
                               }

-- Executes a move on a given GameState
-- TODO handle illegal moves e.g. outOfBounds Action, action on a board with status Won || Lost
makeMove :: GameState -> Move -> GameState
makeMove state m  = state { board   = boardAfterMove,
                            moves   = moves state ++ [m],
                            status  = checkStatus boardAfterMove
                          } where boardAfterMove = case m of (Flag c _) -> flagCell (board state) c
                                                             (Reveal c _) -> revealCell (board state) c
                                                             (RevealAllNonFlagged _) -> revealAllNonFlaggedCells (board state)


checkStatus :: Board -> GameStatus
checkStatus b = case (checkWon b, checkLost b) of  (_,True)      -> Lost
                                                   (True,False)  -> Won
                                                   (False,False) -> Ongoing
