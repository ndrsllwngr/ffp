{-# LANGUAGE BlockArguments #-}

module Game.Game (newGame,
                  makeMove,
                  GameState,
                  gameStateToGameStateEntity,
                  gameStateEntityToGameState,
                  moveEntityToMove,
                  removeRow,
                  Move(Reveal, Flag)) where

import           ClassyPrelude.Conduit (UTCTime)
import           Data.Matrix
import           Game.Board
import           Model

data Move = Reveal Coordinate UTCTime | Flag Coordinate UTCTime deriving (Show, Eq, Read) -- TODO maybe add unflag
data GameStatus = Ongoing | Won | Lost deriving (Show, Eq, Read)
--derivePersistField "Status"

data GameState = GameState { board     :: Board,
                             moves     :: [Move],
                             bombCount :: Int,
                             seed      :: Int,
                             status    :: GameStatus}

instance Show GameState where
   show (GameState board moves bombs seed status) = "Bombcount: " ++ show bombs ++ " Seed: " ++ show seed ++ " Status: " ++ show status ++ "\n" ++ show moves ++ "\n" ++ show board
--instance ToResponse GameState where
--   show (GameState board moves bombs seed status) = "Bombcount: " ++ show bombs ++ " Seed: " ++ show seed ++ " Status: " ++ show status ++ "\n" ++ show moves ++ "\n"++ show board


--type GameStateEntity = [[Cell]] [Move] Int Int GameStatus

gameStateToGameStateEntity :: GameState -> [Char] -> UTCTime -> UTCTime -> UTCTime -> Int -> GameStateEntity
gameStateToGameStateEntity state gameId createdAt updatedAt lastStartedAt timeElapsed = GameStateEntity {
                                gameStateEntityBoard = matrixToRows $ board state,
                                gameStateEntityMoves = map moveToMoveEntity (moves state),
                                gameStateEntityBombCount = bombCount state,
                                gameStateEntitySeed = seed state,
                                gameStateEntityStatus = show (status state),
                                gameStateEntityGameId = gameId,
                                gameStateEntityCreatedAt = createdAt,
                                gameStateEntityUpdatedAt = updatedAt,
                                gameStateEntityLastStartedAt = lastStartedAt,
                                gameStateEntityTimeElapsed = timeElapsed
}

gameStateEntityToGameState :: GameStateEntity -> GameState
gameStateEntityToGameState entity = GameState {
      board = Data.Matrix.fromLists $ map removeRow $ gameStateEntityBoard entity,
      moves = map moveEntityToMove $ gameStateEntityMoves entity,
      bombCount = gameStateEntityBombCount entity,
      seed = gameStateEntitySeed entity,
      status = statusEntityToStatus $ gameStateEntityStatus entity
}

statusEntityToStatus :: [Char] -> GameStatus
statusEntityToStatus "Ongoing" = Ongoing
statusEntityToStatus "Won"     = Won
statusEntityToStatus _         = Lost

matrixToRows :: Matrix Cell -> [Row]
matrixToRows matrix = map createRow $ Data.Matrix.toLists $ mapPos (\(r,c) cell -> cellToCellEntity cell (r,c) ) matrix

cellToCellEntity :: Cell -> (Int, Int) -> CellEntity
cellToCellEntity cell (x, y) = CellEntity {
    cellEntityCoordX = x,
    cellEntityCoordY = y,
    cellEntityIsFlagged = isFlagged cell,
    cellEntityIsRevealed = isRevealed cell,
    cellEntityHasBomb = hasBomb cell,
    cellEntityNeighboringBombs = neighboringBombs cell
}

cellEntityToCell :: CellEntity -> Cell
cellEntityToCell cellEntity = Cell {
    isFlagged = cellEntityIsFlagged cellEntity,
    isRevealed = cellEntityIsRevealed cellEntity,
    hasBomb = cellEntityHasBomb cellEntity,
    neighboringBombs = cellEntityNeighboringBombs cellEntity
}


createRow :: [CellEntity] -> Row
createRow cells = Row {
   rowCells = cells
}

removeRow :: Row -> [Cell]
removeRow row = map cellEntityToCell $ rowCells row

moveToMoveEntity :: Move -> MoveEntity
moveToMoveEntity (Flag (x,y) timeStamp) = MoveEntity {moveEntityAction = "Flag", moveEntityCoordX = x, moveEntityCoordY = y, moveEntityTimeStamp=timeStamp}
moveToMoveEntity (Reveal (x,y) timeStamp) = MoveEntity {moveEntityAction = "Reveal", moveEntityCoordX = x, moveEntityCoordY = y, moveEntityTimeStamp=timeStamp}

moveEntityToMove :: MoveEntity -> Move
moveEntityToMove MoveEntity{moveEntityAction="Flag", moveEntityCoordX=x, moveEntityCoordY=y, moveEntityTimeStamp=timeStamp} = Flag (x,y) timeStamp
moveEntityToMove MoveEntity{moveEntityAction=_, moveEntityCoordX=x, moveEntityCoordY=y, moveEntityTimeStamp=timeStamp} = Reveal (x,y) timeStamp

-- Creates a new game for a given Dimension, bombCount & seed
newGame :: Dimension -> Int -> Int -> GameState
newGame (h,w) b s = GameState { board = generateBoard (h,w) b s,
                                moves = [],
                                bombCount = b,
                                seed = s,
                                status = Ongoing}

-- Executes a move on a given GameState
-- TODO handle illegal moves e.g. outOfBounds Action, action on a board with status Won || Lost
makeMove :: GameState -> Move -> GameState
makeMove state m  = state { board   = boardAfterMove,
                            moves   = moves state ++ [m],
                            status  = checkStatus boardAfterMove}
                              where boardAfterMove = case m of (Flag c _)   -> flagCell (board state) c
                                                               (Reveal c _) -> revealCell (board state) c


checkStatus :: Board -> GameStatus
checkStatus board = case (checkWon board, checkLost board) of  (_,True)      -> Lost
                                                               (True,False)  -> Won
                                                               (False,False) -> Ongoing
