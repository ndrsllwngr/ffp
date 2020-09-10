{-# LANGUAGE TemplateHaskell #-}




module Game.Board  (generateBoard,
                    revealCell,
                    revealAllNonFlaggedCells,
                    flagCell,
                    checkLost,
                    checkWon,
                    coordinateToCellNumber, 
                    Coordinate,
                    Dimension,
                    Board,
                    Cell (..)
                    ) where

import           Data.List
import           Data.Matrix
import           System.Random
import           System.Random.Shuffle
import           Control.Lens

type Dimension = (Int,Int)
type Coordinate = (Int,Int)

data Cell = Cell { _isFlagged        :: Bool,
                   _isRevealed       :: Bool,
                   _hasBomb          :: Bool,
                   _neighboringBombs :: Int,
                   _coordinate       :: Coordinate
                   } deriving (Show, Eq)
makeLenses ''Cell

type Board = Matrix Cell

-- Generates a Minesweeper Board with a given dimension, number of bombs and random seed
generateBoard :: Dimension -> Int -> Int -> Board
generateBoard (h,w) bombCount seed = matrix h w (\(i,j) -> Cell {
                                                              _isRevealed = False,
                                                              _isFlagged = False,
                                                              -- the cell has a bomb on it if the cell number is part of the bomb cells
                                                              _hasBomb = coordinateToCellNumber (i,j) (h,w) `elem` bombPos,
                                                              -- the amount of neighboring bombs is equal to:
                                                              -- the length of the intersection between the neighbouring cell numbers & the bomb cell numbers
                                                              _neighboringBombs = length $ map toCellNumber (neighbourCells (i,j) (h,w)) `intersect` bombPos,
                                                              -- the cells coordinate
                                                              _coordinate = (i,j)
                                                            })
                                                            where
                                                                -- initialize randomizer with seed
                                                                rand = mkStdGen seed
                                                                -- calculate number of total cells on the board
                                                                numCells = h * w
                                                                -- calculate the cell numbers with bombs
                                                                bombPos = take bombCount (shuffle' [1..numCells] numCells rand)
                                                                -- helper function to slim down the neighboring Bomb calculation
                                                                toCellNumber x = coordinateToCellNumber x (h,w)

-- helper function to set a specific cells isRevealed flag to True
setCellToRevealed :: Board -> Coordinate -> Board
setCellToRevealed b (x,y) = setElem newCell (x,y) b where newCell = (getElem x y b) & isRevealed .~ True

-- Reveals a cell at a given coordinate for a given Board
-- Rule explanation: will also reveal any direct neighbouring Cells which have no bomb and their neighbour cells if the have 0 neighboring bombs
revealCell :: Board -> Coordinate -> Board
revealCell board (i,j) = resultBoard where
                            -- dimension of the board
                            dim =  (nrows board, ncols board)
                            resultBoard = case getElem i j board of
                                            -- case of a unrevealed cell with no neighboring bombs and which also does not contain a bomb
                                            -- in this case we want to reveal the neighboring cells as well
                                            (Cell _ True _ _ _)           -> board
                                            (Cell True _ _ _ _)           -> board
                                            (Cell False False False 0 _)  -> neighboursBoard where
                                                                  -- the board with the cell (i,j) set to revealed
                                                                  cellBoard = setCellToRevealed board (i,j)
                                                                  -- all the direct neighbours of the cell (i,j)
                                                                  neighbours = directNeighbourCells (i,j) dim
                                                                  -- fold over the list of neighbours and recursively call revealCell for each one
                                                                  neighboursBoard = foldl revealCell cellBoard neighbours
                                            -- In any other case just reveal the cell at (i,j)
                                            _ -> setCellToRevealed board (i,j)

-- Reveals all cells which have not been flagged
revealAllNonFlaggedCells :: Board -> Board
revealAllNonFlaggedCells board = resultBoard where 
                                cellsToReveal   = filter (not . _isFlagged)(toList board)
                                cellCoordinates = map _coordinate cellsToReveal
                                resultBoard     = foldl setCellToRevealed board cellCoordinates
                            

-- Toggles the isFlagged state of a cell at a given coordinate for a given board
flagCell :: Board -> Coordinate -> Board
flagCell b (i,j) = setElem newCell (i,j) b where
                                            oldCell = getElem i j b
                                            flag = if oldCell ^. isRevealed then oldCell ^. isFlagged else not $ oldCell ^. isFlagged
                                            newCell = oldCell & isFlagged .~ flag

-- Checks if any bomb has been revealed
checkLost :: Board -> Bool
checkLost board = any (\c -> c ^. isRevealed && c ^. hasBomb) (toList board)

-- Checks if all non bomb fields are revealed OR if all bombs have been flagged
checkWon :: Board -> Bool
checkWon board = all _isRevealed $ filter (not . _hasBomb) (toList board) --TODO not sure how to use lens notation here and in the other funky map cases ^_

-- Calculates the cell number of a given XY-Coordinate for a given Board size
-- will also calculate out of bounds cells if out of bounds coordinates are provided
coordinateToCellNumber :: Coordinate -> Dimension -> Int
coordinateToCellNumber (i,j) (_,w) = (i-1) * w + j

-- Checks if a coordinate is inBounds of a given Board size
inBounds :: Coordinate -> Dimension -> Bool
inBounds (i,j) (h,w) = (i > 0) && (i <= h) && (j > 0) && (j <= w)


-- Calculates all inBounds direct (non diagonal) neighbour cells of a given cell
directNeighbourCells :: Coordinate -> Dimension -> [Coordinate]
directNeighbourCells (i,j) (h,w) = filter (\x -> inBounds x (h,w)) theoreticalNeighbors
                                    where
                                      theoreticalNeighbors = [(i-1,j),    -- top
                                                              (i,j-1),    -- left
                                                              (i,j+1),    -- right
                                                              (i+1,j)]    -- bottom

-- Calculates all inBounds (including diagonal) neighbour cells of a given cell
neighbourCells :: Coordinate -> Dimension -> [Coordinate]
neighbourCells (i,j) (h,w) = filter (\x -> inBounds x (h,w)) theoreticalNeighbors
                                    where
                                      theoreticalNeighbors = [(i-1,j-1),  -- top-left
                                                              (i-1,j),    -- top
                                                              (i-1,j+1),  -- top-right
                                                              (i,j-1),    -- left
                                                              (i,j+1),    -- right
                                                              (i+1,j-1),  -- bottom-left
                                                              (i+1,j),    -- bottom
                                                              (i+1,j+1)]  -- bottom-right
