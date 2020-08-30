module Game.Board  (generateBoard,
                    generateMatrixWithCellNumbers,
                    generateMatrixWithCellIndices,
                    revealCell,
                    flagCell) where

import Data.Matrix
import Data.List
import System.Random
import System.Random.Shuffle

type Dimension = (Int,Int)
type Coordinate = (Int,Int)

data Cell = Cell { isRevealed :: Bool
                 , isFlagged :: Bool  
                 , hasBomb :: Bool  
                 , neighboringBombs :: Int
                 }

instance Show Cell where
   show (Cell _ True _ n)       = "F"
   show (Cell True _ _ n)       = show n 
   show (Cell False _ True _)   = "B"
   show _                       = "_"

type Board = Matrix Cell

-- Generates a Minesweeper Board with a given dimension, number of bombs and random seed
generateBoard :: Dimension -> Int -> Int -> Board
generateBoard (h,w) bombCount seed = matrix h w (\(i,j) -> Cell {
                                                              isRevealed = False,
                                                              isFlagged = False,
                                                              -- the cell has a bomb on it if the cell number is part of the bomb cells
                                                              hasBomb = coordinateToCellNumber (i,j) (h,w) `elem` bombPos,
                                                              -- the amount of neighboring bombs is equal to:
                                                              -- the length of the intersection between the neighbouring cell numbers & the bomb cell numbers
                                                              neighboringBombs = length $ map toCellNumber (neighbourCells (i,j) (h,w)) `intersect` bombPos
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

-- Reveals a cell at a given coordinate for a given Board 
-- Rule explanation: will also reveal any direct neighbouring Cells which have no bomb and their neighbour cells if the have 0 neighboring bombs
revealCell :: Board -> Coordinate -> Board
revealCell board (i,j) = resultBoard where
                            -- dimension of the board
                            dim =  (nrows board, ncols board)
                            resultBoard = case getElem i j board of
                                            -- case of a unrevealed cell with no neighboring bombs and which also does not contain a bomb
                                            -- in this case we want to reveal the neighboring cells as well
                                            (Cell False _ False 0 ) -> neighboursBoard where
                                                                  -- the board with the cell (i,j) set to revealed
                                                                  cellBoard = setCellToRevealed board (i,j)
                                                                  -- all the direct neighbours of the cell (i,j)
                                                                  neighbours = directNeighbourCells (i,j) dim
                                                                  -- fold over the list of neighbours and recursively call revealCell for each one
                                                                  neighboursBoard = foldl revealCell cellBoard neighbours
                                            -- In any other case just reveal the cell at (i,j)                 
                                            _ -> setCellToRevealed board (i,j)

-- Toggles the isFlagged state of a cell at a given coordinate for a given board
flagCell :: Board -> Coordinate -> Board
flagCell b (i,j) = setElem newCell (i,j) b where
                                            oldCell = getElem i j b
                                            newCell = oldCell {isFlagged = not $ isFlagged oldCell }

setCellToRevealed :: Board -> Coordinate -> Board
setCellToRevealed b (i,j) = setElem newCell (i,j) b where newCell = (getElem i j b) {isRevealed = True}


-- Calculates the cell number of a given XY-Coordinate for a given Board size
-- will also calculate out of bounds cells if out of bounds coordinates are provided
coordinateToCellNumber :: Coordinate -> Dimension -> Int
coordinateToCellNumber (i,j) (_,w) = (i-1) * w + j


-- Calculates the XY-Coordinate of a given cell number for a given Board size
-- will also calculate out of bounds cells if out of bounds cell numbers are provided
cellNumberToCoordinate :: Int -> Dimension -> Coordinate
cellNumberToCoordinate n (_,w) = (i, j) where
                                    i = ((n - 1) `div` w) + 1
                                    j = ((n - 1) `mod` w) + 1

-- Checks if a coordinate is inBounds of a given Board size
inBounds :: Coordinate -> Dimension -> Bool
inBounds (i,j) (h,w) = (i > 0) && (i <= h) && (j > 0) && (j <= w)


-- Calculates all inBounds neighbour cells of a given cell
directNeighbourCells :: Coordinate -> Dimension -> [Coordinate]
directNeighbourCells (i,j) (h,w) = filter (\x -> inBounds x (h,w)) theoreticalNeighbors
                                    where
                                      theoreticalNeighbors = [(i-1,j),    -- top
                                                              (i,j-1),    -- left
                                                              (i,j+1),    -- right
                                                              (i+1,j)]    -- bottom

-- Calculates all inBounds neighbour cells of a given cell
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



--todo debugging functions to visualize matrix behaviour
generateMatrixWithCellNumbers :: Dimension -> Matrix Int
generateMatrixWithCellNumbers (w,h) = matrix w h (\(i,j) -> coordinateToCellNumber (i,j) (w,h))

generateMatrixWithCellIndices :: Dimension -> Matrix String
generateMatrixWithCellIndices (w,h) = matrix w h (\(i,j) -> (show i) ++ "/" ++ (show j))