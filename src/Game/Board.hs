module Game.Board (generateField, generateMatrixWithCellNumbers, generateMatrixWithCellIndices, neighbourCells, cellNumberToCoordinate) where

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
   show (Cell _ _ b n) = "(" ++ show b ++ "-" ++ show n ++ ")"

type Field = Matrix Cell

-- Generate StdGen with seed 42 => (mkStdGen 42)
generateField :: Dimension -> Int -> Int -> Field
generateField (h,w) bombCount seed = matrix h w (\(i,j) -> Cell {
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




-- Calculates the cell number of a given XY-Coordinate for a given field size
-- will also calculate out of bounds cells if out of bounds coordinates are provided
coordinateToCellNumber :: Coordinate -> Dimension -> Int
coordinateToCellNumber (i,j) (_,w) = (i-1) * w + j


-- Calculates the XY-Coordinate of a given cell number for a given field size
-- will also calculate out of bounds cells if out of bounds cell numbers are provided
cellNumberToCoordinate :: Int -> Dimension -> Coordinate
cellNumberToCoordinate n (_,w) = (i, j) where
                                    i = ((n - 1) `div` w) + 1
                                    j = ((n - 1) `mod` w) + 1

-- Checks if a coordinate is inBounds of a given field size
inBounds :: Coordinate -> Dimension -> Bool
inBounds (i,j) (h,w) = (i > 0) && (i <= h) && (j > 0) && (j <= w)

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