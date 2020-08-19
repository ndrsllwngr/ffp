module Game.Board (generateField) where

import Data.Matrix
import System.Random
import System.Random.Shuffle

type Dimension = (Int,Int)
type Coordinate = (Int,Int)

data Cell = Cell { isRevealed :: Bool  
                 , isFlagged :: Bool  
                 , hasBomb :: Bool  
                 , neighboringBombs :: Int
                 } deriving (Show)  

type Field = Matrix Cell

generateField :: Dimension -> Int -> StdGen -> Field
generateField (w,h) bombCount rand = matrix w h (\(i,j) -> Cell {
                                                              isRevealed = False,
                                                              isFlagged = False,
                                                              hasBomb = mapCoordinateToCellNumber (i,j) (w,h) `elem` bombPos,
                                                              neighboringBombs = 1
                                                            })
                                                            where
                                                                numCells = w*h
                                                                bombPos = take bombCount (shuffle' [1..numCells] numCells rand)
                                                                --neighbours = undefined

--todo find shorter name
mapCoordinateToCellNumber :: Coordinate -> Dimension -> Int
mapCoordinateToCellNumber (i,j) (_,h)  = (i-1) * h + j


--todo debugging functions to visualize matrix behaviour
generateMatrixWithCellNumbers :: Dimension -> Matrix Int
generateMatrixWithCellNumbers (w,h) = matrix w h (\(i,j) -> mapCoordinateToCellNumber (i,j) (w,h))

generateMatrixWithCellIndices :: Dimension -> Matrix String
generateMatrixWithCellIndices (w,h) = matrix w h (\(i,j) -> (show i) ++ "/" ++ (show j))