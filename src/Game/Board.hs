module Game.Board (generateField, generateMatrixWithCellNumbers, neighbourCellNumbers) where

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

generateField :: Dimension -> Int -> StdGen -> Field
generateField (h,w) bombCount rand = matrix h w (\(i,j) -> Cell {
                                                              isRevealed = False,
                                                              isFlagged = False,
                                                              hasBomb = coordinateToCellNumber (i,j) (h,w) `elem` bombPos,
                                                              neighboringBombs = length $ intersect (neighbourCellNumbers (i,j) (h,w)) bombPos
                                                            })
                                                            where
                                                                numCells = h * w
                                                                bombPos = take bombCount (shuffle' [1..numCells] numCells rand)




--todo find shorter name
coordinateToCellNumber :: Coordinate -> Dimension -> Int
coordinateToCellNumber (i,j) (_,w) = (i-1) * w + j

neighbourCellNumbers :: Coordinate -> Dimension -> [Int]
neighbourCellNumbers (i,j) (h,w) = [c-w-1,  -- top-left
                                    c-w,    -- top
                                    c-w+1,  -- top-right
                                    c-1,    -- left
                                    c+1,    -- right
                                    c+w-1,  -- bottom-left
                                    c+w,    -- bottom
                                    c+w+1]  -- bottom-right
                                    where c = coordinateToCellNumber (i,j) (h,w)

--todo debugging functions to visualize matrix behaviour
generateMatrixWithCellNumbers :: Dimension -> Matrix Int
generateMatrixWithCellNumbers (w,h) = matrix w h (\(i,j) -> coordinateToCellNumber (i,j) (w,h))

generateMatrixWithCellIndices :: Dimension -> Matrix String
generateMatrixWithCellIndices (w,h) = matrix w h (\(i,j) -> (show i) ++ "/" ++ (show j))