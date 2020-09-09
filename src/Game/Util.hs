module Game.Util (generateMatrixWithCellNumbers,
                  generateMatrixWithCellIndices,
                  getHeightAndWidthFromBoard) where

import           Data.Matrix
import           Model
import           Game.Board  (Dimension, coordinateToCellNumber)
import           Import (headEx)


generateMatrixWithCellNumbers :: Dimension -> Matrix Int
generateMatrixWithCellNumbers (w,h) = matrix w h (\(i,j) -> coordinateToCellNumber (i,j) (w,h))

generateMatrixWithCellIndices :: Dimension -> Matrix String
generateMatrixWithCellIndices (w,h) = matrix w h (\(i,j) -> (show i) ++ "/" ++ (show j))

getHeightAndWidthFromBoard :: [Row] -> (Int, Int)
getHeightAndWidthFromBoard rows = (length rows, length $ rowCells $ headEx rows )