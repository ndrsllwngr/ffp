module Game.Util (generateMatrixWithCellNumbers,
                  generateMatrixWithCellIndices) where

import           Data.Matrix
import           Game.Board  (Dimension, coordinateToCellNumber)


generateMatrixWithCellNumbers :: Dimension -> Matrix Int
generateMatrixWithCellNumbers (w,h) = matrix w h (\(i,j) -> coordinateToCellNumber (i,j) (w,h))

generateMatrixWithCellIndices :: Dimension -> Matrix String
generateMatrixWithCellIndices (w,h) = matrix w h (\(i,j) -> (show i) ++ "/" ++ (show j))
