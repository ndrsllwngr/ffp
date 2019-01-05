module Board where

import Data.Char (toUpper, toLower, isLower) 
import Piece

initBoardString = unlines ["RNBQKBNR",
                           "PPPPPPPP",
                           "--------",
                           "--------",
                           "--------",
                           "--------",
                           "pppppppp",
                           "rnbqkbnr"]

type Board = [[Square]] --Make board with padding possible

readBoard :: String -> Board
readBoard = map readRow . lines
    where readRow = map readSquare

boardToString :: Board -> String
boardToString = unlines . map rowToString
    where rowToString = map squareToChar

type Square = Maybe Piece

squareToChar :: Square -> Char 
squareToChar = maybe '-' pieceToChar

readSquare :: Char -> Square
readSquare = readPiece
