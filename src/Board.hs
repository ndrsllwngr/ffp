module Board where

import Data.Char (toUpper, toLower, isLower) 

initBoardString = unlines ["RNBQKBNR",
                           "PPPPPPPP",
                           "--------",
                           "--------",
                           "--------",
                           "--------",
                           "pppppppp",
                           "rnbqkbnr"]

type Board = [[Square]] --Make board with padding possible
type Square = Maybe Piece

readBoard :: String -> Board
readBoard = map readRow . lines
    where readRow = map readSquare

boardToString :: Board -> String
boardToString = unlines . map rowToString
    where rowToString = map squareToChar

squareToChar :: Square -> Char 
squareToChar = maybe '-' pieceToChar

readSquare :: Char -> Square
readSquare = readPiece


data Piece = Piece PlayerColor PieceType deriving(Show)
data PlayerColor = White | Black deriving(Show)
{-|
  Englische Figuren Namen:
  Pawn = Bauer
  Knight = Springer
  Bishop = Läufer
  Rook = Turm
-}
data PieceType = Pawn | Knight | Bishop | Rook | Queen | King deriving(Show)

pieceToChar :: Piece -> Char
pieceToChar (Piece White p) = pieceTypeToChar p
pieceToChar (Piece Black p) = toUpper (pieceTypeToChar p)

pieceTypeToChar :: PieceType -> Char
pieceTypeToChar Pawn   = 'p'
pieceTypeToChar Knight = 'n'
pieceTypeToChar Bishop = 'b'
pieceTypeToChar Rook   = 'r'
pieceTypeToChar Queen  = 'q'
pieceTypeToChar King   = 'k' 
 
readPiece :: Char -> Maybe Piece
readPiece c = fmap makePiece charToPiece
    where   
        color = if isLower c then White else Black
        charToPiece = lookup (toLower c) charPieceMapping
        makePiece = Piece color
        charPieceMapping = [('p',Pawn),
                        ('n',Knight),
                        ('b',Bishop),
                        ('r',Rook),
                        ('q',Queen),
                        ('k',King)]