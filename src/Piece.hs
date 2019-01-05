module Piece where 

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
readPiece c = fmap makePiece charToPieceType
    where   
        color = if isLower c then White else Black
        charToPieceType = lookup (toLower c) charPieceMapping
        makePiece = Piece color
        charPieceMapping = [('p',Pawn),
                            ('n',Knight),
                            ('b',Bishop),
                            ('r',Rook),
                            ('q',Queen),
                            ('k',King)]