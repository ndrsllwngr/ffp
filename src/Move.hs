module Move where
    
import Piece
data Move = Move{
                from :: Pos,
                to :: Pos,
                piece :: Piece}    

