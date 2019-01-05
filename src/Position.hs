module Position where

type Pos = (Int, Int)

fromAbsIndex :: Int -> Pos
fromIndex n = (n `mod` 8, n `quot` 8)

toAbsIndex :: Pos -> Int
toAbsIndex (c, r) = 8 * r + f

isValid :: Pos -> Bool
isValid (c, r) = onBoard c && onBoard r
   where onBoard p = p >= 0 && p <= 7