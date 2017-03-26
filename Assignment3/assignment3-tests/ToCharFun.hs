module ToCharFun where

toCharFun :: (Int -> Int) -> (Char -> Char)

toCharFun f character = toEnum (f (fromEnum character))