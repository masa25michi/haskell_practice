module Rank where
import Data.List
import Data.Function

rank :: (Ord a) => [a] -> [(Int, a)]

rank [] = []
rank lst = let ((i, x):xs) = (zip [1 ..] (sort lst)) in ((i, x):(rank2 x i xs))
        where rank2 _ _ [] = []
              rank2 target targetIndex ((i, x):xs) = 
                 if(x==target) then (targetIndex, x) : (rank2 target targetIndex xs) else (i,x): (rank2 x i xs) 