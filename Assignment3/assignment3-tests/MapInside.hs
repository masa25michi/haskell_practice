module MapInside where

mapInside :: (a -> b) -> [[a]] -> [[b]]

mapInside f lst = map localf lst 
  where localf lst = (map f lst)