module WhatIndex where

whatIndex :: (Eq a) => a -> [a] -> Integer
whatIndex sought [] = (-1)
whatIndex sought (x:xs) | (sought == (x)) = (0)
                      | ((1 + func) >= 1) = (1 + func)
                      | otherwise = (-1)
                      where func = (whatIndex sought (xs))
