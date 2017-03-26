module FreeQBExp where
import QBExp

freeQBExp :: QBExp -> [String]

freeQBExp (Varref str) = rmDup (str:[])
freeQBExp (Not exp) = let x = (freeQBExp exp) in (rmDup x)
freeQBExp (exp1 `Or`exp2) = let x1 = (freeQBExp exp1) in let x2 = (freeQBExp exp2) in (rmDup (x1++x2))
--Exists String QBExp
freeQBExp (Exists str exp) = let x = freeQBExp(exp) in (if (str `elem` x) then (rmStr str x) else x)

rmDup :: [String] -> [String]
rmDup [] = []
rmDup (x:xs) = x : rmDup (filter (\y -> not(x == y)) xs)

rmStr :: String -> [String] -> [String]
rmStr target str = [x | x <- str, x /= target]