module DeleteNth where
deleteNth :: (Eq a) => Int -> a -> [a] -> [a]
deleteNth n target [] = []
deleteNth n target (x:xs) = if (x /= target) then 
 [x]++ (deleteNth n target xs)
 else if (n <= 1) then 
 xs
 else
 [x] ++ (deleteNth (n-1) target xs)