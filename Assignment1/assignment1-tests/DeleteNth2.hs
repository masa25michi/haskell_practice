module DeleteNth where
deleteNth :: (Eq a) => Int -> a -> [a] -> [a]
deleteNth n target [] = []
deleteNth n target (x:xs) = if (x /= target) then 
 [x]++ (deleteNth n target xs)
 else if (n <= 1) then 
 -- (deleteNth 0 target xs)
 xs
 else
 [x] ++ (deleteNth (n-1) target xs)



everyNth :: [a] -> Int -> [a]
everyNth xs n = [snd x | x <- (zip [1..] xs), fst x `mod` n == 0]

deleteNth' n target inputList = [x |x<-inputList, (x/=target)]
	-- (let (n = n-1) in (if (n <= 0) then [] else [x])) | x<- inputList]

-- [if (x /= target) then x else (x + 1) | x <- inputList] 