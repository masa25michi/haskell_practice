-- testMain :: Integer -> [Integer] -> [Integer]

testMain target (x:xs) = 
	if (x /= target) then [x]++xs
		else []