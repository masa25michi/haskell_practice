module CubeOdds where
cubeOdds :: [Integer] -> [Integer]

cubeOdds inputList = [if odd x then x^3 else x | x <- inputList]