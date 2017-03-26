module ConcatMap where
import Prelude hiding (concatMap)

concatMap :: (a -> [b]) -> [a] -> [b]

concatMap f xs = (foldr (\x acc -> f x ++ acc) [] xs)