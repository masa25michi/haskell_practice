-- $Id: InfSet.hs,v 1.1 2015/02/12 04:17:59 leavens Exp leavens $
module InfSet where

fromFunc :: (a -> Bool) -> (Set a)
unionSet :: Set a -> Set a -> Set a
intersectSet :: Set a -> Set a -> Set a
inSet :: a -> Set a -> Bool
complementSet :: Set a -> Set a

data Set a = Empty | Add (a -> Bool)

fromFunc f = (Add f)

unionSet (Add f) (Add g) = (Add (\x -> (f x)|| (g x)))
intersectSet (Add f) (Add g) = (Add ((\x -> (f x) && (g x))))

inSet x (Add f) | (f x) = True
                | otherwise = False
complementSet (Add f) = (Add (not.f))

