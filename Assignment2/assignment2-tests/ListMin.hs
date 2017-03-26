-- $Id: ListMinTests.hs,v 1.1 2013/08/22 18:09:10 leavens Exp $
module ListMin where

listMin :: (Ord a) => [a] -> a
listMin [x] = x
listMin lst = min (head lst) (listMin (tail lst))