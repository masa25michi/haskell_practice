module BinaryRelationOps where
import BinaryRelation

project1 :: (BinaryRelation a b) -> [a]
project1 pairs = [a | (a,b) <- pairs]

project2 :: (BinaryRelation a b) -> [b]
project2 pairs = [b | (a,b) <- pairs]

select :: ((a,b) -> Bool) -> (BinaryRelation a b) -> (BinaryRelation a b)
select _ [] = []
select cond pairs = [(x,y) | (x,y) <- pairs, (cond ((x),(y)))]

compose :: Eq b => (BinaryRelation a b) -> (BinaryRelation b c)
                    -> (BinaryRelation a c)
compose pair1 pair2 = [(a,c) | (a,x) <- pair1, (y,c) <- pair2, (x == y)]
