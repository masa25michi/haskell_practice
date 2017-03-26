module BinaryRelationOps where 
import BinaryRelation

project1 :: (BinaryRelation a b) -> [a]
project1 brlist = [ fst pair | pair <- brlist]

project2 :: (BinaryRelation a b) -> [b]
project2 brlist = [snd pair | pair <- brlist]

select :: ((a,b) -> Bool) -> (BinaryRelation a b) -> (BinaryRelation a b)
select s [] = []
select s brlists = 
  let pair = (head brlists) in 
  (if (s ((fst pair),(snd pair)))
    then pair : (select s (tail brlists))
    else (select s (tail brlists)))

compose :: Eq b => (BinaryRelation a b) -> (BinaryRelation b c) -> (BinaryRelation a c)
compose ab bc = [ ((fst b1), (snd b2)) | b1 <- ab, b2 <- bc, ((snd b1) == (fst b2))]