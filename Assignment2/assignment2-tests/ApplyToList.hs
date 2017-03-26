module ApplyToList where
import BinaryRelation

applyRel :: (Eq k) => k -> (BinaryRelation k v) -> [v]
applyRel k kv = [ snd pair |pair <- kv, (fst pair == k)]

applyToList :: (Eq k) => [k] -> (BinaryRelation k v) -> [v]
applyToList [] kv = []
applyToList keys kv = (applyRel (head keys) kv) ++ (applyToList (tail keys) kv)