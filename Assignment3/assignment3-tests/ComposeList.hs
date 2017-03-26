module ComposeList where

composeList :: [(a -> a)] -> (a -> a)
composeList [] f = f
composeList (x:xs) f = x (composeList xs f)