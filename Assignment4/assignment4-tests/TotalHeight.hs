module TotalHeight where
import WindowLayout 
import FoldWindowLayout

totalHeight :: WindowLayout -> Int

totalHeight x = foldWindowLayout (\(_,_,height) -> height) (\x -> if (null x) then 0 else maximum x) sum x