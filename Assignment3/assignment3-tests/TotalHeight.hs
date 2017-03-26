module TotalHeight where
import WindowLayout 

totalHeight :: WindowLayout -> Int

totalHeight (Window {wname = x, width = y, height = z}) = z

totalHeight (Vertical []) = 0
totalHeight (Vertical (Window {wname = _, width = _, height = z}:others)) = (z + (totalHeight (Vertical others)))

totalHeight (Horizontal []) = 0
totalHeight (Horizontal (Window {wname = _, width = _, height = z}:others)) = max z (totalHeight (Horizontal others))

totalHeight (Horizontal (x:xs)) = max (totalHeight x) (totalHeight (Horizontal xs))
totalHeight (Vertical (x:xs)) = max (totalHeight x) (totalHeight (Vertical xs))