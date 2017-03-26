module SplitScreen where
import WindowLayout 
import FoldWindowLayout

splitScreen :: String -> WindowLayout -> WindowLayout

splitScreen name x = foldWindowLayout (\(n,w,h) -> 
    if(n == name) then (Horizontal (let win = Window {wname = n, width = (w `div` 2), height = h} in [win, win] )) 
       else (Window {wname = n, width = w, height = h}) ) Horizontal Vertical x