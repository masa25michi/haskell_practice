module SplitScreen where
import WindowLayout 

splitScreen :: String -> WindowLayout -> WindowLayout

splitScreen name (Vertical []) = (Vertical [])
splitScreen name (Horizontal []) = (Horizontal [])

splitScreen name (Window {wname = x, width = y, height = z}) 
    |(x == name) = (Horizontal (let w = Window {wname = x, width = (y `div` 2), height = z} in [w, w] ))
    | otherwise  = (Window {wname = x, width = y, height = z})

splitScreen name (Horizontal lst) = Horizontal (map (splitScreen name) lst)

splitScreen name (Vertical lst) = Vertical (map (splitScreen name) lst)