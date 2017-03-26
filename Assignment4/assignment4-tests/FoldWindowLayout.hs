module FoldWindowLayout where
import WindowLayout

foldWindowLayout :: ((String, Int, Int) -> r) -> ([r] -> r) -> ([r] -> r) -> WindowLayout -> r

foldWindowLayout wf hf vf (Window {wname = n, width = w, height = h}) = wf (n,w,h)
foldWindowLayout wf hf vf (Horizontal lst) = hf (map (\x -> foldWindowLayout wf hf vf x) lst)
foldWindowLayout wf hf vf (Vertical lst) = vf (map (\x -> foldWindowLayout wf hf vf x) lst)