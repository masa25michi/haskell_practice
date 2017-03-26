module EncryptWith where
import ToCharFun

encryptWith :: (Int -> Int) -> [String] -> [String]
encryptWith f lst = (map localf lst)
  where localf locallst = (map (toCharFun f) locallst)