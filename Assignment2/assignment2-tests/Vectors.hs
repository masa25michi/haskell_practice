module Vectors where

type Vector = [Double]
scale :: Double -> Vector -> Vector
add :: Vector -> Vector -> Vector
dot :: Vector -> Vector -> Double


scale y v = [b * y| b <- v]

add x y = zipWith (+) x y

dot [] [] = 0.0::Double
dot x y = head x * head y + dot (tail x::Vector) (tail y::Vector)
