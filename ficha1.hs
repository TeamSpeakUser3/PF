module Ficha1 where

perimetro :: Float -> Float
perimetro r = 2 * pi * r

primUlt :: [a] -> (a, a)
primUlt l= (head l, last l)

multiplo :: Int -> Int -> Bool
multiplo a b = mod a b == 0

 
