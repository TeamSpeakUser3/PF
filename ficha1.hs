module Ficha1 where

perimetro :: Float -> Float
perimetro r = 2 * pi * r

primUlt :: [a] -> (a, a)
primUlt l= (head l, last l)

multiplo :: Int -> Int -> Bool
multiplo a b = mod a b == 0

max2 :: Int -> Int -> Int
max2 a b = if a > b then a else b

max3 :: Int -> Int -> Int -> Int
max3 a b c  | a > max2 b c  = a
            | otherwise     = max2 b c

type Hora = (Int, Int)

hValida :: Hora -> Bool
hValida (a,b) = a >= 0 && b >= 0 && a < 24 && b < 60

hMaior :: Hora -> Hora -> Bool
hMaior a b = fst a > fst b || (fst a == fst b && snd a > snd b)
