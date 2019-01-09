module Teste19 where

import Control.Monad
import Data.Char
import System.Random

-- 1
-- a)
elemIndices :: Eq a => a -> [a] -> [Int]
elemIndices a b =
  fst $
  foldr
    (\x (l, i) ->
       if x == a
         then ((l ++ [i]), succ i)
         else (l, succ i))
    ([], 0)
    b

-- b)
-- isSubsequenceOf :: Eq a => [a] -> [a] -> Bool
-- 2)
data BTree a
  = Empty
  | Node a
         (BTree a)
         (BTree a)

-- a)
lookupAP :: Ord a => a -> BTree (a, b) -> Maybe b
lookupAP _ Empty = Nothing
lookupAP p (Node a x y)
  | fst a == p = Just (snd a)
  | p > fst a = lookupAP p y
  | otherwise = lookupAP p x

-- b)
zipWithBT :: (a -> b -> c) -> BTree a -> BTree b -> BTree c
zipWithBT _ _ Empty = Empty
zipWithBT _ Empty _ = Empty
zipWithBT f (Node a l r) (Node b l' r') =
  Node (f a b) (zipWithBT f l l') (zipWithBT f r r')

-- 3
digitAlpha :: String -> (String, String)
digitAlpha = liftM2 (,) (filter (isDigit)) (filter (not . isDigit))

-- 4
data Seq a
  = Nil
  | Cons a
         (Seq a)
  | App (Seq a)
        (Seq a)
  deriving (Show)

-- a)
firstSeq :: Seq a -> a
firstSeq Nil = undefined
firstSeq (Cons a _) = a
firstSeq (App a _) = firstSeq a

-- b) 
dropSeq :: Int -> Seq a -> Seq a
dropSeq 0 l = l
dropSeq _ Nil = Nil
dropSeq i (App x z) =
  if l >= i + 1
    then App (dropSeq i x) z
    else dropSeq (i - l) z
  where
    l = contSeq x
dropSeq i (Cons _ z) = dropSeq (pred i) z

contSeq :: Seq a -> Int
contSeq (App x y) = contSeq x + contSeq y
contSeq (Cons a b) = 1 + contSeq b
contSeq Nil = 0

-- c)
-- 5
type Mat a = [[a]]

-- a)
getElem :: Mat a -> IO a
getElem a = do
  x <- randomRIO (0, length a - 1)
  y <- randomRIO (0, (length $ head a) - 1)
  return $ (a !! x) !! y

-- b)
sumLines :: Mat Int -> Bool
sumLines a = filter (/= head x) x == []
  where
    x = map sum a

sumCols :: Mat Int -> Bool
sumCols a = filter (/= head x) x == []
  where
    x = cenas a
    cenas a =
      if head a /= []
        then sum (map (head) a) : cenas (map tail a)
        else []

magic :: Mat Int -> Bool
magic a = sumLines a && sumCols a
