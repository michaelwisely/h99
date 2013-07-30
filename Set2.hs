-- -*- mode: Haskell -*-
-- Second set of the 99 Haskell programming problems

module Set2 where

import Data.List
import Set1

-- Problem 11
data Encoding a = Single a
                | Multiple Int a
                  deriving Show

encodeModified :: Eq a => [a] -> [Encoding a]
encodeModified = map code . pack
  where code = \x -> if (length x) == 1 then
                       Single (head x)
                     else
                       Multiple (length x) (head x)

-- Problem 12
decodeModified :: [Encoding a] -> [a]
decodeModified [] = []
decodeModified ((Single x):xs) = x : decodeModified xs
decodeModified ((Multiple num x):xs) = replicate num x ++ decodeModified xs

decodeModified' :: [Encoding a] -> [a]
decodeModified' = concatMap decode
  where
    decode (Single x) = [x]
    decode (Multiple num x) = replicate num x

-- Problem 13
encodeDirect :: Eq a => [a] -> [Encoding a]
encodeDirect [] = []
encodeDirect (x:[]) = [Single x]
encodeDirect lst@(x:_) = term : (encodeDirect rest)
  where
    (xs, rest) = span (==x) lst
    term
      | (length xs) > 1 = Multiple (length xs) x
      | otherwise = Single x

-- Problem 14
dupli :: [a] -> [a]
dupli = foldr (\x acc -> x:x:acc) []

-- Problem 15
repli :: [a] -> Int -> [a]
repli lst n = concatMap (replicate n) lst

repli' = flip $ concatMap . replicate

-- Problem 16
dropEvery :: [a] -> Int -> [a]
dropEvery [] _ = []
dropEvery [x] _ = [x]
dropEvery lst num = (init first) ++ (dropEvery rest num)
  where
    (first, rest) = splitAt num lst

-- Problem 17
split :: [a] -> Int -> ([a], [a])
split [] _ = ([], [])
split xs n = ((take n xs), (drop n xs))

-- Problem 18
slice :: [a] -> Int -> Int -> [a]
slice lst a b = reverse (drop (length lst - b) (reverse (drop (a-1) lst)))

slice1 :: [a] -> Int -> Int -> [a]
slice1 lst a b = drop (a-1) $ take b lst


-- Problem 19
rotate :: [a] -> Int -> [a]
rotate lst n
  | n == 0 = lst
  | n > 0  = drop n lst ++ take n lst
  | n < 0  = drop (len + n) lst ++ take (len + n) lst
  where
    len = length lst

-- Problem 20
removeAt :: Int -> [a] -> (a, [a])
removeAt n lst = (lst !! i, firstPart ++ secondPart)
  where
    i = n-1
    firstPart = take i lst
    secondPart = drop n lst

removeAt' :: Int -> [a] -> (a, [a])
removeAt' n lst = (lst !! i, f ++ tail s)
  where
    i = n-1
    (f, s) = splitAt i lst