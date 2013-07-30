-- -*- mode: Haskell -*-
-- First set of the 99 Haskell programming problems

module Set1 where

import Data.List

-- Problem 1
myLast :: [a] -> a
myLast = head . reverse

-- Problem 2
myButLast :: [a] -> a
myButLast = head . tail . reverse

-- Problem 3
elementAt :: [a] -> Int -> a
elementAt as n = as !! (n-1)

-- Problem 4
myLength :: [a] -> Int
myLength = foldl (\x y -> x + 1) 0

-- Problem 5
myReverse :: [a] -> [a]
myReverse [] = []
myReverse (x:xs) = (myReverse xs) ++ [x]

betterReverse :: [a] -> [a]
betterReverse = foldl (flip (:)) []

-- Problem 6
isPalindrome :: Eq a => [a] -> Bool
isPalindrome [] = True
isPalindrome (x:[]) = True
isPalindrome (x:xs)
  | x == (last xs) = isPalindrome (init  xs)
  | otherwise = False

isPalindrome' :: Eq a => [a] -> Bool
isPalindrome' as = as == (reverse as)

-- Problem 7
data NestedList a = Elem a
                  | List [NestedList a]

flatten :: NestedList a -> [a]
flatten (Elem x) = [x]
flatten (List []) = []
flatten (List xs) = foldr (++) [] (map flatten xs)

-- Problem 8
compress :: Eq a => [a] -> [a]
compress [] = []
compress (x:[]) = [x]
compress (x:y:rest)
  | x == y = compress (y : rest)
  | otherwise = x : compress (y : rest)

-- Problem 9
pack :: Eq a => [a] -> [[a]]
pack [] = []
pack (x:[]) = [[x]]
pack list@(x:_) = xs : (pack rest)
  where
    (xs, rest) = span (\y -> x == y) list

-- Problem 10
encode :: Eq a => [a] -> [(Int, a)]
encode xs = zip (map length packed) (map head packed)
  where packed = pack xs

encode' :: Eq a => [a] -> [(Int, a)]
encode' = map (\x -> (length x, head x)) . pack
