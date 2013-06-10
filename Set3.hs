module Set3 where

import Set2
import Data.List
import System.Random
import Hugs.Observe

-- Problem 21
insertAt :: a -> [a] -> Int -> [a]
insertAt y xs 1 = y : xs
insertAt y (x:xs) n = x : insertAt y xs (n-1)

-- Problem 22
range :: Int -> Int -> [Int]
range a b = [a..b]

range' :: Int -> Int -> [Int]
range' a b
  | a == b = []
  | a < b  = a : (range (a+1) b)
  | a > b = reverse $ range' b a

-- Problem 23
rnd_select :: (Eq a, RandomGen g) => [a] -> Int -> g -> ([a], g)
rnd_select [] _   gen = ([], gen)
rnd_select _  0   gen = ([], gen)
rnd_select xs num gen = (item : rest, gen'')
  where
    numItems = (length xs) - 1
    (index, gen') = randomR (0, numItems) gen
    item = xs !! index
    remaining = delete item xs
    (rest, gen'') = rnd_select remaining (num-1) gen'

rnd_selectIO :: Eq a => [a] -> Int -> IO [a]
rnd_selectIO l n = getStdRandom $ rnd_select l n

-- Problem 24
diff_select :: RandomGen g => Int -> Int -> g -> ([Int], g)
diff_select n m gen = diff_select' n [1..m] gen

diff_select' :: RandomGen g => Int -> [Int] -> g -> ([Int], g)
diff_select' 0  _ gen = ([], gen)
diff_select' _ [] gen = error "No items to choose from"
diff_select' n ms gen = (item : rest, gen'')
  where
    (index, gen') = randomR (0, (length ms) - 1) gen
    item = ms !! index
    remaining = delete item ms
    (rest, gen'') = diff_select' (n - 1) remaining gen'

diff_selectIO :: Int -> Int -> IO [Int]
diff_selectIO n m = getStdRandom $ diff_select n m