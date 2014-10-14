module Practice_2 where

import Test.QuickCheck
import qualified Data.List as D

all_sums :: [Integer] -> [Integer] -> [Integer]
all_sums xs ys = [x+y | x <- xs, y <- ys]

evens :: [Integer] -> [Integer]
evens xs = [x | x <- xs, x `mod` 2 == 0]

n_lists :: [Integer] -> [[Integer]]
n_lists xs = [[1..x] | x <- xs]

all_even_sum_lists :: [Integer] -> [Integer] -> [[Integer]]
all_even_sum_lists xs ys = [[1..x+y] | x <- xs, y <- ys, (x+y) `mod` 2 == 0]

all_even_sum_lists' :: [Integer] -> [Integer] -> [[Integer]]
all_even_sum_lists' xs ys = n_lists $ evens $ all_sums xs ys

prop_all xs ys = all_even_sum_lists' xs ys == all_even_sum_lists xs ys

elem' x [] = False
elem' x (y:ys)
  | x == y = True
  | otherwise = elem x ys

union :: [Integer] -> [Integer] -> [Integer]
union a [] = a
union [] a = a
union (x:xs) lat = if x `elem'` lat then union xs lat else x:(union xs lat)

intersection :: [Integer] -> [Integer] -> [Integer]
intersection [] _ = []
intersection _ [] = []
intersection (x:xs) lat = if x `elem'` lat then x:(intersection xs lat) else intersection xs lat

diff :: [Integer] -> [Integer] -> [Integer]
diff a [] = a
diff [] a = []
diff (x:xs) ys = if x `elem'` ys then diff xs ys else x:(diff xs ys)
