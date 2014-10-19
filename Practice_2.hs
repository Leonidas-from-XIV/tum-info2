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

--elem' x [] = False
--elem' x (y:ys)
--  | x == y = True
--  | otherwise = elem x ys

union :: [Integer] -> [Integer] -> [Integer]
--union a [] = a
--union [] a = a
--union (x:xs) lat = if x `elem'` lat then union xs lat else x:(union xs lat)
union xs ys = xs ++ ys

intersection :: [Integer] -> [Integer] -> [Integer]
--intersection [] _ = []
--intersection _ [] = []
--intersection (x:xs) lat = if x `elem'` lat then x:(intersection xs lat) else intersection xs lat
intersection xs ys = [x | x <- xs, y <- ys, x == y]

diff :: [Integer] -> [Integer] -> [Integer]
--diff a [] = a
--diff [] a = []
--diff (x:xs) ys = if x `elem'` ys then diff xs ys else x:(diff xs ys)
diff xs ys = [x | x <- xs, not (x `elem` ys)]

elem' :: Integer -> [Integer] -> Bool
elem' x xs = not (null [y | y <- xs, y == x])

prop_inter_1 x xs ys = x `elem` xs ==> x `elem` ys ==> x `elem` intersection xs ys
prop_inter_2 x xs ys = x `elem` intersection xs ys ==> x `elem` xs && x `elem` ys
prop_inter_3 x xs ys = (x `elem` xs && x `elem` ys) == (x `elem` intersection xs ys)

{- G2.3 -}

eqFrac :: (Integer,Integer) -> (Integer,Integer) -> Bool
eqFrac (a,b) (c,d) = a * d == c * b

prop_eqFrac_scale a b n = eqFrac (a, b) (n * a, n * b)
prop_eqFrac_refl a b = eqFrac (a, b) (a, b)

pow2_slow :: Integer -> Integer
pow2_slow 0 = 1
pow2_slow n | n > 0 = 2 * pow2_slow (n - 1)

pow2 :: Integer -> Integer
pow2 0 = 1
pow2 n | n < 0 = undefined
       | n `mod` 2 == 0 = k * k
       | otherwise = 2 * pow2 (n - 1)
     where k = pow2 (n `div` 2)
