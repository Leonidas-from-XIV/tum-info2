module Practice_1 where

import Test.QuickCheck
-- import Test.SmallCheck

fac :: Integer -> Integer
fac n
  | n == 0 = 1
  | otherwise = n * fac (n - 1)

-- fac :: Integer -> Integer
-- fac 0 = 1
-- fac n = n * fac (n - 1)

sum_eleven' :: Integer -> Integer -> Integer
sum_eleven' num times
  | times == 10 = num
  | otherwise = num + sum_eleven' (num + 1) (times + 1)

sum_eleven :: Integer -> Integer
sum_eleven n = sum_eleven' n 0

-- sum_eleven' :: Integer -> Integer -> Integer
-- sum_eleven' num times
--   | times == 0 = num
--   | otherwise = num + sum_eleven' (num + 1) (times - 1)

-- sum_eleven' num 0 = num
-- sum_eleven' num times = num + sum_eleven' (num + 1) (times - 1)

-- sum_eleven :: Integer -> Integer
-- sum_eleven n = sum_eleven' n 10

-- sum_eleven n = sum [n..10+n]
-- sum_eleven 0 = 55
-- sum_eleven n = 11 + sum_eleven (n - 1)

prop_e n = n >= 0 ==> sum_eleven n == sum [n..10+n]

g :: Integer -> Integer
g n = if n < 10 then n*n else n

max_g :: Integer -> Integer
max_g n
  | n > 0 = if (g n) > (g (max_g (n-1))) then n else max_g (n-1)
  | otherwise = 0

max_g' n
  | n <= 0 = 0
  | n > 0 && n <= 9 = n
  | otherwise = 9

prop_m n = max_g n == max_g' n
