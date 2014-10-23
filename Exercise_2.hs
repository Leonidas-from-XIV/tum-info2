module Exercise_2 where

import Data.List

{-H2.1-}

index :: [Char] -> Char -> Int
index xs x
  | x `elem` xs = snd $ head $ filter fst $ map (\(e, pos) -> (x == e, pos)) $ zip xs [0..]
  | otherwise = length xs

{-H2.2-}

hasDraws :: [(String, Integer)] -> Bool
hasDraws [] = False
hasDraws lat = k /= nub k
  where k = map snd lat


{-H2.3-}

perms _ existing 0 = existing
perms add existing n = perms add [(x:xs) | x<-add, xs<-existing] (n-1)

intLists :: (Integer, Integer) -> Integer -> [[Integer]]
intLists (m, n) k
  | k < 0 = []
  | otherwise = perms [m..n] [[]] k


{-H2.4-}

{-WETT-}
decodeInt' :: Integer -> [Bool] -> [Bool] -> Bool -> ([Bool], [Bool])
decodeInt' 0 x y isx = (x, y)
decodeInt' i x y isx
  | isx = decodeInt' (i `div` 2) (odd i:x) y False
  | otherwise = decodeInt' (i `div` 2) x (odd i:y) True

num :: [Bool] -> Integer
num [False] = 0
num [True] = 1
num (x:xs) = 2*(num xs) + (if x then 1 else 0)

decodeInt :: Integer -> (Integer, Integer)
decodeInt x = (num $ reverse z, num $ reverse y)
  where (z, y) = decodeInt' x [] [] True

decodeIntPairs :: [Integer] -> [(Integer, Integer)]
decodeIntPairs = map decodeInt . filter (>=0)
{-TTEW-}
