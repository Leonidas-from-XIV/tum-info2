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
decodeInt' :: Integer -> [Integer] -> [Integer] -> Bool -> ([Integer], [Integer])
decodeInt' 0 x y isx = (x, y)
decodeInt' i x y isx
  | isx = decodeInt' (i `div` 2) (mod i 2:x) y False
  | otherwise = decodeInt' (i `div` 2) x (mod i 2:y) True

num :: [Integer] -> Integer
num = foldl (\acc e -> acc *2 + e) 0

decodeInt :: Integer -> (Integer, Integer)
decodeInt x = (num z, num y)
  where (z, y) = decodeInt' x [] [] True

decodeIntPairs :: [Integer] -> [(Integer, Integer)]
decodeIntPairs = map decodeInt . filter (>=0)
{-TTEW-}
