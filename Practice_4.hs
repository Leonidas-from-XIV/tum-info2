module Exercise_4 where

import Data.List (partition, genericLength, span, sort, nub, delete)

{- G1 -}

hasFibonacciProperty :: [Integer] -> Bool
hasFibonacciProperty (x:y:z:zs) = x + y == z && hasFibonacciProperty (y:z:zs)
hasFibonacciProperty _ = True

{- G2 -}

{- See reverse_snco.cprf -}

{- G3 -}

perms :: [Char] -> [[Char]]
perms "" = [""]
perms xs = reverse (sort (nub [y:ys | y <- xs, ys <- perms (delete y xs)]))

{- G4 -}

match :: [Char] -> [Char] -> Bool
match [] ys = null ys
match ('?' : ps) (_ : ys) = match ps ys
match ('*' : ps) [] = match ps []
match ('*' : ps) (y : ys) = match ps (y : ys) || match ('*' : ps) ys
match (p : ps) [] = False
match (p : ps) (y : ys) = p == y && match ps ys

{-H4.1-}

allSumOfPrevious :: [Integer] -> Bool
allSumOfPrevious (x:y:xs) = x == y && allSumOfPrevious (x+y:xs)
allSumOfPrevious _        = True

{-H4.2-}

splitter :: Eq a => [a] -> [a] -> [[a]]
splitter [] _ = []
splitter xs [] = [xs]
splitter xs (p:ps) = prefix : cont rest
  where (prefix, rest) = span (/= p) xs
        cont [] = []
        cont [_] = [[]] -- optional
        cont (_:tail) = splitter tail ps

splitter' :: Eq a => [a] -> [a] -> [[a]]
splitter' [] _ = []
splitter' xs [] = [xs]
splitter' xs (p:ps) = prefix : cont rest
  where (prefix, rest) = span (/= p) xs
        cont [] = []
        cont (_:tail) = splitter' tail ps

{-H4.4-}

-- Lösung für die Hausaufgabe
-- nur für nicht-negative Zahlen
encodeInts :: [Integer] -> Integer
encodeInts xs = encodePosPair (genericLength xs, go xs)
  where go [] = 0
        go (x:xs) = encodePosPair (x, go xs)

decodeInts :: Integer -> [Integer]
decodeInts n = go count values
  where (count, values) = decodePosPair n
        go 0 _ = []
        go n m = h : go (n-1) t
          where (h, t) = decodePosPair m

-- aus H2.4
-- nur für nicht-negative Zahlen
decodePosPair :: Integer -> (Integer, Integer)
decodePosPair 0 = (0, 0)
decodePosPair n = (2 * a + d, 2 * b + e)
  where
    (a, b) = decodePosPair (n `div` 4)
    d = n `mod` 2
    e = (n `div` 2) `mod` 2

encodePosPair :: (Integer, Integer) -> Integer
encodePosPair = fromList 0 . reverse . toList
  where toList (0, 0) = []
        toList (x, y) = let (q, r) = divMod x 2 in r : toList (y, q)
        fromList n [] = n
        fromList n (x : xs) = fromList (2 * n + x) xs
