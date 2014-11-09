module Exercise_5 where

import Data.Char (toUpper)
import Data.List

{-G5.1-}

{-
Berechnet "gg (xx, yy)".
-}

{-G5.2-}

iter :: Int -> (a -> a) -> a -> a
iter n f x | n <= 0 = x
           | otherwise = iter (n - 1) f (f x)

pow :: Int -> Int -> Int
pow n k = iter k mul 1
  where mul x = n * x

drop' :: Int -> [a] -> [a]
drop' n xs = iter n tail xs

replicate' :: Int -> a -> [a]
replicate' n x = iter n cons []
  where cons xs = x : xs

{-G5.3-}

-- rekursiv
partition_rec :: (a -> Bool) -> [a] -> ([a], [a])
partition_rec _ [] = ([], [])
partition_rec p (x : xs) =
  if p x then (x : ts, fs) else (ts, x : fs)
  where (ts, fs) = partition_rec p xs

-- mit filter
partition_filter :: (a -> Bool) -> [a] -> ([a], [a])
partition_filter p xs = (filter p xs, filter help xs)
  where help x = not (p x)

prop_partition_rec xs = partition_rec even xs == partition even xs
prop_partition_filter xs = partition_filter even xs == partition even xs

prop_partitionDistrib xs ys =
    partition even (xs ++ ys) == (xs1 ++ ys1, xs2 ++ ys2)
  where
    (xs1, xs2) = partition even xs
    (ys1, ys2) = partition even ys

{-
Die ersten Implementierung ist effizienter als die zweite, weil "p" nur einmal
pro Element angewandt wird statt zweimal. Die zweite Version ist wahrscheinlich
die einfachste. Statt "help" zu definieren kann man auch "not . p" schreiben
(vgl. 6.7 in den Folien).
-}


{-G5.1-}

{-
Die Evaluierung von "zeros" terminiert nicht:

    zeros = 0 : zeros = 0 : 0 : zeros = 0 : 0 : 0 : zeros = ...

Das sieht man deutlich, wenn man versucht, die Funktion in "ghci" auszuwerten.
-}
