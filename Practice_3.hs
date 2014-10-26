module Practice_3 where

import Data.List (group)

snoc :: [a] -> a -> [a]
snoc [] a = [a]
snoc (x:xs) a = x:(snoc xs a)

member :: Eq a => a -> [a] -> Bool
member a [] = False
member a (x:xs)
  | a == x = True
  | otherwise = member a xs

butlast :: [a] -> [a]
butlast [] = []
butlast [x] = []
butlast (x:xs) = x:(butlast xs)

uniq :: Eq a => [a] -> [a]
uniq [] = []
uniq [x] = [x]
uniq (y:x:xs)
  | x == y = y:(uniq xs)
  | otherwise = y:(uniq (x:xs))

uniqCount :: Eq a => [a] -> [(a, Integer)]
uniqCount = map (\e -> (head e, toInteger $ length e)) . group

intersep :: a -> [a] -> [a]
intersep _ [] = []
intersep _ [x] = [x]
intersep sep (x:xs) = x:sep:(intersep sep xs)

andList :: [[Char]] -> [Char]
andList [] = ""
andList [x] = x
andList [x, y] = x ++ " and " ++ y
andList [x, y, z] = x ++ ", " ++ y ++ ", and " ++ z
andList (x:xs) = x ++ ", " ++ andList xs

triangle :: [a] -> [(a, a)]
triangle [] = []
triangle (x:xs) = [(x, x') | x' <- xs] ++ triangle xs

{- QuickCheck properties -}
prop_triangle_base = triangle ([] :: [Int]) == []
prop_triangle_one x = triangle [x] == []
prop_triangle_two x y = triangle [x, y] == [(x, y)]
prop_triangle_length xs =
  length (triangle xs) == n * (n - 1) `div` 2
  where n = length xs
prop_triangle_distinct xs =
  distinct xs ==> distinct (triangle xs)
  where distinct ys = nub ys == ys
prop_triangle_complete x xs y ys = (x, y) `elem` triangle (x : xs ++ y : ys)
prop_triangle_sound1 x y xs =
  not ((x, y) `elem` triangle (delete x (nub xs)))
  && not ((y, x) `elem` triangle (delete x (nub xs)))
prop_triangle_rec x xs =
  triangle (x : xs) == [(x, x') | x' <- xs] ++ triangle xs
