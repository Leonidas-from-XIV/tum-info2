module Practice_3 where

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

incOccurence :: Eq a => a -> [(a, Integer)] -> [(a, Integer)]
incOccurence key [] = [(key, 1)]
incOccurence key ((k, v):xs)
  | key == k = (k, v + 1):xs
  | otherwise = (k, v):(incOccurence key xs)

uniqCount :: Eq a => [a] -> [(a, Integer)] -> [(a, Integer)]
uniqCount [] h = h
uniqCount [x] h = incOccurence x h
uniqCount (y:x:xs) h
  | x == y = (uniqCount xs (incOccurence y h))
  | otherwise = uniqCount (x:xs) h
