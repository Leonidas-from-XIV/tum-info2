module Exercise_8_Sol where

import Data.List (intercalate)
import System.Random

import Game

{- Library -- nicht veraendern -}
data Tree a =
  Empty |
  Node a (Tree a) (Tree a)
    deriving (Show, Eq)

insert :: Ord a => a -> Tree a -> Tree a
insert x Empty = Node x Empty Empty
insert x (Node a l r)
  | x < a = Node a (insert x l) r
  | x > a = Node a l (insert x r)
  | otherwise = Node a l r

delete :: Ord a => a -> Tree a -> Tree a
delete _ Empty = Empty
delete x (Node a l r)
  | x == a     =  combine l r
  | x < a      =  Node a (delete x l) r
  | otherwise  =  Node a l (delete x r)

find :: Ord a => a -> Tree a -> Bool
find _ Empty  =  False
find x (Node a l r)
  | x == a     =  True
  | x < a      =  find x l
  | otherwise  =  find x r

combine :: Tree a -> Tree a -> Tree a
combine Empty r  =  r
combine l Empty  =  l
combine l r      =  Node m l r'  where (m,r') = delL r

delL :: Tree a -> (a, Tree a)
delL (Node a Empty r)  = (a, r)
delL (Node a l     r)  = (m, Node a l' r)  where (m,l') = delL l
{- End Library -}

{-G8.1-}

data Direction = L | R
  deriving (Show, Eq)

navigate :: [Direction] -> Tree a -> Maybe (Tree a)
navigate []       t            = Just t
navigate (L : ds) (Node _ l _) = navigate ds l
navigate (R : ds) (Node _ _ r) = navigate ds r
navigate _        _            = Nothing

{- Die gesuchte Datenstruktur ist ein sogenannter "Zipper".
Man wuerde dann nach dem Absteigen in einem Baum nicht die
Liste aller Parents speichern, sondern nur die *Werte* der
Parents und die zugehoerigen Geschwister.

Siehe auch <http://learnyouahaskell.com/zippers>, die
Definition von `Crumb` und `Breadcrumbs`.
-}


{-G8.2-}

isOrderedTree :: Ord a => Tree a -> Bool
isOrderedTree = rec Nothing Nothing
  where checkMin Nothing _ = True
        checkMin (Just min) a = min < a
        checkMax Nothing _ = True
        checkMax (Just max) a = a < max

        rec _ _ Empty = True
        rec min max (Node a l r) =
          checkMin min a && checkMax max a &&
          rec min (Just a) l && rec (Just a) max r

flat :: Tree a -> [a]
flat Empty = []
flat (Node a l r) = flat l ++ a : flat r

treeFromList :: Ord a => [a] -> Tree a
treeFromList = foldl (flip insert) Empty

treeSort :: Ord a => [a] -> [a]
treeSort = flat . treeFromList

{-
Die Funktion `insert` steigt in dem Baum so bis zu einem Blatt ab, dass alle
Knoten links des Blattes kleiner und alle Knoten rechts des Blattes größer
sind.

Neben `insert` erhaelt auch `delete` die Suchbaumeigenschaft.
`find` benoetigt die Eigenschaft.

Betrachten wir auch die Hilfsfunktion `combine` und `delL`, so erhaelt `delL`
die Eigenschaft (wenn es definiert ist). `combine` dagegen erhaelt die
Eigenschaft im Allgemeinen nicht.
-}

isSorted :: Ord a => [a] -> Bool
isSorted (x1 : x2 : xs) = x1 < x2 && isSorted (x2 : xs)
isSorted _ = True

prop_treeSort_sorted :: [Int] -> Bool
prop_treeSort_sorted =
  isSorted . treeSort

prop_treeSort_elems :: [Int] -> Bool
prop_treeSort_elems xs =
  all (`elem` sorted) xs
  where sorted = treeSort xs


{-G8.3-}

data NonEmptyList a =
  Single a |
  Cons a (NonEmptyList a)
  deriving (Show, Eq)

toList :: NonEmptyList a -> [a]
toList (Single a) = [a]
toList (Cons h t) = h : toList t

fromList [] = Nothing
fromList (x : xs) = Just (go x xs)
  where go x [] = Single x
        go x (y : ys) = Cons x (go y ys)

nHead :: NonEmptyList a -> a
nHead (Single a) = a
nHead (Cons a _) = a

nTail :: NonEmptyList a -> [a]
nTail (Single _) = []
nTail (Cons _ t) = toList t

nAppend :: NonEmptyList a -> NonEmptyList a -> NonEmptyList a
nAppend (Single a) xs = Cons a xs
nAppend (Cons h t) xs = h `Cons` nAppend t xs
