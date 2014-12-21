module Practice_11 where

import Data.List
import Network
import System.IO

{-G11.1-}

{- Siehe AssocList_Sol.hs -}
module AssocList_Sol (
    Map, empty, insert, lookup, delete, keys,
    {- für Teilaufgabe 2 -} kvList, fold) where

import Prelude hiding (lookup)

newtype Map k v = AL [(k,v)]

empty :: Eq k => Map k v
empty = AL []

insert :: Eq k => k -> v -> Map k v -> Map k v
insert k v (AL al) = AL $ins al
  where
    ins [] = [(k,v)]
    ins ((k',v') : m)
        | k == k' = (k,v) : m
        | otherwise = (k',v') : ins m

lookup :: Eq k => k -> Map k v -> Maybe v
lookup k (AL al) = lo al
  where
    lo [] = Nothing
    lo ((k',v') : m)
        | k == k' = Just v'
        | otherwise = lo m

delete :: Eq k => k -> Map k v -> Map k v
delete k (AL al) = AL $ del al
  where
    del [] = []
    del ((k',v') : m)
        | k == k' = m
        | otherwise = (k',v') : del m

keys :: Eq k => Map k v -> [k]
keys (AL al) = map fst al

kvList :: Eq k => Map k v -> [(k,v)]
kvList (AL al) = al

fold :: Eq k => (k -> v -> a -> a) -> a -> Map k v -> a
fold f s (AL al) = foldr (\(k,v) -> f k v) s al

{-G11.2-}

module Queue_Sol (Queue, empty, isEmpty, enqueue, dequeue, toList) where

data Queue a = Queue [a] [a]
    deriving Show

empty :: Queue a
empty = Queue [] []

isEmpty :: Queue a -> Bool
isEmpty (Queue [] []) = True
isEmpty _ = False

enqueue :: a -> Queue a -> Queue a
enqueue a (Queue hs ts) = Queue hs (a : ts)

dequeue :: Queue a -> Maybe (a, Queue a)
dequeue (Queue [] []) = Nothing
dequeue (Queue [] ts) = dequeue $ Queue (reverse ts) []
dequeue (Queue (h:hs) ts) =  Just (h, Queue hs ts)

toList :: Queue a -> [a]
toList (Queue hs ts) = hs ++ reverse ts

{-G11.3-}

vocabs = ["i", "more", "more", "now", "want", "won", "wow"]

quasiIdentical :: String -> String -> Bool
quasiIdentical [] [] = False
quasiIdentical (c : cs) (d : ds)
    | c == d = quasiIdentical cs ds
    | otherwise = cs == ds
quasiIdentical _ _ = False

fixTypo :: [String] -> String -> Either [String] String
fixTypo vocabs word = 
    if word `elem` vocabs then Right word else Left quasis
  where quasis = nub (filter (quasiIdentical word) vocabs)
