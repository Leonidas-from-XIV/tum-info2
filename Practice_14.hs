module Practice_14 where

import Data.List
import Data.Maybe
import Test.QuickCheck


{-G14.1-}

{-

1) endrekursiv: der letzte Aufruf ist 'prod', alle weiteren Berechnungen finden
   in den Argumenten statt
2) nicht endrekursiv: nach dem Aufruf von 'prod' muss noch eine Multiplikation
   durchgeführt werden
3) endrekursiv: der letzte Aufruf ist 'prod', ebenso wie bei 1). Das
   'if-then-else' ist hier nicht ausschlaggebend, weil die Bedingung vor dem
   rekursiven Aufruf berechnet wird. Nach dem Aufruf muss nichts weiter getan
   werden.

-}

{-G14.2-}

concat :: [[a]] -> [a]
concat = go []
    where go acc [] = reverse acc
          go acc (xs:xss) = go (reverse xs ++ acc) xss

{- Warum 'reverse'? Ohne 'reverse' müsste man hinten anhängen (acc ++ xs), was
 - aber quadratische Laufzeit erzeugt, denn die Laufzeit von '++' ist von der
 - Länge des ersten Arguments abhängig. -}

{-G14.3-}

{-
 - map (*2) (1 : threes) !! 1
 - = ((*2) 1 : map (*2) threes) !! 1
 - = map (*2) threes !! (1-1)
 - = map (*2) (3 : threes) !! (1-1)
 - = ((*2) 3 : map (*2) threes) !! (1-1)
 - = ((*2) 3 : map (*2) threes) !! 0
 - = (*2) 3
 - = 3*2
 - = 6
 -
 -
 - (\f -> \x -> x + f 2) (\y -> y * 2) (3 + 1)
 - = (\x -> x + (\y -> y * 2) 2) (3 + 1)
 - = (3 + 1) + (\y -> y * 2) 2
 - = 4 + (\y -> y * 2) 2
 - = 4 + 2 * 2
 - = 4 + 4
 - = 8
 -
 - head (filter (/=3) threes)
 - head (filter (/=3) (3 : threes))
 - head (filter (/=3) threes)
 -
 -  --> terminiert nicht
 -
 -
 -}


{-G14.4-}

{- Richtige Implementierungen -}

uniqueElems_good1 :: Eq a => [a] -> [a]
uniqueElems_good1 = nub

uniqueElems_good2 :: Eq a => [a] -> [a]
uniqueElems_good2 = reverse . nub

uniqueElems_good3 :: Eq a => [a] -> [a]
uniqueElems_good3 [] = []
uniqueElems_good3 (x : xs) =
  if x `elem` xs then uniqueElems_good3 (reverse xs)
  else x : uniqueElems_good3 xs

{- Falsche Implementierungen -}

uniqueElems_bad1 :: Eq a => [a] -> [a]
uniqueElems_bad1 xs = xs

uniqueElems_bad2 :: Eq a => [a] -> [a]
uniqueElems_bad2 xs = xs

uniqueElems_bad3 :: Eq a => [a] -> [a]
uniqueElems_bad3 [] = []
uniqueElems_bad3 (x : xs) = x : uniqueElems_bad3 (delete x xs)

uniqueElems_bad4 :: Eq a => [a] -> [a]
uniqueElems_bad4 [] = []
uniqueElems_bad4 (_ : xs) = nub xs

{- Implementierung unter Test -}

uniqueElems :: Eq a => [a] -> [a]
uniqueElems = uniqueElems_bad4

{-
   Anmerkung: Tests sollten von einem konkret festgelegten Typ sein (unten z.B. Int):
   Die Standard-Instantiierung von Eq a durch QuickCheck ist () -- wenig hilfreich
-}

{- Erste vollstaendige Testsuite

   Diese basiert auf eine Musterloesung. Im Allgemeinen ist dies eher
   suboptimal, weil nichts guarantiert, dass die Musterloesung richtig ist.
   Hier ist es in Ordung, weil wenn wir uns nich auf "nub" verlassen können,
   koennen wir auch davon ausgehen, dass "ghc" ganz falsch ist.
-}

prop_uniqueElems_vsMuster :: [Int] -> Bool
prop_uniqueElems_vsMuster xs =
  sort (uniqueElems xs) == sort (uniqueElems_good1 xs)


{- Zweite vollstaendige Testsuite

   Diese ist theoretisch besser, weil sie nur über abstrakte Eigenschaften
   spricht, die ganz offensichtlich gelten müssen.

   "Sound" heißt: die Ergebnisse sind korrekt (d.h. die Elemente, die in der
   Ausgabenliste stehen, gehören darin).

   "Complete" heißt: die Ergebnisse sind vollständig (d.h. alle Elemente, die
   in der Ausgabenliste stehen sollten, sind da).

   Am Ende überprüfen wir, dass das Ergebnis duplikatfrei ist. -}

prop_uniqueElems_sound :: [Int] -> Bool
prop_uniqueElems_sound xs = all (`elem` xs) (uniqueElems xs)

prop_uniqueElems_complete :: [Int] -> Bool
prop_uniqueElems_complete xs = all (`elem` uniqueElems xs) xs

prop_uniqueElems_noDups :: [Int] -> Bool
prop_uniqueElems_noDups xs =
  length (nub xs') == length xs' where xs' = uniqueElems xs
