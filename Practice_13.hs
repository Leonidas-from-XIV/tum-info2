module Practice_13 where

import Data.Function
import Data.Maybe
import Data.List
import Test.QuickCheck

{-G13.1-}

{- Ausdruck: 1 + (2 * 3)
 -   2 * 3                      [Innermost + Outermost]
 -
 - "+" als primitive Operation kann nur auf vollständig ausgewerteten
 - Argumenten reduziert werden, daher kann 1 + (2 * 3) nicht als ganzes
 - reduziert werden.
 -
 -
 - Ausdruck: (1 + 2) * (2 + 3)
 -   1 + 2                      [Innermost + Outermost]
 -   2 + 3                      [Innermost + Outermost]
 -
 -
 - Ausdruck: fst (1 + 2, 2 + 3)
 -   1 + 2                      [Innermost]
 -   2 + 3                      [Innermost]
 -   fst (1 + 2, 2 + 3)         [Outermost]
 -
 -
 - Ausdruck: fst (snd (1, 2 + 3), 4)
 -   2 + 3                      [Innermost]
 -   snd (1, 2 + 3)             [weder noch]
 -   fst (snd (1, 2 + 3), 4)    [outermost]
 -
 -
 - Ausdruck: (\x -> 1 + x) (2 * 3)
 -   2 * 3                      [Innermost]
 -   (\x -> 1 + x) (2 * 3)        [Outermost]
 -
 - Hinweis: Generell kann unter Lambda-Ausdrücken nicht reduziert werden,
 - so hat z.B. der Ausdruck
 -
 -   (\x -> (1 + 2) + x) 3
 -
 - nur einen einzigen Redex und
 -
 -   (\x -> (1 + 2) + x)
 -
 - gar keinen.
 -}


{-G13.2-}

{- Aufruf:
 -    f (inf (1+0)) (inf (1+1)) (inf (1+2))
 - Argumente
 -   inf (1+0) ~~> 1 : 1 : inf 1
 -   inf (1+1) ~~> 2 : inf 2
 -   inf (1+2) ~~> (1+2) : inf (1+2)
 - Warum?
 -  Zur Auswertung von f wird zunächst Pattern Matching gegen die erste
 -  Regel gemacht. Haskell muss also feststellen, ob das erste und zweite
 -  Argument die Form _:_ (statt []) hat. Das dritte Argument ist nur eine
 -  Variable, hier muss also noch nichts ausgewertet werden.
 -    inf (1+0) ~> (1+0) : inf (1+0)
 -    inf (1+1) ~> (1+1) : inf (1+1)
 -    inf (1+2) ~> inf(1+2)
 -  Jetzt wird (unter anderem) x = 1+0 und y = 1+1 gebunden.
 -  In einem zweiten Schritt muss "x > y" getestet werden, dazu müssen x und y
 -  ausgewertet werden:
 -    (1+0) : inf (1+0) ~> 1 : inf 1
 -    (1+1) : inf (1+1) ~> 2 : inf 2
 -    inf (1+2) ~> inf(1+2)
 -  Warum wurden jetzt beide 1+0 zu 1 (bzw. beide 1+1 zu 2 ausgewertet)? Weil es
 -  jeweils der selbe (nicht nur der gleiche Ausdruck war) ~> Sharing.
 -
 -  "x > y" ist False, deswegen muss jetzt noch die zweite Regel getestet werden.
 -  Dafür ist es notwendig, das erste Argument zu der Form _:_:_ und das dritte
 -  Argument zu der Form _:_ auszuwerten:
 -    1 : inf 1 ~> 1 : 1 : inf 1
 -    2 : inf 2 ~> 2 : inf 2
 -    inf (1+2) ~> (1+2) : inf (1+2)
 -  Der Rückgabewert ist jetzt 1, was schon vollständig ausgewertet ist und daher
 -  für die Anzeige nicht weiter ausgewertet werden muss.
 -}

{- Aufruf:
 -   f (inf(1+2)) (inf(1+1)) (inf(1+0))
 - Argumente:
 -   inf (1+2) ~> 3 : inf 3
 -   inf (1+1) ~> 2 : inf 2
 -   inf (1+0) ~> inf (1+0)
 - Ähnlich wie oben, nur schlägt hier der Test "x > y" nicht fehl, daher wird das
 - dritte Argument nie ausgewertet.
 -}

{- Aufruf
 -   f (inf (1+0)) [] (inf 0)
 - Argumente:
 -   inf (1+0) ~~> 1 : 1 : inf 1
 -   inf 0 ~~> 0 : inf 0
 - Warum?
 -  Pattern Matching auf erste Regel:
 -    inf (1+0) ~> 1 + 0 : inf (1 + 0)
 -    inf 0 ~> inf 0
 -  Hier wird der Test "x > y" nie erreicht (denn das zweite Argument hat die Form []).
 -
 -  Pattern Matching auf zweite Regel:
 -    1 + 0 : inf (1+0) ~> 1 + 0 : 1 + 0 : inf (1 + 0)
 -    inf 0 ~> 0 : inf 0
 -
 -  Um jetzt 1+0 ausgeben zu können, muss der Ausdruck noch ausgewertet werden:
 -    1+0 : 1+0 :  inf (1+0) ~> 1 : 1 : inf 1
 -    0 : inf 0 ~> 0 : inf 0
 -}



{-G13.3-}

fib1 = 0 : 1 : zipWith (+) fib1 (tail fib1)

{- Auswertung der ersten 5 Elemente von fib1:
 -
 - fib1
 - ~> 0 : 1 : zipWith (+) fib1 (tail fib1)
 - ~> 0 : 1 : zipWith (+) (0 : 1 : zipWith ...) (1 : zipWith ...)
 - ~> 0 : 1 : 0+1 : zipWith (+) (1 : 0+1 : zipWith ...) (0+1 : zipWith ...)
 - ~> 0 : 1 : 1 : zipWith (+) (1 : 1 : zipWith ...) (1 : zipWith ...)
 - ~> 0 : 1 : 1 : 1+1 : zipWith (+) (1 : 1+1 : zipWith ...) (1+1 : zipWith ...)
 - ~> 0 : 1 : 1 : 2 : zipWith (+) (1 : 2 : zipWith ...) (2 : zipWith ...)
 - ~> 0 : 1 : 1 : 2 : 1+2 : zipWith (+) (2 : 1+2 : zipWith ...) (1+2 : zipWith ...)
 - ~> 0 : 1 : 1 : 2 : 3 : zipWith (+) (2 : 3 : zipWith ...) (3 : zipWith ...)
 -}

fib2 x y = x : fib2 y (x + y)

{-
fib2 0 1
~> 0 : fib2 1 (0+1)
~> 0 : 1 : fib2 (0+1) ((0+1)+1)
~> 0 : 1 : (0+1) : fib2 ((0+1)+1) (((0+1)+1)+(0+1))
~> 0 : 1 : 1 : fib2 (1+1) ((1+1)+1)
~> 0 : 1 : 1 : 1+1 : fib2 ((1+1)+1) (((1+1)+1)+(1+1))
~> 0 : 1 : 1 : 2 : fib2 (2+1) ((2+1)+2)
-}

{- Um fib n auszuwerten, muss fib (n-1) 2-mal, fib (n - 2) 4-mal, fib (n-3)
 - 8-mal, ... ausgewertet werden. Zur Auswertung von fib1 !! n müssen nur die
 - ersten (n+1) Elemente von fib1 jeweils 1-mal ausgewertet werden.
 - Die Laufzeit ist also linear statt exponentiell.
 -}

{-G13.4-}

{- Für Interessierte: Stichwort "Hilberts Hotel" -}

mix :: [[a]] -> [a]
mix [] = []
mix (xs:xss) = aux xs (mix xss)
  where aux [] ys = ys
        aux (x:xs) ys = x : aux ys xs

prop_mix :: Integer -> Property
prop_mix n = n > 1 && n < 25 ==>
  n `elem` mix (map list [2..])
  where list k = map (*k) [1..]
