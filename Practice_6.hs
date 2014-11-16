module Exercise_6_Sol where
import Data.Char
import Data.List
import Data.List.Split (splitOn)

{-G6.1-}

{-
1. "(div 5) == (\x. div 5 x)", eine gecurryte Anwendung von "div". Also die
Funktion x |-> 5/x.

"(`div' 5) == (\x. x `div` 5) == (\x. div x 5)", eine Sektion. Also die Funktion
x |-> x/5.

2. "(+ 7) == (\x. x + 7)" (eine Sektion) vs. "((+) 7) == (\x. 7 + x)" (eine
gecurryte Anwendung). Beide sind gleich wenn "+" kommutativ ist.

3. "map (: []) :: [a] -> [[a]]" bildet die Liste "[x_1, ..., x_n]"
auf "[x_1 : [], ..., x_n : []]" ab, d.h. "[[x_1], ..., [x_n]]". Anders geschrieben:
"map (\x. [x])".

"map ([] :) :: [[[a]]] -> [[[a]]]" bildet die Liste "[x_1, ..., x_n]"
auf "[[] : x_1, ..., [] : x_n]" ab. Dafuer m\"ussen die "x"s Listen von Listen sein,
damit "[]" als Element vorne angehaengt werden kann.

4. "flip . flip :: (b -> a -> c) -> b -> a -> c" macht eigentlich nichts:
"flip (flip (>=)) == (>=)". D.h., sie ist die Identitaet, aber mit einem
beschraenkten Typ.

Anderseits hat "id :: a -> a" den allgemeinsten Typ fuer Identitaet. Man kann
beispielsweise "id 5" schreiben; "flip (flip 5)" geht nicht.

5. "[x, y, z]" ist eine dreielementige Liste, die "x :: a", "y :: a" und "z ::
a" enthält. "x : y : z" ist eine (2 oder mehr)-elementige Liste, mit "x :: a"
und "y :: a" vorne und der Teilliste "z :: [a]" hinten.
-}

{-G6.2-}

{-
1. "($)": Die Funktion nimmt zwei Argumente. Das heißt, der Typ muss von der Form

    a -> b -> c

  sein. Betrachten wir also

    f :: a
    x :: b

  Wegen dem Ausdruck "f z" gilt: f ist Funktion, der Typ des ersten Arguments ist der Typ von x:

    a = b -> c

  Der Rückgabetyp von "$" ist der Typ von "f z", also c. Insgesamt erhalten wir:

    ($) :: (b -> c) -> b -> c


2. "foldl": Die Funktion nimmt drei Argumente. Das heißt, der Typ muss von der Form

    a -> b -> c -> d

  sein. Die erste Gleichung liefert das zweite Argument zurück. Daraus folgt dass
  "b = d", also

    a -> b -> c -> b

  Und das dritte Argument ist eine Liste, wegen des Mustervergleichs:

    a -> b -> [c] -> b

  Die zweite Gleichung gibt zwei Argumente an "f", also "a = d -> e -> f":

    (d -> e -> f) -> b -> [c] -> b

  Das erste Argument an "f" ist "z", vom Typ "b", also

    (b -> e -> f) -> b -> [c] -> b

  Das zweite Argument an "f" ist "x", vom Typ "c", also

    (b -> c -> f) -> b -> [c] -> b

  Und das Ergebnis von "f" wird als zweites Argument an "foldl" rekursiv
  übergeben, also müssen "f" und "b" gleich sein:

    (b -> c -> b) -> b -> [c] -> b

  Kanonisch:

    (a -> b -> a) -> a -> [b] -> a

  Schauen wir mal:

    $ ghci
    Prelude> :t foldl
    foldl :: (a -> b -> a) -> a -> [b] -> a


3. Nun zu "not . null". Zuerst schauen wir uns die Basistypen an.

    not :: Bool -> Bool
    null :: [d] -> Bool
    (.) :: (b -> c) -> (a -> b) -> a -> c

  (Es ist immer eine gute Idee, die Typvariablen umzubenennen, damit keine
  Clashes auftauchen.)

  "(.)" nimmt also offenbar zwei Funktionen als Argumente. Jetzt müssen wir nur
  noch einsetzen:

    b -> c  = Bool -> Bool
    a -> b  = [d]  -> Bool

  Daraus folgt direkt:

    b = Bool
    c = Bool
    a = [d]

  Der Rückgabetyp von "(.)" ist "a -> c". Einsetzen:

    [d] -> Bool

  Kanonisch:

    [a] -> Bool


4. "g": Wir haben (mit umbenannten Typvariablen):

    (.) :: (a -> b) -> (c -> a) -> c -> b
    map :: (d -> e) -> [d] -> [e]

  Aus dem Ausdruck "(.) map" erhalten wir

    a -> b = (d -> e) -> [d] -> [e]

  Der Funktionspfeil klammert implizit nach rechts, also ist das das gleiche wie

    a -> b = (d -> e) -> ([d] -> [e])

  und damit

    a = d -> e
    b = [d] -> [e]

  der Typ von g ist der Typ von "(.) map", also

    (c -> a) -> c -> a

  Setzen wir die Gleichungen für a und b ein, so landen wir bei dem endgültigen Typ

    (c -> d -> e) -> c -> [d] -> [e]


5. Kommen wir zu "oo". Statt "(.).(.)" können wir äquivalent auch "(.) (.) (.)"
  schreiben. Wir haben also drei Vorkommen von "(.)":

    (.) :: (a1 -> b1) -> (c1 -> a1) -> (c1 -> b1)   -- erstes (.)
    (.) :: (a2 -> b2) -> (c2 -> a2) -> (c2 -> b2)   -- zweites (.)
    (.) :: (a3 -> b3) -> (c3 -> a3) -> (c3 -> b3)   -- drittes (.)

  Aus dem ersten Argument ergibt sich

    a1 -> b1 = (a2 -> b2) -> (c2 -> a2) -> (c2 -> b2)

  und aus dem zweiten:

    c1 -> a1 = (a3 -> b3) -> (c3 -> a3) -> (c3 -> b3)

  Vereinfacht also

    a1 = a2 -> b2
    b1 = (c2 -> a2) -> (c2 -> b2)
    c1 = a3 -> b3
    a1 = (c3 -> a3) -> (c3 -> b3)

  Nehmen wir die beiden Gleichungen für a1 zusammen, so ergibt sich:

    a2 = c3 -> a3
    b2 = c3 -> b3

  Der Typ von (oo) ergibt sich jetzt aus dem Rückgabetyp des ersten (.)

    c1 -> b1

  nach dem Auflösen der Gleichungen:

    (a3 -> b3) -> (c2 -> a2) -> (c2 -> b2)              -- c1 und b1 eingesetzt
    (a3 -> b3) -> (c2 -> c3 -> a3) -> (c2 -> c3 -> b3)  -- a2 und b2 eingesetzt

  Wenn wir jetzt noch ein wenig schönere Variablennamen verwenden, erhalten wir:

    (a -> b) -> (c -> d -> a) -> (c -> d -> b)

2. "oo" komponiert eine zweisteillige Funktion "g" mit einer einstelligen "f". Die
expandierte Form ist

    oo f g x y = f (g x y)

Beispiel:

    absSum = abs `oo` (+)
    absSum 5 (-100) == 95
-}


{-G6.3-}

{- See sum_filter.cprf -}


{-G6.4-}

partition_fold :: (a -> Bool) -> [a] -> ([a], [a])
partition_fold p =
  foldr (\x (ts, fs) -> if p x then (x : ts, fs) else (ts, x : fs)) ([], [])
