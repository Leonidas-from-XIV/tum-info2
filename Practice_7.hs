module Exercise_7_Sol where

import Data.Bits ((.&.), (.|.), complement)
import Data.Function (on)
import Data.List
import Data.Maybe (mapMaybe)
import Data.Ord (comparing)
import Data.Ratio (numerator, denominator)
import Test.QuickCheck (Arbitrary, arbitrary, suchThat, (==>))

{- Library DO NOT CHANGE -}
toBinary :: Integral a => a -> String
toBinary 0 = "0"
toBinary x = reverse (aux x)
  where aux 0 = []
        aux x = (if x `mod` 2 == 0 then '0' else '1') : aux (x `div` 2)
        
instance Show IntegerBitSet where
  show (IntegerBitSet s) = "IntegerBitSet " ++ toBinary s
{- End Library -}

{-G7.1-}

data Fraction = Over Integer Integer

instance Show Fraction where
  show (a `Over` b) =
    if a == 0 then
      "0"
    else
      minus ++ show (abs a) ++ "/" ++ show (abs b)
    where minus = if (a < 0) /= (b < 0) then "-" else ""

-- also possible:
-- data Fraction = Over Integer Integer deriving Show

norm :: Fraction -> Fraction
norm (Over a b) = (a `div` c) `Over` (b `div` c)
  where c = gcd a b * (if b < 0 then -1 else 1)

instance Num Fraction where
  (a1 `Over` b1) + (a2 `Over` b2) = norm $ (a1*b2 + a2*b1) `Over` (b1 * b2)
  (a1 `Over` b1) - (a2 `Over` b2) = norm $ (a1*b2 - a2*b1) `Over` (b1 * b2)
  (a1 `Over` b1) * (a2 `Over` b2) = norm $ (a1 * a2) `Over` (b1 * b2)
  negate (a `Over` b) = negate a `Over` b
  fromInteger n = n `Over` 1
  abs (a `Over` b) = abs a `Over` abs b
  signum (a `Over` b) = (signum a * signum b) `Over` 1

instance Eq Fraction where
  (a1 `Over` b1) == (a2 `Over` b2) = a1*b2 == a2*b1

instance Fractional Fraction where
  recip (a `Over` b) = (b `Over` a)
  fromRational r = numerator r `Over` denominator r

-- Tests

prop_abs_signum x y = y /= 0 ==>
  abs frac * signum frac == frac
  where frac = Over x y

-- We can also define an instance of `Arbitrary`

instance Arbitrary Fraction where
  arbitrary = do
    num <- arbitrary
    den <- arbitrary `suchThat` (/= 0)
    return $ num `Over` den

prop_abs_signum' frac =
  abs frac * signum frac == (frac :: Fraction)

prop_minus_consistent frac1 frac2 =
  frac1 - frac2 == frac1 + (negate frac2 :: Fraction)

prop_plus_comm frac1 frac2 =
  frac1 + frac2 == frac2 + (frac1 :: Fraction)

prop_times_comm frac1 frac2 =
  frac1 * frac2 == frac2 * (frac1 :: Fraction)

-- ... and countless other (ring) laws


{-G7.2-}

f1 xs = map (\x -> x + 1) xs
f2 xs = map (\x -> 2 * x) (map (\x -> x + 1) xs)
f3 xs = filter (\x -> x > 1) (map (\x -> x + 1) xs)
f4 f g x = f (g x)
f5 f g x y = f (g x y)
f6 f g x y z = f (g x y z)
f7 f g h x = g (h (f x))

f1' = map (+1)

f2' = map (2*) . map (+1)
f2'' = map ((2*) . (+1))

f3' = filter (>1) . map (+1)

f4' f g = f . g
f4'' f = (.) f
f4''' = (.)

f5' f g x = f . g x
f5'' f g = ((.).(.)) f g
f5''' f = ((.).(.)) f
f5'''' = (.).(.)  -- vgl. "oo"

f6' f g x y = f . g x y
f6'' f g x = (.) f . g x
f6''' f g = ((.).(.)) f . g
f6'''' f g = ((.).(.).(.)) f g
f6''''' f = ((.).(.).(.)) f
f6'''''' = (.).(.).(.)


f7' f g h = g . h . f
-- So weit, so gut. Vernünftigerweise hört man hier wohl auf. Das heißt aber
-- nicht, dass man das nicht noch weiter treiben kann ...
f7_2 f g h = (.) (g . h) f
f7_3 f g h = flip (.) f (g . h)
f7_4 f g h = flip (.) f ((.) g h)
f7_5 f = flip (.) f `oo` (.)
  where oo = (.).(.)
f7_6 f = ((.).(.)) (flip (.) f) (.)
f7_7 f = flip ((.).(.)) (.) (flip (.) f)
f7_8 = flip ((.).(.)) (.) . flip (.)
-- Jetzt haben wir alle Parameter eliminiert. Ein solcher Ausdruck
-- heißt 'point-free' (da keine expliziten "Punkte" (== Werte) mehr
-- vorkommen). Solche extremen Anwendungen werden in der Haskell-Welt
-- auch gerne mal als 'pointless' bezeichnet ...
--
-- Wer noch nicht genug hat, darf jetzt überlegen, warum wir die
-- Definition noch zu Folgendem vereinfachen dürfen:
f7_9 = flip ((.).(.)) . flip (.)

-- f7_9 f g h x
-- = (flip oo . flip (.)) f g h x
-- = (flip oo (flip (.) f)) g h x
-- = flip oo (. f) g h x
-- = (\y -> oo y (. f)) g h x
-- = oo g (. f) h x
-- = (\a b c d -> a (b c d)) g (. f) h x
-- = g ((. f) h x)
-- = g ((h . f) x)
-- = g (h (f x))

-- f7_8 f g h x
-- = (flip oo (.) . flip (.)) f g h x
-- = flip oo (.) (flip (.) f) g h x
-- = flip oo (.) (. f) g h x
-- = (\y z -> oo z y) (.) (. f) g h x
-- = oo (. f) (.) g h x
-- = (\a b c d -> a (b c d)) (. f) (.) g h x
-- = (. f) ((.)g h) x
-- = (. f) (g . h) x
-- = (g . h . f) x
-- = g (h (f x))


{-G7.3-}

data Shape =
  Circle Integer |
  Rectangle Integer Integer
  deriving (Show, Eq)

isValid :: Shape -> Bool
isValid (Circle r) = r >= 0
isValid (Rectangle h w) = h >= 0 && w >= 0

scale :: Integer -> Shape -> Shape
scale n (Circle r) = Circle (r * n)
scale n (Rectangle h w) = Rectangle (h * n) (w * n)

-- We represent triangles via the lengths of all sides.
-- There are other ways though, e.g. the lengths of two sides and
-- the angle between them. For other representations, `isValid` and
-- `scale` look differently.

data Shape' =
  Circle' Integer |
  Rectangle' Integer Integer |
  Triangle' Integer Integer Integer
  deriving (Show, Eq)

isValid' :: Shape' -> Bool
isValid' (Circle' r) = r >= 0
isValid' (Rectangle' h w) = h >= 0 && w >= 0
isValid' (Triangle' a b c) = a + b >= c && a + c >= b && b + c >= a

scale' :: Integer -> Shape' -> Shape'
scale' n (Circle' r) = Circle' (r * n)
scale' n (Rectangle' h w) = Rectangle' (h * n) (w * n)
scale' n (Triangle' a b c) = Triangle' (a * n) (b * n) (c * n)
