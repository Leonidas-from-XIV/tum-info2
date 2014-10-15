module Exercise_1 where

import Test.QuickCheck
import Data.List

{-H1.1-}

{-WETT-}
allDistinct :: Integer -> Integer -> Integer -> Integer -> Integer -> Bool
allDistinct a b c d e = nub l == l
  where l = [a, b, c, d, e]
{-TTEW-}

{-H1.2-}

f :: Integer -> Integer -> Integer
f 0 _ = 0
f _ 0 = 0
f m n | m < 0 || n < 0 = 0
f m n = 1 + f (f (n - 1) m) (f (m - 1) n)

-- Ersetzen Sie hier das „True” durch die Ihre vermutete Eigenschaft.
prop_f :: Integer -> Integer -> Property
prop_f m n = m >= 0 ==> n >= 0 ==> f m n == min m n

{-H1.3-}

isPower :: Integer -> Integer -> Bool
isPower x a = any (\i -> a ^ i == x) [0..x]
