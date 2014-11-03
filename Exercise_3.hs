module Exercise_3 where

import Data.List (intercalate, genericLength)
import Test.QuickCheck

{- Library DO NOT CHANGE -}
type Picture = [[Char]]
type PadNumbersFn = [Integer] -> [String]

printPicture :: Picture -> IO ()
printPicture [] = return ()
printPicture (xs : xss) = do
  putStrLn xs
  printPicture xss

pic = [".##.", ".#.#", ".###", "####"]

export_prop_padNumbers :: PadNumbersFn -> [Integer] -> Property
export_prop_padNumbers f xs = property $ prop_padNumbers f xs
{- End Library -}


{-H3.1-}

-- Wichtiger Hinweis zum Template:
-- Bitte ändern Sie _nicht_ die Parameter, die 'prop_padNumbers' übernimmt, sonst kommt es höchstwahrscheinlich zu Typfehlern
-- Sie dürfen allerdings die gewöhnlichen QuickCheck-Operatoren benutzen, z.B. '==>'
-- Entfernen Sie außerdem vor Bearbeitung die Typannotation 'Bool'

prop_padNumbers padNumbers xs = length padded == length xs ==>
  all (\x -> length x == (length $ head padded)) padded
  where padded = padNumbers xs

{-H3.2-}

wrap :: Integer -> [[String]] -> String -> [[String]]
wrap n (line:lines) word = if genericLength (unwords (word:line)) <= n then (word:line):lines else [word]:line:lines

wrapText :: Integer -> [String] -> String
wrapText n ws = intercalate "\n" $ reverse $ map (unwords . reverse) $ foldl (wrap n) [[]] ws

{-H3.3-}

stretch :: Picture -> Picture
stretch = undefined

{-H3.4-}

coalesce :: [(String, Integer)] -> [(String, Integer)]
coalesce = undefined
