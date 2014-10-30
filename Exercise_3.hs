module Exercise_3 where

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

prop_padNumbers padNumbers xs = undefined :: Bool

{-H3.2-}

wrapText :: Integer -> [String] -> String
wrapText = undefined

{-H3.3-}

stretch :: Picture -> Picture
stretch = undefined

{-H3.4-}

coalesce :: [(String, Integer)] -> [(String, Integer)]
coalesce = undefined
