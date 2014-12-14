module Practice_10 where

import System.Random (randomRIO)
import Text.Read (readMaybe)

nRandomR' :: (Int, Int) -> [Int] -> Int -> IO [Int]
nRandomR' _ _ 0 = return []
nRandomR' range seen n = do
  num <- randomRIO range
  if num `notElem` seen then do
    tail <- nRandomR' range (num:seen) (n-1)
    return (num:tail)
   else nRandomR' range seen n

nRandomR :: (Int, Int) -> Int -> IO [Int]
nRandomR range amount = nRandomR' range [] amount

getLineInt :: IO Int
getLineInt = do
  user <- getLine
  case readMaybe user of
    Just x -> if x >= 0 then return x else do
      putStrLn "Not a positive number"
      getLineInt
    Nothing -> do
      putStrLn "Not a number"
      getLineInt

guessNum' :: Int -> Int -> IO Int
guessNum' tries lookingFor = do
  putStrLn "Specify a number"
  num <- getLineInt
  case num `compare` lookingFor of
    EQ -> return tries
    LT -> do
      putStrLn "Too small"
      putStrLn $ show lookingFor
      guessNum' (tries + 1) lookingFor
    GT -> do
      putStrLn "Too large"
      putStrLn $ show lookingFor
      guessNum' (tries + 1) lookingFor

guessNum :: IO Int
guessNum = do
  lookingFor <- randomRIO (0, 100)
  guessNum' 1 lookingFor
