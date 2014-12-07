module Exercise_9_Sol where

{- Library DO NOT CHANGE -}

infixl 6 :+:
infixl 7 :*:

data Arith = Literal Integer
           | Var String
           | Arith :+: Arith
           | Arith :*: Arith
  deriving (Show, Eq)

{- End Library -}

{-G9.1-}

-- siehe Form_9_Sol.hs

{-G9.2-}

data Html = Elem String [Html] | Text String

startTag name = "<" ++ name ++ ">"
endTag name = "</" ++ name ++ ">"

htmlShow :: Html -> String
htmlShow (Text s) = s
htmlShow (Elem name es) = startTag name ++ concatMap htmlShow es ++ endTag name


escape :: String -> String
escape = concatMap f
  where
    f x | x == '<' = "&lt;"
        | x == '>' = "&gt;"
        | x == '&' = "&amp;"
        | otherwise = [x]

htmlShow' :: Html -> String
htmlShow' (Text s) = escape s
htmlShow' (Elem name es) = startTag name ++ concatMap htmlShow' es ++ endTag name
  where
    start = "<" ++ name ++ ">"
    end = "</" ++ name ++ ">"
