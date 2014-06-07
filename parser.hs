import Data.Char (isAlphaNum, isAlpha, isDigit, isSpace)

data Result a = Ok a String | Fail deriving (Show)

instance Monad Result where
  Fail >>= _ = Fail
  (Ok a _) >>= f = (f a)
  return a = Ok a ""

data Parser a = Parser (String -> Result a)

parse (Parser f) cs = (f cs)

instance Monad Parser where
  (Parser p) >>= f = Parser p2 where
             p2 cs = case p cs
                     of Fail -> Fail
                        (Ok a rs) -> parse (f a) rs
  return v = Parser p where
       p _ = Ok v ""

both :: Parser a -> Parser a -> Parser a
both p1 p2 = Parser p where
      p cs = case parse p1 cs
             of Fail -> Fail
                (Ok m rest) -> parse p2 cs

either :: Parser a -> Parser a -> Parser a
either p1 p2 = (Parser p) where
         p s = case parse p1 s
               of Fail -> parse p2 s
                  ok -> ok

notEmpty :: Parser Char
notEmpty = Parser p where
     p s = case s
           of [] -> Fail
              (fs:rest) -> Ok fs rest

charTest :: (Char -> Bool) -> Parser Char
charTest f = both notEmpty (Parser p) where
       p s = if not $ f $ head s
             then Fail
             else Ok (head s) (tail s)

numeric = charTest isDigit
alpha = charTest isAlpha
alphaNumeric = charTest isAlphaNum
whitespace = charTest isSpace 
