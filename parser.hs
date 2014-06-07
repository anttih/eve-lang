import Data.Char (isAlphaNum, isAlpha, isDigit, isSpace)
import Prelude hiding (seq)

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

instance Functor Parser where
  fmap f (Parser p) = (Parser newP) where
            newP cs = case p cs of
                        (Ok r s) -> Ok (f r) s
                        _ -> Fail


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

seq :: Parser a -> Parser a -> Parser [a]
seq p1 p2 = (Parser p) where
      p s = case parse p1 s of
              (Ok c1 rs) -> case parse p2 rs of
                             (Ok c2 rs2) -> Ok [c1, c2] rs2
                             Fail -> Fail
              Fail -> Fail

zeroMany :: Parser a -> Parser [a]
zeroMany p = (Parser pa) where
  pa init = loop [] init
  loop acc cs = case parse p cs of
                 Fail -> Ok (reverse acc) cs
                 (Ok c rest) -> loop (c:acc) rest

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

lift :: Parser a -> Parser [a]
lift p = fmap (\r -> [r]) p

numeric = charTest isDigit
alpha = charTest isAlpha
alphaNumeric = charTest isAlphaNum
whitespace = charTest isSpace 

symbolSeq = fmap concat $ seq (lift alpha) (zeroMany alphaNumeric)

data Ast = Symbol String deriving (Show)

symbol = fmap Symbol symbolSeq
