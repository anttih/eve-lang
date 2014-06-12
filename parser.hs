import Prelude hiding (seq, either, negate, True, False)
import Data.Char (isAlphaNum, isAlpha, isDigit, isSpace)
import qualified Data.Map as M

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
       p cs = Ok v cs

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

seq :: Parser a -> Parser b -> Parser (a, b)
seq p1 p2 = (Parser p) where
      p s = case parse p1 s of
              (Ok c1 rs) -> case parse p2 rs of
                             (Ok c2 rs2) -> Ok (c1, c2) rs2
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

numeric = charTest isDigit
alpha = charTest isAlpha
alphaNumeric = charTest isAlphaNum
space = charTest isSpace 
whitespace = zeroMany space
char c = charTest $ (== c)
notChar c = charTest $ not . (== c)

second r = r !! 1

token p = fmap snd $ seq whitespace p

-- data LispMap' = MakeLispMap M.Map Sexpr Sexpr

data Sexpr = Symbol String | True | False | Keyword String | Str String | List [Sexpr] | LispMap (M.Map Sexpr Sexpr) deriving (Show, Ord, Eq)

symbolSpecial = charTest $ \c -> elem c "+-/=><*!?"
symbolSeq = do first <- either alpha symbolSpecial
               rest <- zeroMany alphaNumeric
               return (first:rest)

symbol = fmap Symbol $ token symbolSeq where

identifier name = do (Symbol sym) <- symbol
                     Parser $ equals sym where
  equals sym cs = if sym == name
                  then (Ok (Symbol sym) cs)
                  else Fail

boolean = either (fmap (const True) (identifier "true"))
                 (fmap (const False) (identifier "false"))


keyword = do token $ char ':'
             str <- symbolSeq
             return (Keyword str)

string = do token quote
            xs <- zeroMany stringChar
            quote
            return (Str xs) where
  quote = char '"'
  stringChar = notChar '"'

list = do token $ char '('
          xs <- zeroMany expr
          token $ char ')'
          return (List xs)

lispMap = do token $ char '{'
             xs <- zeroMany (seq keyword expr)
             token $ char '}'
             return $ LispMap (M.fromList xs)

expr =  boolean `either` string `either` symbol `either` keyword  `either` list `either` lispMap
