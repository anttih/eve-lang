module Parser (
  Result(Ok, Fail),
  List'(Pair, Null),
  Sexpr(Symbol, Str, Number, List, LispMap),
  concat,
  readSexpr) where

import Prelude hiding (seq, either, negate, True, False, foldr, takeWhile, concat)
import Data.Char (isAlphaNum, isAlpha, isDigit, isSpace)
import Data.Traversable
import Control.Applicative
import Control.Monad (liftM, ap)
import Data.Foldable (Foldable(foldr))
import qualified Data.Map as M


data Result a = Ok a String | Fail deriving (Show)

instance Monad Result where
  Fail >>= _ = Fail
  (Ok a _) >>= f = f a
  return a = Ok a ""

instance Functor Result where
  fmap = liftM

instance Applicative Result where
  pure = return
  (<*>) = ap

data Parser a = Parser { parse :: String -> Result a } 

instance Monad Parser where
  (Parser p) >>= f = Parser p2 where
             p2 cs = case p cs
                     of Fail -> Fail
                        (Ok a rs) -> parse (f a) rs
  return v = Parser (Ok v)

instance Functor Parser where
  fmap f (Parser p) = Parser newP where
            newP cs = case p cs of
                        (Ok r s) -> Ok (f r) s
                        _ -> Fail


instance Applicative Parser where
  pure = return
  (<*>) = ap

both :: Parser a -> Parser a -> Parser a
both p1 p2 = Parser p where
      p cs = case parse p1 cs
             of Fail -> Fail
                (Ok _ _) -> parse p2 cs

either :: Parser a -> Parser a -> Parser a
either p1 p2 = Parser p where
         p s = case parse p1 s
               of Fail -> parse p2 s
                  ok -> ok

seq :: Parser a -> Parser b -> Parser (a, b)
seq p1 p2 = Parser p where
      p s = case parse p1 s of
              (Ok c1 rs) -> case parse p2 rs of
                             (Ok c2 rs2) -> Ok (c1, c2) rs2
                             Fail -> Fail
              Fail -> Fail

zeroMany :: Parser a -> Parser [a]
zeroMany p = Parser pa where
  pa = loop []
  loop acc cs = case parse p cs of
                 Fail -> Ok (reverse acc) cs
                 (Ok c rest) -> loop (c:acc) rest

oneMany :: Parser a -> Parser [a]
oneMany p = do
    x <- p
    xs <- zeroMany p
    return (x:xs)

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

numeric ::  Parser Char
numeric = charTest isDigit

alpha ::  Parser Char
alpha = charTest isAlpha

alphaNumeric ::  Parser Char
alphaNumeric = charTest isAlphaNum

space ::  Parser Char
space = charTest isSpace

whitespace ::  Parser String
whitespace = zeroMany space

char ::  Char -> Parser Char
char c = charTest (== c)

notChar ::  Char -> Parser Char
notChar c = charTest $ not . (== c)

token ::  Parser b -> Parser b
token p = snd <$> seq whitespace p

data List' a = Pair a (List' a) | Null deriving (Show, Ord, Eq)

concat :: List' a -> List' a -> List' a
concat Null ys = ys
concat (Pair x xs) ys = Pair x (concat xs ys)

instance Foldable List' where
  foldr _ z Null = z
  foldr f z (Pair first rest) = f first (foldr f z rest)

instance Functor List' where
  fmap _ Null = Null
  fmap f (Pair x xs)  = Pair (f x) (fmap f xs)

instance Monad List' where
  return x = Pair x Null
  m >>= f = foldr (concat . f) Null m

instance Applicative List' where
  pure = return
  (<*>) = ap

instance Traversable List' where
  -- traverse f Null = pure Null
  -- traverse f (Pair x xs) = Pair (f x) $ traverse f xs
  traverse f = foldr cons_f (pure Null) where
    cons_f x ys = Pair <$> f x <*> ys

-- takeWhile :: (a -> Bool) -> List' a -> List' a
-- takeWhile _ Null = Null
-- takeWhile f (Pair x xs)
--   | f x       = Pair x (takeWhile f xs)
--   | otherwise = Null

data Sexpr = Symbol String
             | True
             | False
             | Keyword String
             | Str String
             | Number Int
             | List (List' Sexpr)
             | LispMap (M.Map Sexpr Sexpr)
             deriving (Show, Ord, Eq)

symbolSpecial ::  Parser Char
symbolSpecial = charTest $ \c -> c `elem` "+-/=><*!?"

symbolSeq ::  Parser String
symbolSeq = do first <- either alpha symbolSpecial
               rest <- zeroMany alphaNumeric
               return (first:rest)

symbol ::  Parser Sexpr
symbol = Symbol <$> token symbolSeq

identifier ::  String -> Parser Sexpr
identifier name = do (Symbol sym) <- symbol
                     Parser $ equals sym where
  equals sym cs = if sym == name
                  then Ok (Symbol sym) cs
                  else Fail

boolean ::  Parser Sexpr
boolean = either (fmap (const True) (identifier "true"))
                 (fmap (const False) (identifier "false"))


keyword ::  Parser Sexpr
keyword = do _ <- char ':'
             str <- symbolSeq
             return (Keyword str)

string ::  Parser Sexpr
string = do _ <- quote
            xs <- zeroMany stringChar
            _ <- quote
            return (Str xs) where
  quote = char '"'
  stringChar = notChar '"'

number :: Parser Sexpr
number = Number . read <$> oneMany numeric where

cons ::  a -> List' a -> List' a
--cons :: (Sexpr a) -> List' b -> List' a
cons = Pair

list ::  Parser Sexpr
list = do _ <- token $ char '('
          xs <- zeroMany expr
          _ <- token $ char ')'
          return $ List (foldr cons Null xs)

lispMap ::  Parser Sexpr
lispMap = do _ <- token $ char '{'
             xs <- zeroMany (seq keyword expr)
             _ <- token $ char '}'
             return $ LispMap (M.fromList xs)

expr ::  Parser Sexpr
expr =  boolean `either` string `either` number `either` symbol `either` keyword  `either` list `either` lispMap

-- @todo implement instance Read
readSexpr :: String -> Result Sexpr
readSexpr = parse expr

