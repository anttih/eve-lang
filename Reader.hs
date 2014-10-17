module Reader (
  Result(Ok, Fail),
  readLispData
  ) where

import Prelude hiding (seq, either, negate, foldr, takeWhile, concat)
import Data.Char (isAlphaNum, isAlpha, isDigit, isSpace)
import Data.Foldable (Foldable(foldr))
import Control.Applicative
import Control.Monad (liftM, ap)
import qualified Data.Map as M

import Data

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

data Reader a = Reader { runReader :: String -> Result a }

instance Monad Reader where
  (Reader p) >>= f = Reader p2 where
    p2 cs = case p cs of
      Fail -> Fail
      (Ok a rs) -> runReader (f a) rs
  return v = Reader (Ok v)

instance Functor Reader where
  fmap f (Reader p) = Reader newP where
    newP cs = case p cs of
      (Ok r s) -> Ok (f r) s
      _ -> Fail

instance Applicative Reader where
  pure = return
  (<*>) = ap

both :: Reader a -> Reader a -> Reader a
both p1 p2 = Reader p where
  p cs = case runReader p1 cs of
    Fail -> Fail
    (Ok _ _) -> runReader p2 cs

either :: Reader a -> Reader a -> Reader a
either p1 p2 = Reader p where
  p s = case runReader p1 s of
    Fail -> runReader p2 s
    ok -> ok

seq :: Reader a -> Reader b -> Reader (a, b)
seq p1 p2 = (,) <$> p1 <*> p2

zeroMany :: Reader a -> Reader [a]
zeroMany p = Reader pa where
  pa = loop []
  loop acc cs = case runReader p cs of
                 Fail -> Ok (reverse acc) cs
                 (Ok c rest) -> loop (c:acc) rest

oneMany :: Reader a -> Reader [a]
oneMany p = do
    x <- p
    xs <- zeroMany p
    return (x:xs)

notEmpty :: Reader Char
notEmpty = Reader p where
     p s = case s
           of [] -> Fail
              (fs:rest) -> Ok fs rest

charTest :: (Char -> Bool) -> Reader Char
charTest f = both notEmpty (Reader p) where
       p s = if not $ f $ head s
             then Fail
             else Ok (head s) (tail s)

numeric ::  Reader Char
numeric = charTest isDigit

alpha ::  Reader Char
alpha = charTest isAlpha

alphaNumeric ::  Reader Char
alphaNumeric = charTest isAlphaNum

space ::  Reader Char
space = charTest isSpace

whitespace ::  Reader String
whitespace = zeroMany space

char ::  Char -> Reader Char
char c = charTest (== c)

notChar ::  Char -> Reader Char
notChar c = charTest $ not . (== c)

token ::  Reader b -> Reader b
token p = snd <$> seq whitespace p

symbolSpecial ::  Reader Char
symbolSpecial = charTest $ \c -> c `elem` "+-/=><*!?"

symbolSeq ::  Reader String
symbolSeq = do first <- either alpha symbolSpecial
               rest <- zeroMany alphaNumeric
               return (first:rest)

symbol ::  Reader LispData
symbol = Symbol <$> token symbolSeq

identifier ::  String -> Reader LispData
identifier name = do (Symbol sym) <- symbol
                     Reader $ equals sym where
  equals sym cs = if sym == name
                  then Ok (Symbol sym) cs
                  else Fail

boolean ::  Reader LispData
boolean = either (fmap (const $ LispBool True) (identifier "true"))
                 (fmap (const $ LispBool False) (identifier "false"))

keyword ::  Reader LispData
keyword = do _ <- token $ char ':'
             str <- symbolSeq
             return (Keyword str)

string ::  Reader LispData
string = do _ <- token quote
            xs <- zeroMany stringChar
            _ <- quote
            return (Str xs) where
  quote = char '"'
  stringChar = notChar '"'

number :: Reader LispData
number = Number . read <$> token (oneMany numeric)

list ::  Reader LispData
list = do _ <- token $ char '('
          xs <- zeroMany expr
          _ <- token $ char ')'
          return $ Sexpr (foldr cons Null xs)

lispMap ::  Reader LispData
lispMap = do _ <- token $ char '{'
             xs <- zeroMany (seq keyword expr)
             _ <- token $ char '}'
             return $ LispMap (M.fromList xs)

expr ::  Reader LispData
expr =  boolean `either` string `either` number `either` symbol `either` keyword  `either` list `either` lispMap

-- @todo implement instance Read
readLispData :: String -> Result LispData
readLispData = runReader expr
