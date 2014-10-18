module Reader (
  Result(Ok, Fail),
  readLispData
  ) where

import Prelude hiding (seq, negate, foldr, takeWhile, concat)
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

instance Alternative Reader where
  empty = Reader (const Fail)
  (<|>) p1 p2 = Reader f where
    f xs = case runReader p1 xs of
      Fail -> runReader p2 xs
      ok -> ok

both :: Reader a -> Reader a -> Reader a
both p1 p2 = Reader p where
  p cs = case runReader p1 cs of
    Fail -> Fail
    (Ok _ _) -> runReader p2 cs

seq :: Reader a -> Reader b -> Reader (a, b)
seq p1 p2 = (,) <$> p1 <*> p2

notEmpty :: Reader Char
notEmpty = Reader p where
  p []        = Fail
  p (fs:rest) = Ok fs rest

charTest :: (Char -> Bool) -> Reader Char
charTest f = both notEmpty (Reader p) where
  p []                = Fail
  p (c:_) | not $ f c = Fail
  p (x:xs)            = Ok x xs

numeric ::  Reader Char
numeric = charTest isDigit

alpha ::  Reader Char
alpha = charTest isAlpha

alphaNumeric ::  Reader Char
alphaNumeric = charTest isAlphaNum

space ::  Reader Char
space = charTest isSpace

whitespace ::  Reader String
whitespace = many space

char ::  Char -> Reader Char
char c = charTest (== c)

notChar ::  Char -> Reader Char
notChar c = charTest $ not . (== c)

token ::  Reader b -> Reader b
token p = snd <$> seq whitespace p

symbolSpecial ::  Reader Char
symbolSpecial = charTest $ \c -> c `elem` "+-/=><*!?"

symbolSeq ::  Reader String
symbolSeq = do first <- alpha <|> symbolSpecial
               rest <- many alphaNumeric
               return (first:rest)

symbol ::  Reader LispData
symbol = Symbol <$> token symbolSeq

identifier ::  String -> Reader LispData
identifier name = do
  (Symbol sym) <- symbol
  Reader $ equals sym where
    equals sym cs | sym == name = Ok (Symbol sym) cs
    equals _ _ = Fail

boolean ::  Reader LispData
boolean = true <$> identifier "true" <|> false <$> identifier "false" where
  true = const $ LispBool True
  false = const $ LispBool False

keyword ::  Reader LispData
keyword = do
  _ <- token $ char ':'
  str <- symbolSeq
  return (Keyword str)

string ::  Reader LispData
string = do
  _ <- token quote
  xs <- many stringChar
  _ <- quote
  return (Str xs) where
    quote = char '"'
    stringChar = notChar '"'

number :: Reader LispData
number = Number . read <$> token (some numeric)

list ::  Reader LispData
list = do
  _ <- token $ char '('
  xs <- many expr
  _ <- token $ char ')'
  return $ Sexpr (foldr cons Null xs)

lispMap ::  Reader LispData
lispMap = do
  _ <- token $ char '{'
  xs <- many (seq keyword expr)
  _ <- token $ char '}'
  return $ LispMap (M.fromList xs)

expr ::  Reader LispData
expr =  boolean <|> string <|> number <|> symbol <|> keyword <|> list <|> lispMap

-- @todo implement instance Read
readLispData :: String -> Result LispData
readLispData = runReader expr
