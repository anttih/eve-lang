module Eve.Reader (
  Result(Ok, Fail),
  readLispData
  ) where

import Prelude hiding (seq, negate, foldr, takeWhile, concat)
import Data.Char (isAlphaNum, isAlpha, isDigit, isSpace)
import Data.Foldable (Foldable(foldr))
import Control.Applicative
import Control.Monad (liftM, ap)
import qualified Data.Map as M

import Eve.Data

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

lexeme :: Reader a -> Reader a
lexeme p = p <* whitespace

symbolSpecial ::  Reader Char
symbolSpecial = charTest $ \c -> c `elem` "+-/=><*!?"

symbolSeq ::  Reader String
symbolSeq = lexeme $ (:) <$> (alpha <|> symbolSpecial) <*> many alphaNumeric

symbol ::  Reader LispData
symbol = Symbol <$> symbolSeq

identifier ::  String -> Reader LispData
identifier name = do
  (Symbol sym) <- symbol
  Reader $ equals sym where
    equals sym cs | sym == name = Ok (Symbol sym) cs
    equals _ _                  = Fail

boolean ::  Reader LispData
boolean = true <$> identifier "true" <|> false <$> identifier "false" where
  true = const $ LispBool True
  false = const $ LispBool False

keyword ::  Reader LispData
keyword = Keyword <$> (char ':' *> symbolSeq)

string ::  Reader LispData
string = lexeme $ Str <$> (quote *> many stringChar <* quote) where
  quote = char '"'
  stringChar = notChar '"'

number :: Reader LispData
number = lexeme $ Number . read <$> some numeric

list ::  Reader LispData
list = lexeme $ Sexpr . foldr cons Null <$> (char '(' *> many expr <* char ')') where

lispMap ::  Reader LispData
lispMap = lexeme $ LispMap . M.fromList <$> (char '{' *> pairs <* char '}') where
  pairs = many (seq keyword expr)

expr ::  Reader LispData
expr = whitespace *> (boolean <|> string <|> number <|> symbol <|> keyword <|> list <|> lispMap)

-- @todo implement instance Read
readLispData :: String -> Result LispData
readLispData = runReader expr
