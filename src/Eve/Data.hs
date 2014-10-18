module Eve.Data (
  List(Pair, Null),
  LispData(LispBool, Symbol, Keyword, Str, Number, Sexpr, LispMap),
  cons,
  concat
  ) where

import Prelude hiding (True, False, foldr, takeWhile, concat)
import Control.Applicative
import Control.Monad (ap)
import Data.Foldable (Foldable(foldr))
import Data.Traversable
import qualified Data.Map as M

data List a = Pair a (List a) | Null deriving (Show, Ord, Eq)

data LispData = Symbol String
             | LispBool Bool
             | Keyword String
             | Str String
             | Number Int
             | Sexpr (List LispData)
             | LispMap (M.Map LispData LispData)
             deriving (Show, Ord, Eq)

cons ::  a -> List a -> List a
cons = Pair

concat :: List a -> List a -> List a
concat Null ys = ys
concat (Pair x xs) ys = Pair x (concat xs ys)

instance Foldable List where
  foldr _ z Null = z
  foldr f z (Pair first rest) = f first (foldr f z rest)

instance Functor List where
  fmap _ Null = Null
  fmap f (Pair x xs)  = Pair (f x) (fmap f xs)

instance Monad List where
  return x = Pair x Null
  m >>= f = foldr (concat . f) Null m

instance Applicative List where
  pure = return
  (<*>) = ap

instance Traversable List where
  traverse f = foldr cons_f (pure Null) where
    cons_f x ys = Pair <$> f x <*> ys

-- takeWhile :: (a -> Bool) -> List a -> List a
-- takeWhile _ Null = Null
-- takeWhile f (Pair x xs)
--   | f x       = Pair x (takeWhile f xs)
--   | otherwise = Null
