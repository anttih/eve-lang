module Eve.Data (
  List(Pair, Null),
  LispData(LispBool, Symbol, Keyword, Str, Number, Sexpr, LispMap, Function),
  Primitive(..),
  cons
  ) where

import Prelude hiding (foldr, takeWhile, concat)
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
              | Function Primitive
              deriving (Ord, Eq)

instance Show LispData where
  show (Symbol name) = name
  show (LispBool True) = "true"
  show (LispBool False) = "false"
  show (Keyword name) = ":" ++ name
  show (Str s) = show s
  show (Number n) = show n
  show (Sexpr Null) = "()"
  show (Sexpr (Pair f rest)) = "(" ++ show f ++ show rest ++ ")"
  show (LispMap m) = show m
  show (Function f) = show f

data Primitive = Fn1 (LispData -> LispData) 
             | Fn2 (LispData -> LispData -> LispData)
             | Fn3 (LispData -> LispData -> LispData -> LispData)
             | Fn4 (LispData -> LispData -> LispData -> LispData -> LispData)
             | Fn5 (LispData -> LispData -> LispData -> LispData -> LispData -> LispData)
             | Fn6 (LispData -> LispData -> LispData -> LispData -> LispData -> LispData -> LispData)
             | Fn7 (LispData -> LispData -> LispData -> LispData -> LispData -> LispData -> LispData -> LispData)
             | Fn8 (LispData -> LispData -> LispData -> LispData -> LispData -> LispData -> LispData -> LispData -> LispData)

instance Show Primitive where
  show _ = "#<primitive>"

instance Eq Primitive where
  (==) _ _ = False

instance Ord Primitive where
  compare _ _ = LT

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
