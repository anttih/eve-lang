module Eve.Data
  ( List(Pair, Null)
  , LispData(..)
  , Primitive(..)
  , cons
  , OpCode
  , OpCodeF(..)
  ) where

import Prelude hiding (foldr, takeWhile, concat)
import Control.Applicative
import Control.Monad (ap)
import Control.Monad.Free
import Data.Foldable (Foldable(foldr))
import Data.Traversable
import qualified Data.Map as M

data List a = Pair a (List a) | Null deriving (Ord, Eq)

instance (Show a) => Show (List a) where
  show Null = ""
  show (Pair a Null) = show a
  show (Pair a xs) = show a ++ " " ++ show xs

data LispData = Symbol String
              | LispBool Bool
              | Keyword String
              | Str String
              | Number Int
              | Sexpr (List LispData)
              | LispMap (M.Map LispData LispData)
              | Function Primitive
              | Closure [String] (OpCode ()) [M.Map String LispData]
              deriving (Ord, Eq)

instance Show LispData where
  show (Symbol name) = name
  show (LispBool True) = "true"
  show (LispBool False) = "false"
  show (Keyword name) = ":" ++ name
  show (Str s) = show s
  show (Number n) = show n
  show (Sexpr Null) = "()"
  show (Sexpr pair) = "(" ++ show pair ++ ")"
  show (LispMap m) = show m
  show (Function f) = show f
  show Closure{} = "#<closure>"

data Primitive = NAry ([LispData] -> LispData)
               | Fn1 (LispData -> LispData) 
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

-- Opcodes for the VM
data OpCodeF next = Refer String next
                  | Constant LispData next
                  | Assign String next
                  | Close [String] next next
                  | Def String next
                  | Test next next
                  | Apply
                  | Return
                  | Frame next next
                  | Argument next
                  | Halt deriving (Show, Ord, Eq)

type OpCode a = Free OpCodeF a

instance Functor OpCodeF where
  fmap f (Refer v next) = Refer v (f next)
  fmap f (Constant v next) = Constant v (f next)
  fmap f (Assign n next) = Assign n (f next)
  fmap f (Close params body next) = Close params (f body) (f next)
  fmap f (Def n next) = Def n (f next)
  fmap f (Test then' alt) =  Test (f then') (f alt)
  fmap _ Apply = Apply
  fmap _ Return = Return
  fmap f (Frame next next') = Frame (f next) (f next')
  fmap f (Argument next) = Argument (f next)
  fmap _ Halt = Halt

