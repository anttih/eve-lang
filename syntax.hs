module Syntax where

import Prelude hiding (concat)
import Parser
import Control.Applicative hiding ((<|>))
import Control.Monad.Trans.Except
import Control.Monad.State.Lazy

import Data

type Syntax a = ExceptT String (State ()) (a, List LispData)

type Bindings = [String]

data Ast = Let [String] [Ast] Ast
         | Definition String Ast
         | Literal LispData
         | If Ast Ast Ast
         | Seq [Ast] deriving (Show)

newtype Checker a = Checker { runChecker :: List LispData -> Syntax a }

instance Monad Checker where
  checker >>= f = Checker bind where
    bind xs = 
      let syntax = runChecker checker xs in
      ExceptT $ do
        a <- runExceptT syntax
        case a of
          Left e -> return $ Left e
          Right (x, rest) -> runExceptT $ runChecker (f x) rest
  return x = Checker (\_ -> return (x, Null))

instance Functor Checker where
  fmap f checker = Checker c where
    c xs = 
      let syntax = runChecker checker xs in
      ExceptT $ do
        a <- runExceptT syntax
        case a of
          Left e -> return $ Left e
          Right (x, rest) -> return $ Right (f x, rest)
        

-- lispSpecial :: String -> LispData -> Syntax LispData
-- lispSpecial name sexpr = case sexpr of
--     (List (Pair (Symbol sym) rest)) | sym == name -> return (List rest)
--     _ -> fail "failure"
-- 
-- lispLet :: LispData -> Syntax Ast
-- lispLet s = do
--   (List (Pair b (Pair body _))) <- lispSpecial "let" s
--   b <- bindings b
--   seq <- thunk body
--   return (Let ["moi"] [] seq)
-- 
-- bindings :: LispData -> Syntax LispData
-- bindings = list $ zeroMany $ list2 symbol any

definition :: Checker Ast
definition = sexpr $ do
  _ <- symbol "def"
  (Symbol name) <- anySymbol
  expr <- lispExpr
  -- @todo set binding in lexical context
  -- set ...
  return $ Definition name expr

infixl 6 &&&
-- @todo Won't work? State from c1 is being used in c2
(&&&) :: Checker a -> Checker b -> Checker b
(&&&) c1 c2 = Checker f where
  f xs = 
    let syntax = runChecker c1 xs in
    ExceptT $ do
      a <- runExceptT syntax
      case a of
        Left e -> return $ Left e
        Right _ -> runExceptT $ runChecker c2 xs

-- the 'or' operator
(<|>) :: Checker a -> Checker a -> Checker a
(<|>) c1 c2 = Checker f where
  f xs = 
    let syntax = runChecker c1 xs in
    ExceptT $ do
      a <- runExceptT syntax
      case a of
        Left _ -> runExceptT $ runChecker c2 xs
        Right x -> return $ Right x

anyVal :: Checker LispData
anyVal = Checker f where
  f Null = throwE "Expecting a value, but got nothing"
  f (Pair x rest) = return (x, rest)

sexpr :: Checker Ast -> Checker Ast
sexpr c = anyVal &&& Checker f where
  f (Pair x _) = case x of
    (Sexpr rest) -> runChecker c rest
    _ -> throwE $ "Expecting a list, got " ++ show x
  f _ = throwE ""

--zeroMany :: (LispData -> Syntax LispData) -> LispData -> Syntax LispData
--zeroMany _ (List Null) = return (List Null)
--zeroMany f (List l) = ExceptT $ loop Null l where
--  loop acc Null = return $ Right (List acc)
--  loop acc (Pair x xs) = do
--    a <- runExceptT (f x)
--    case a of
--      Left _ -> return $ Right (List acc)
--      Right r -> loop (concat acc (Pair r Null)) xs
--zeroMany _ _ = fail "Not a list"

check :: (LispData -> Bool) -> String -> Checker LispData
check f msg = Checker checker where
  checker (Pair x rest) = if f x then return (x, rest) else throwE $ msg ++ ", got " ++ show x
  checker _ = throwE "Fail. This should not happen."

anySymbol ::  Checker LispData
anySymbol = anyVal &&& check sym "Expecting any symbol" where
  sym (Symbol _) = True
  sym _ = False

symbol ::  String -> Checker LispData
symbol name = anySymbol &&& check named ("Expecting symbol " ++ name) where
  named (Symbol sym) = sym == name
  named _ = False

string :: Checker LispData
string = check s "Expecting a string" where
  s (Str _) = True
  s _ = False

number :: Checker Ast
number = Literal <$> check num "Expecting a number" where
  num (Number _) = True
  num _ = False

lispExpr :: Checker Ast
lispExpr = number <|> definition

parse :: Checker LispData -> String -> Either String LispData
parse c input = case readLispData input of
  Ok e _ -> either Left (Right . fst) $ evalState (runExceptT (runChecker c (Pair e Null))) ()
  Fail -> Left "Parse error"

parseExpr :: String -> Either String Ast
parseExpr input = case readLispData input of
  Ok e _ -> either Left (Right . fst) $ evalState (runExceptT (runChecker lispExpr (Pair e Null))) ()
  Fail -> Left "Parse error"
