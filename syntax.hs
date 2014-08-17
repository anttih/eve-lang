module Syntax where

import Prelude hiding (concat)
import Parser
import Control.Monad.Trans.Except
import Control.Monad.State.Lazy

type Syntax a = ExceptT String (State ()) (a, List' Sexpr)

type Bindings = [String]

data Ast = Let [String] [Ast] Ast
         | Definition String Ast
         | LispSymbol String
         | LispString String
         | LispNumber Int
         | If Ast Ast Ast
         | Seq [Ast]

newtype Checker a = Checker { runChecker :: List' Sexpr -> Syntax a }

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

-- lispSpecial :: String -> Sexpr -> Syntax Sexpr
-- lispSpecial name sexpr = case sexpr of
--     (List (Pair (Symbol sym) rest)) | sym == name -> return (List rest)
--     _ -> fail "failure"
-- 
-- lispLet :: Sexpr -> Syntax Ast
-- lispLet s = do
--   (List (Pair b (Pair body _))) <- lispSpecial "let" s
--   b <- bindings b
--   seq <- thunk body
--   return (Let ["moi"] [] seq)
-- 
-- bindings :: Sexpr -> Syntax Sexpr
-- bindings = list $ zeroMany $ list2 symbol any

--definition :: Sexpr -> Syntax Ast
--definition s = do
--  _ <- symbol "def" s
--  (Symbol name) <- anySymbol s
--  expr <- lispExpr
--  -- @todo set binding in lexical context
--  -- set ...
--  return $ Definition name expr

--thunk :: Sexpr -> Syntax Ast
--thunk _ = return $ Seq []

-- checkLet :: MaybeT ExpansionState Int
-- checkLet = do (List p) <- lispSpecial "let"
--               return p

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

anyVal :: Checker Sexpr
anyVal = Checker f where
  f Null = throwE "Expecting a value, but got nothing"
  f (Pair x rest) = return (x, rest)

list :: Checker Sexpr
list = anyVal &&& Checker f where
  f (Pair x rest) = case x of
    (List _) -> return (x, rest)
    _ -> throwE $ "Expecting a list, got " ++ show x
  f _ = throwE ""

--zeroMany :: (Sexpr -> Syntax Sexpr) -> Sexpr -> Syntax Sexpr
--zeroMany _ (List Null) = return (List Null)
--zeroMany f (List l) = ExceptT $ loop Null l where
--  loop acc Null = return $ Right (List acc)
--  loop acc (Pair x xs) = do
--    a <- runExceptT (f x)
--    case a of
--      Left _ -> return $ Right (List acc)
--      Right r -> loop (concat acc (Pair r Null)) xs
--zeroMany _ _ = fail "Not a list"

--  success e = case runStateT (runExceptT e) of
--    Right _ -> True
--    Left _ -> False
 
check :: (Sexpr -> Bool) -> String -> Checker Sexpr
check f msg = Checker checker where
  checker (Pair x rest) = if f x then return (x, rest) else throwE $ msg ++ ", got " ++ show x
  checker _ = throwE "Fail. This should not happen."

anySymbol ::  Checker Sexpr
anySymbol = anyVal &&& check sym "Expecting any symbol" where
  sym (Symbol _) = True
  sym _ = False

symbol ::  String -> Checker Sexpr
symbol name = anySymbol &&& check named ("Expecting symbol " ++ name) where
  named (Symbol sym) = sym == name
  named _ = False

string :: Checker Sexpr
string = check s "Expecting a string" where
  s (Str _) = True
  s _ = False

number :: Checker Sexpr
number = check num "Expecting a number" where
  num (Number _) = True
  num _ = False

--nullList ::  Sexpr -> Result
--nullList (List Null) = Ok
--nullList s = Error $ "Expecting an empty list, got " ++ show s
--
--isPair ::  Sexpr -> Result
--isPair (List Null) = Error "Not expecting an empty list"
--isPair (List (Pair _ _)) = Ok
--isPair s = Error $ "Expecting a non empty list, got" ++ show s
--
--pair :: (Sexpr -> Result) -> (Sexpr -> Result) -> Sexpr -> Result
--pair first rest = both isPair checkPair where
--  checkPair (List (Pair x xs)) = case (first x, rest (List xs))
--                                   of (Ok, Ok) -> Ok
--                                      (Error m, _) -> Error m
--                                      (Ok, Error m) -> Error m
--  checkPair _ = Error "Expecting a pair"

--data Checker a = Checker (a -> Result)
--

-- letForm = do symbol "let"
--              list (zeroMany bindings)
--              thunk


--checkString :: (Sexpr -> Result) -> String -> Either String Sexpr
--checkString c s = check (parse expr s) where
--  check (Ok sexpr _) = case c sexpr
--                          of Ok -> Right sexpr
--                             (Error msg) -> Left msg
--  check Fail = Left "Parse error"

lispExpr :: Checker Sexpr
lispExpr = anySymbol <|> number

parse :: Checker Sexpr -> String -> Either String Sexpr
parse c input = case readSexpr input of
  Ok e _ -> either Left (Right . fst) $ evalState (runExceptT (runChecker c (Pair e Null))) ()
  Fail -> Left "Parse error"

parseExpr :: String -> Either String Sexpr
parseExpr input = case readSexpr input of
  Ok e _ -> either Left (Right . fst) $ evalState (runExceptT (runChecker lispExpr (Pair e Null))) ()
  Fail -> Left "Parse error"

--parseString ::  String -> Sexpr
--parseString input = runState (runExceptT )

-- forms (List (Pair (Symbol "let") rest)) =
-- forms (List (Pair (Symbol "if") rest)) =
