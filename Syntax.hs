module Syntax where

import Prelude hiding (concat, sequence)
import Parser
import Control.Applicative hiding ((<|>))
import Control.Monad.Trans.Either
import Control.Monad.State.Strict hiding (sequence)

import Data

type Bindings = [String]

type Syntax a = EitherT String (State Bindings) (a, List LispData)

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
      EitherT $ do
        a <- runEitherT syntax
        case a of
          Left e -> return $ Left e
          Right (x, rest) -> runEitherT $ runChecker (f x) rest
  return x = Checker (\xs -> return (x, xs))

instance Applicative Checker where
  pure = return
  (<*>) = ap

instance Functor Checker where
  fmap f checker = Checker c where
    c xs =
      let syntax = runChecker checker xs in
      EitherT $ do
        a <- runEitherT syntax
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

--addBinding :: String -> Syntax ()
--addBinding name = mapEitherT f $ return ((), Null) where
--  f = mapState (\(a, _) -> (a, [name]))

addBinding :: String -> List LispData -> Syntax ()
addBinding name xs = state (\names -> (((), xs), name : names))

definition :: Checker Ast
definition = sexpr $ do
  _ <- symbol "def"
  (Symbol name) <- anySymbol
  expr <- lispExpr
  _ <- Checker $ addBinding name
  return $ Definition name expr

sequence :: Checker [Ast]
sequence = zeroMany lispExpr

doBlock :: Checker Ast
doBlock = sexpr $ do
  _ <- symbol "do"
  xs <- sequence
  return $ Seq xs

infixl 6 &&&
-- @todo Won't work? State from c1 is being used in c2
(&&&) :: Checker a -> Checker b -> Checker b
(&&&) c1 c2 = Checker f where
  f xs =
    let syntax = runChecker c1 xs in
    EitherT $ do
      a <- runEitherT syntax
      case a of
        Left e -> return $ Left e
        Right _ -> runEitherT $ runChecker c2 xs

-- the 'or' operator
(<|>) :: Checker a -> Checker a -> Checker a
(<|>) c1 c2 = Checker f where
  f xs =
    let syntax = runChecker c1 xs in
    EitherT $ do
      a <- runEitherT syntax
      case a of
        Left _ -> runEitherT $ runChecker c2 xs
        Right x -> return $ Right x

anyVal :: Checker LispData
anyVal = Checker f where
  f Null = left "Expecting a value, but got nothing"
  f (Pair x rest) = return (x, rest)

sexpr :: Checker Ast -> Checker Ast
sexpr c = anyVal &&& Checker f where
  f (Pair x _) = case x of
    (Sexpr rest) -> runChecker c rest
    _ -> left $ "Expecting a list, got " ++ show x
  f _ = left ""

zeroMany :: Checker a -> Checker [a]
zeroMany c = Checker p where
  -- p Null = return (Sexpr Null, Null)
  p l = EitherT $ loop [] l where
    loop acc Null = return $ Right (acc, Null)
    loop acc rest = do
      a <- runEitherT (runChecker c rest)
      case a of
        Left _ -> return $ Right (acc, rest)
        Right (x, xs) -> loop (acc ++ [x]) xs

check :: (LispData -> Bool) -> String -> Checker LispData
check f msg = Checker checker where
  checker (Pair x rest) = if f x then return (x, rest) else left $ msg ++ ", got " ++ show x
  checker _ = left "Fail. This should not happen."

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

literal :: Checker Ast
literal = Literal <$> check val "Expecting a literal" where
  val (LispBool _) = True
  val (Number _) = True
  val (Str _) = True
  val (LispMap _) = True
  val (Keyword _) = True
  val _ = False

lispExpr :: Checker Ast
lispExpr = literal <|> definition <|> doBlock

parse :: Checker LispData -> String -> Either String LispData
parse c input = case readLispData input of
  Ok e _ -> either Left (Right . fst) $ evalState (runEitherT (runChecker c (Pair e Null))) []
  Fail -> Left "Parse error"

parseExpr :: String -> Either String Ast
parseExpr input = case readLispData input of
  Ok e _ -> either Left (Right . fst) $ evalState (runEitherT (runChecker lispExpr (Pair e Null))) []
  Fail -> Left "Parse error"

runExprState' :: Checker Ast -> String -> Either String Bindings
runExprState' checker input = case readLispData input of
  Ok e _ -> case runState (runEitherT (runChecker checker (Pair e Null))) [] of
    (Left m, _) -> Left m
    (Right _, s) -> Right s
  Fail -> Left "Parse error"

runExprState :: String -> Either String Bindings
runExprState = runExprState' lispExpr
