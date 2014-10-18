module Parser where

import Prelude hiding (concat, sequence)
import Reader
import Control.Applicative
import Control.Monad.Trans.Either
import Control.Monad.State.Strict hiding (sequence)

import Data
import Data.List (find)

type Bindings = [[String]]

type Syntax a = EitherT String (State Bindings) (a, List LispData)

data Ast = Let [String] [Ast] Ast
         | Function [String] Ast
         | Application Ast [Ast]
         | Definition String Ast
         | LocalReference String
         | FreeReference String
         | Literal LispData
         | Alternative Ast Ast Ast
         | Seq [Ast] deriving (Show)

newtype Parser a = Parser { runParser :: List LispData -> Syntax a }

instance Monad Parser where
  checker >>= f = Parser bind where
    bind xs =
      let syntax = runParser checker xs in
      EitherT $ do
        a <- runEitherT syntax
        case a of
          Left e -> return $ Left e
          Right (x, rest) -> runEitherT $ runParser (f x) rest
  return x = Parser (\xs -> return (x, xs))

instance Applicative Parser where
  pure = return
  (<*>) = ap

instance Alternative Parser where
  empty = Parser (\_ -> left "")
  (<|>) c1 c2 = Parser f where
    f xs = runParser c1 xs <|> runParser c2 xs

instance Functor Parser where
  fmap f checker = Parser c where
    c xs =
      let syntax = runParser checker xs in
      EitherT $ do
        a <- runEitherT syntax
        case a of
          Left e -> return $ Left e
          Right (x, rest) -> return $ Right (f x, rest)

-- State modification
bindings :: Parser [(String, Ast)]
bindings = many $ sexpr $ (,) <$> binding <*> lispExpr where
  binding = (\(Symbol s) -> s) <$> anySymbol

addBinding :: String -> Parser ()
addBinding name = Parser c where
  c xs = state (\s -> (((), xs), newState name s)) where
    newState x [] = [[x]]
    newState x (frame:prev) = (x : frame) : prev

pushFrame :: [String] -> Parser ()
pushFrame names = Parser c where
  c xs = state (\prev -> (((), xs), names : prev))

popFrame :: Parser ()
popFrame = Parser c where
  c xs = state (\s -> (((), xs), pop s)) where
    pop [] = []
    pop (_:rest) = rest

lispLet :: Parser Ast
lispLet = sexpr $ do
  void $ symbol "let"
  b <- sexpr bindings
  pushFrame (fst <$> b)
  body <- sequence
  popFrame
  return (Let (fst <$> b) (snd <$> b) (Seq body))

sequence :: Parser [Ast]
sequence = many lispExpr

definition :: Parser Ast
definition = sexpr $ do
  void $ symbol "def"
  (Symbol name) <- anySymbol
  expr <- lispExpr
  addBinding name
  return $ Definition name expr

funcDefinition :: Parser Ast
funcDefinition = sexpr $ do
  void $ symbol "defn"
  (Symbol name) <- anySymbol
  addBinding name
  params <- sexpr $ many $ (\(Symbol s) -> s) <$> anySymbol
  pushFrame params
  impl <- sequence
  popFrame
  return $ Definition name (Function params (Seq impl))

doBlock :: Parser Ast
doBlock = Seq <$> sexpr (symbol "do" *> sequence)

reference :: Parser Ast
reference = do
  (Symbol name) <- anySymbol
  addRef name where
    addRef :: String -> Parser Ast
    addRef name = Parser f where
      f xs = do
        s <- get
        return $ maybe (FreeReference name, xs) (const (LocalReference name, xs)) $ find (elem name) s

-- @todo Won't work? State from c1 is being used in c2
(<&>) :: Parser a -> Parser b -> Parser b
(<&>) c1 c2 = Parser f where
  f xs =
    let syntax = runParser c1 xs in
    EitherT $ do
      a <- runEitherT syntax
      case a of
        Left e -> return $ Left e
        Right _ -> runEitherT $ runParser c2 xs

anyVal :: Parser LispData
anyVal = Parser f where
  f Null = left "Expecting a value, but got nothing"
  f (Pair x rest) = return (x, rest)

endSexpr :: Parser ()
endSexpr = Parser f where
  f Null = return ((), Null)
  f (Pair x _) = left $ "Expecting the end of list, but got " ++ show x

-- Parse values from inside a lisp list (...) with checker `c`. The checker
-- should consume all of the values inside the list.
sexpr :: Parser a -> Parser a
sexpr c = anyVal <&> Parser isSexpr where
  isSexpr (Pair x rest) = case x of
    (Sexpr xs) -> (\(res, _) -> (res, rest)) <$> runParser (c <* endSexpr) xs
    _ -> left $ "Expecting a list, got " ++ show x
  isSexpr _ = left ""

check :: (LispData -> Bool) -> String -> Parser LispData
check f msg = Parser checker where
  checker (Pair x rest) = if f x then return (x, rest) else left $ msg ++ ", got " ++ show x
  checker _ = left "Fail. This should not happen."

anySymbol ::  Parser LispData
anySymbol = anyVal <&> check sym "Expecting any symbol" where
  sym (Symbol _) = True
  sym _ = False

symbol ::  String -> Parser LispData
symbol name = anySymbol <&> check named ("Expecting symbol " ++ name) where
  named (Symbol sym) = sym == name
  named _ = False

string :: Parser LispData
string = check s "Expecting a string" where
  s (Str _) = True
  s _ = False

literal :: Parser Ast
literal = Literal <$> check val "Expecting a literal" where
  val (LispBool _) = True
  val (Number _) = True
  val (Str _) = True
  val (LispMap _) = True
  val (Keyword _) = True
  val _ = False

lispExpr :: Parser Ast
lispExpr = literal <|> definition <|> funcDefinition <|> reference <|> lispLet <|> doBlock

parse :: Parser LispData -> String -> Either String LispData
parse c input = case readLispData input of
  Ok e _ -> either Left (Right . fst) $ evalState (runEitherT (runParser c (Pair e Null))) []
  Fail -> Left "Parse error"

parseExpr :: String -> Either String Ast
parseExpr input = case readLispData input of
  Ok e _ -> either Left (Right . fst) $ evalState (runEitherT (runParser lispExpr (Pair e Null))) []
  Fail -> Left "Parse error"

runExprState' :: Parser Ast -> String -> Either String Bindings
runExprState' checker input = case readLispData input of
  Ok e _ -> case runState (runEitherT (runParser checker (Pair e Null))) [] of
    (Left m, _) -> Left m
    (Right _, s) -> Right s
  Fail -> Left "Parse error"

runExprState :: String -> Either String Bindings
runExprState = runExprState' lispExpr
