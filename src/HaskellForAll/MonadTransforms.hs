module HaskellForAll.MonadTransforms where

import Control.Monad.Identity
import Control.Monad.Except
import Control.Monad.Reader
import Control.Monad.State
import Control.Monad.Writer
import Data.Maybe
import qualified Data.Map as Map

-- Types
type Name = String

data Exp = Lit Integer
         | Var Name
         | Plus Exp Exp
         | Abs Name Exp
         | App Exp Exp
         deriving (Show)

data Value = IntVal Integer
           | FunVal Env Name Exp
           deriving (Show)

type Env = Map.Map Name Value -- Maps from names to values

exampleExp :: Exp
exampleExp = Lit 12 `Plus` (App (Abs "x" (Var "x")) (Lit 4 `Plus` Lit 2))

--Basic Eval function
eval0 :: Env -> Exp -> Value
eval0 env (Lit i) = IntVal i
eval0 env (Var n) = fromJust (Map.lookup n env)

eval0 env (Plus e1 e2) =
  let IntVal i1 = eval0 env e1
      IntVal i2 = eval0 env e2
  in IntVal(i1 + i2)

eval0 env (Abs n e) = FunVal env n e
eval0 env (App e1 e2) =
  let val1 = eval0 env e1
      val2 = eval0 env e2
  in
    case val1 of
      FunVal env' n body -> eval0 (Map.insert n val2 env') body

--------------------
-- Monadic Eval
-----

type Eval1 a = Identity a

runEval1 :: Eval1 a -> a
runEval1 e = runIdentity e

eval1 :: Env -> Exp -> Eval1 Value
eval1 env (Lit i) = return $ IntVal i
eval1 env (Var n) = return $ fromJust (Map.lookup n env)

eval1 env (Plus e1 e2) = do
  IntVal i1 <- eval1 env e1
  IntVal i2 <- eval1 env e2
  return $ IntVal(i1 + i2)

eval1 env (Abs n e) = return $ FunVal env n e
eval1 env (App e1 e2) = do
  val1 <- eval1 env e1
  val2 <- eval1 env e2
  case val1 of
    FunVal env' n body -> eval1 (Map.insert n val2 env') body

----------------------
-- Error Handling
---------

type Eval2 a = ExceptT String Identity a

runEval2 :: Eval2 a -> Either String a
runEval2 e = runIdentity $ runExceptT e

eval2 :: Env -> Exp -> Eval2 Value
eval2 env (Lit i) = return $ IntVal i
eval2 env (Var n) =
  case Map.lookup n env of
    Just x -> return x
    Nothing -> throwError $ "Unbound variable " ++ n

eval2 env (Plus e1 e2) = do
  i1 <- eval2 env e1
  i2 <- eval2 env e2
  case (i1,i2) of
    (IntVal a, IntVal b) -> return $ IntVal(a+b)
    otherwise -> throwError "Type error"

eval2 env (Abs n e) = return $ FunVal env n e
eval2 env (App e1 e2) = do
  val1 <- eval2 env e1
  val2 <- eval2 env e2
  case val1 of
    FunVal env' n body -> eval2 (Map.insert n val2 env') body
    otherwise -> throwError "Type error: value is not a function"


----------------------
-- Reader Extension
------------

type Eval3 a = ReaderT Env (ExceptT String Identity) a

runEval3 :: Env -> Eval3 a -> Either String a
runEval3 env e = runIdentity $ runExceptT $ runReaderT e env

eval3 :: Exp -> Eval3 Value
eval3 (Lit i) = return $ IntVal i
eval3 (Var n) = do
  env <- ask
  case Map.lookup n env of
    Just x -> return x
    Nothing -> throwError $ "Unbound variable " ++ n

eval3 (Plus e1 e2) = do
  i1 <- eval3 e1
  i2 <- eval3 e2
  case (i1,i2) of
    (IntVal a, IntVal b) -> return $ IntVal(a+b)
    otherwise -> throwError "Type error"

eval3 (Abs n e) = do
  env <- ask
  return $ FunVal env n e

eval3 (App e1 e2) = do
  val1 <- eval3 e1
  val2 <- eval3 e2
  case val1 of
    FunVal env' n body ->
      local (const (Map.insert n val2 env')) (eval3 body)
    otherwise ->
        throwError "Type error: value is not a function"

-------------------
-- Adding State
----------

type Eval4 a = ReaderT Env (ExceptT String (StateT Integer Identity)) a

runEval4 :: Env -> Integer -> Eval4 a -> (Either String a, Integer)
runEval4 env state eval = runIdentity (runStateT (runExceptT (runReaderT eval env )) state)

tick :: (Num a, MonadState a m) => m ()
tick = do
  st <- get
  put(st + 1)

eval4 :: Exp -> Eval4 Value
eval4 (Lit i) = do
  tick
  return $ IntVal i

eval4 (Var n) = do
  tick
  env <- ask
  case Map.lookup n env of
    Just x -> return x
    Nothing -> throwError $ "Unbound variable " ++ n

eval4 (Plus e1 e2) = do
  tick
  i1 <- eval4 e1
  i2 <- eval4 e2
  case (i1,i2) of
    (IntVal a, IntVal b) -> return $ IntVal(a+b)
    otherwise -> throwError "Type error"

eval4 (Abs n e) = do
  tick
  env <- ask
  return $ FunVal env n e

eval4 (App e1 e2) = do
  tick
  val1 <- eval4 e1
  val2 <- eval4 e2
  case val1 of
    FunVal env' n body ->
      local (const (Map.insert n val2 env')) (eval4 body)
    otherwise ->
        throwError "Type error: value is not a function"

