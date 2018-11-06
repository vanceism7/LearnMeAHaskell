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
