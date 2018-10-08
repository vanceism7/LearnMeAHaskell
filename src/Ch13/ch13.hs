-- Chapter 13

import Control.Monad
import Control.Applicative
import Data.Monoid

applyLog :: (Monoid m) => (a,m) -> (a -> (b,m)) -> (b,m)
applyLog (val,m) f =
  let (newVal, newM) = f val
  in (newVal, m <> newM)


newtype Writer w a = Writer (a,w) deriving (Show, Read, Eq)

instance Functor (Writer w) where
  fmap f (Writer (a,b)) = Writer (f a,b)

instance (Monoid w) => Applicative (Writer w) where
  pure x = Writer (x,mempty)
  Writer (f,w) <*> Writer (a,b) = Writer (f a, w <> b)

instance (Monoid w) => Monad (Writer w) where
  return = pure
  Writer (a,b) >>= f =
    let Writer (val,m) = f a
    in Writer (val, b <> m)


addStuff a b = Writer (a+b,["Added " ++ show a ++ " and " ++ show b])
multStuff a b = Writer (a*b, ["Multiplied " ++ show a ++ " by " ++ show b])
squareStuff a = Writer (a**2, ["Squared " ++ show a])


doStuff =
  Writer (10, ["How did we do it?"]) >>= addStuff 5 >>= multStuff 5 >>= squareStuff
