-- Chapter 13

import Control.Monad
import Control.Applicative
import Data.Monoid
import Data.Maybe

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

tell :: a -> Writer a ()
tell s = Writer ((), s)

addStuff a b = let r = a+b in
  Writer (r,["Added " ++ show a ++ " and " ++ show b ++ " gives " ++ show r])

multStuff a b = let r = a*b in
  Writer (r, ["Multiplied " ++ show a ++ " by " ++ show b ++ " gives " ++ show r])

squareStuff a = let r = a**2 in
  Writer (r, ["Squared " ++ show a ++ " gives " ++ show r])


doStuff =
  Writer (10, ["How did we do it?"]) >>= addStuff 5 >>= multStuff 5 >>= squareStuff

doStuff2 :: (Show a, Floating a) => Writer [String] a
doStuff2 = do
  a <- Writer (10, ["How did we do it again??"])
  b <- addStuff 5 a
  c <- multStuff 5 b
  d <- squareStuff c
  tell ["Thats how we do it!"]
  return d

doStuff3 :: (Show a, Floating a) => Writer [String] a
doStuff3 = do
  let doIt a = addStuff 5 a >>= multStuff 5 >>= squareStuff
  a <- Writer (10, ["How do we do this again!? I got bad memory!"])
  result <- doIt a
  tell ["Dont forget anymore!"]
  return result


----------------
-- GCD Logger
------

gcd' :: Int -> Int -> Writer [String] Int
gcd' a b
  | b == 0 = do
      tell ["b is 0, found gcd: " ++ show a]
      return a

  | otherwise = do
      tell ["Calling gcd' with " ++ show b ++ " and (" ++ show a ++ " mod " ++ show b ++ ")"]
      gcd' b (a `mod` b)


------------------------
-- Difference Lists
------------

newtype DiffList a = DiffList { getList :: [a] -> [a] }

newDiffList :: [a] -> DiffList a
newDiffList xs = DiffList (xs++)

instance Semigroup (DiffList a) where
  DiffList a <> DiffList b = DiffList ( a . b )

instance Monoid (DiffList a) where
  mempty = DiffList ([]++)


gcd2' :: Int -> Int -> Writer (DiffList String) Int
gcd2' a b
  | b == 0 = do
      tell $ newDiffList ["The answer is: " ++ show a ++ "!"]
      return a

  | otherwise = do
      let r = a `mod` b
      tell $ newDiffList [show a ++ " mod " ++ show b ++ " = " ++ show r]
      gcd2' b r

gcd3' a b =
  let x = getList $ snd $ (\x -> let Writer(a,b) = x in (a,b)) $ gcd2' a b
  in
    x []
