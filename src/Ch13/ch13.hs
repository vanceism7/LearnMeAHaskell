module Ch13.Ch13 where

-- Chapter 13

import Control.Monad
import Control.Applicative
import Data.Monoid
import Data.Maybe
-- import System.Random
import Data.Ratio

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

-------------
-- Reader
-----

addStuff2 :: Int -> Int
addStuff2 = do
  a <- (*5)
  b <- (+10)
  return (a+b)

------------------
-- Testing Stuff
--------

maybeDoStuff :: Int -> Maybe String
maybeDoStuff x = do
  let getMsg = (\m -> if m >= 20 then Just "Hello" else Nothing)
  msg <- getMsg x
  return (msg ++ " friend!")

------------------
-- State Monad
---------

type Stack a = [a]

-- Plain Stack (Non Monadic)
push :: a -> Stack a -> ( (), Stack a )
push item xs = ((), item : xs)

pop :: Stack a -> (a, Stack a)
pop (x:xs) = (x,xs)

stackManip :: Stack Int -> (Int, Stack Int)
stackManip stack =
  let (v,s) = push 3 stack
      (v1,s1) = pop s
  in
    pop s1

applyState :: (s -> (v,s)) -> (v1,s) -> (v,s)
applyState f (v,s) =
  let (a,b) = f s in (a,b)

stackManip2 :: Stack Int -> (Int, Stack Int)
stackManip2 stack =
  let r1 = push 3 stack
      result = applyState (push 15) (applyState (push 10) (applyState (push 4) r1))
  in
    applyState pop result


-- State Monadic Stack
newtype State s a = State { runState :: s -> (a,s) }

instance Functor (State s) where
  fmap f (State s) = State $ \w -> let (a,b) = s w in (f a, b)

instance Applicative (State s) where
  pure x = State $ \s -> (x,s)
  State f <*> State s =
    State $ \w -> let (f1,s1) = f w
                      (a,b) = s w
                  in
                    (f1 a, b)

instance Monad (State s) where
  return = pure
  State st >>= f =
    State $ \w ->
              let (a,b) = st w
                  State m = f a
              in
                m b


pushs :: a -> State (Stack a) ()
pushs item = State $ (\xs -> ((), item : xs))

-- Gotta have em
pops :: State (Stack a) a
pops = State $ \(x:xs) -> (x,xs)


stackManip3 :: State (Stack Int) Int
stackManip3 = do
  pushs 3
  pushs 4
  pushs 10
  pops

stackManip4 :: State (Stack Int) Int
stackManip4 =
  pushs 3 >> pushs 4 >> pushs 10 >> pops


------------------
-- Random State
-------

-- randomSt :: (RandomGen a, Random b ) => State a b
-- randomSt = State random

-- getRandoms :: State StdGen Bool
-- getRandoms = do
--   a <- randomSt :: State StdGen Int
--   if a > 50
--     then return True
--     else return False

----------------
-- Funcs
----

justIt :: Int -> Maybe Int
justIt x = Just x

myfmap :: (a -> b) -> Maybe a -> Maybe b
myfmap f Nothing = Nothing
myfmap f (Just x) = Just (f x)


-----------------
-- Making Monads
-------

newtype Prob a = Prob { getProb :: [(a,Ratio Int)] } deriving (Show, Eq)

instance Functor Prob where
  fmap f (Prob a) = Prob $ fmap (\(x,y) -> (f x,y)) a

instance Applicative Prob where
  pure x = Prob [(x,1 % 1)]
  Prob fs <*> Prob as =
    let ans = fmap (\(x,y) -> fmap (\(f,z) -> (f x, z * y)) fs ) as
    in Prob $ concat ans

instance Monad Prob where
  return = pure
  Prob a >>= f =
    let res = fmap (\(x,y) ->
            let Prob mx = f x
                result = fmap (\(w,z) -> (w, z * y)) mx
            in result) a
    in
      Prob $ concat res

makeDice :: Int -> Prob Int
makeDice sides =
    Prob $ foldr (\x acc -> (x, 1 % sides): acc) [] [1..sides]

testProb :: Prob Int
testProb =
  let d1 = makeDice 4
      d3 = makeDice 10
  in
    (+) <$> d1 <*> d3

data Coin = Heads | Tails deriving (Show, Eq)

coin :: Prob Coin
coin = Prob [(Heads,1%2),(Tails,1%2)]

cheatCoin :: Prob Coin
cheatCoin = Prob [(Heads, 1%10), (Tails,9%10)]

flipCoins :: Prob Bool
flipCoins = do
  a <- coin
  b <- coin
  c <- cheatCoin
  return (all (==Tails) [a,b,c])

-------------------------------
-- Other stuff/tests
------------------------------

data MyType a = MakeIt { runMakeIt :: a } deriving (Show)

newtype MaybeT m a = MaybeT { runMaybeT :: m (Maybe a) } 

data Person = Person {
    firstName :: String
  , lastName :: String
  , age :: Int
  }


--------------
(>>>=) :: (a -> Maybe b) -> (Maybe a -> Maybe b)
(>>>=) f x =
  case x of
    Just x -> f x
    Nothing -> Nothing

jdiv :: Int -> Int -> Maybe Int
jdiv _ 0 = Nothing
jdiv a b = Just $ a `div` b

------------------
funk :: Float -> Float -> Float
funk a b =
  (a**2) + (b**2)

