module Ch12.Ch12 where

-- Chapter 12
import Control.Monad

applyMaybe :: Maybe a -> (a -> Maybe b) -> Maybe b
applyMaybe Nothing _ = Nothing
applyMaybe (Just x) f = f x

--------
type Pole = (Int,Int)
data PoleSide = PLeft | PRight

addBirds :: PoleSide -> Int -> Pole -> Maybe Pole

addBirds PLeft num (l,r) =
  if abs ((l+num) - r) > 3 then
    Nothing
  else
    Just (l + num, r)

addBirds PRight num (l,r) =
  if abs ((r+num) -l) > 3 then
    Nothing
  else
    Just (l, r + num)

doTheDo :: Pole -> Maybe Pole
doTheDo initial = do
  first <- addBirds PLeft 3 initial
  second <- addBirds PRight 2 first
  third <- addBirds PLeft 4 second
  return third

listDos :: [(Int,Char)]
listDos = do
  a <- filter ((elem '7') . show) [1..50]
  b <- "ab"
  return (a,b)


-------------------------------
-- Chess Knight Positions --
-----------------

type KnightPos = (Int,Int)

getNextPossible :: KnightPos -> [KnightPos]
getNextPossible (x,y) =
  let possible =
        [(x+2,y+1),
        (x+2,y-1),
        (x-2,y+1),
        (x-2,y-1),
        (x+1,y+2),
        (x-1,y+2),
        (x+1,y-2),
        (x-1,y-2)]
  in
    filter (\(a,b) -> a >= 0 && b >= 0) possible

possibleInNMoves :: KnightPos -> Int -> KnightPos -> Bool
possibleInNMoves pos 0 from = pos == from
possibleInNMoves pos n from =
  let nextMoves acc _ = acc ++ (acc >>= getNextPossible)
      possible = foldl nextMoves [from] [1..n]
  in
    elem pos possible

