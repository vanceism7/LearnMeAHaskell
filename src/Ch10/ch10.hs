module Ch10.Ch10 where
-- Reverse Polish Notation Calculator

import qualified Data.Char as Char
import qualified Data.List as List
import qualified Text.Read as Text

solveRPN :: String -> Float
solveRPN "" = 0
solveRPN s =
  let eqtn = words s
  in
    head $ foldl addStack [] eqtn

ops :: (Fractional a) => String -> Maybe (a -> a -> a)
ops "+" = Just (+)
ops "-" = Just (-)
ops "*" = Just (*)
ops "/" = Just (/)
ops _   = Nothing

-- addStack :: (Fractional a) => [a] -> String -> [a]
addStack :: [Float] -> String -> [Float]
addStack acc cur =
  case Text.readMaybe cur of
    Just x -> x: acc
    otherwise ->
      case ops cur of
        Just op ->
          let (h1:h2:acc') = acc
          in op h1 h2: acc'
        Nothing -> acc


------------------------------
-- Traveling - Graphing Algorithms

data Road = Road { getA :: Int, getB :: Int, getC :: Int } deriving (Show)

-- data Path = Path 

{-
10,50,30
5,90,20
40,2,25
10,8,0
-}

--Groups items in a list into sublists of size n
groupsOf :: Int -> [a] -> [[a]]
groupsOf 0 _ = undefined
groupsOf _ [] = []
groupsOf n xs =
  take n xs : groupsOf n (drop n xs)

-- Parses a set of road lengths into a road list
parseGraph :: String -> [Road]
parseGraph graph =
  let weights = groupsOf 3 $ map read $ lines graph
  in
    foldl (\acc (a:b:c:_) -> Road {getA = a, getB = b, getC = c }: acc) [] weights

-- findShortestPath :: [Road] -> Path
