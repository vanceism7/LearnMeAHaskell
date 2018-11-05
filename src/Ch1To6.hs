module Ch1To6
    ( someFunc,
      add,
    ) where

someFunc :: IO ()
someFunc =
  let num = add 5 10
      msg = "Hello world! Your secret number is: " ++ (show 5)
  in putStrLn msg

add :: (Num a) => a -> a -> a
add x y = x + y

myDrop :: (Num a, Ord a) => a -> [b] -> [b]
myDrop count xs =
   if count <= 0 || null xs
   then xs
   else myDrop (count-1) (tail xs)

maximum' :: (Num a, Ord a) => [a] -> a
maximum' [] = error "No max on empty list"
maximum' [x] = x
maximum' (x:xs)
  | x > maxTail = x
  | otherwise = maxTail
  where
    maxTail = maximum' xs

take' :: (Num a, Ord a) => a -> [b] -> [b]
take' n _
  | n <= 0 = []
take' _ [] = []
take' n (x:xs) =
  x: take' (n-1) xs

zip' :: [a] -> [b] -> [(a,b)]
zip' [] _ = []
zip' _ [] = []
zip' (x:xs) (y:ys) = (x,y):zip' xs ys

elem' :: (Eq a) => a -> [a] -> Bool
elem' _ [] = False
elem' a (x:xs)
  | a == x = True
  | otherwise = elem' a xs


quicksort :: (Ord a) => [a] -> [a]
quicksort [] = []
quicksort (x:xs) = lt ++ [x] ++ gt
   where
     lt = quicksort (filter (<=x) xs)
     gt = quicksort (filter (>x) xs)

applyTwice :: (a -> a) -> a -> a
applyTwice f a = f (f a)

zipWith' :: (a -> b -> c) -> [a] -> [b] -> [c]
zipWith' _ [] _ = []
zipWith' _ _ [] = []
zipWith' f (x:xs) (y:ys) =
  f x y : zipWith' f xs ys

flip' :: (a -> b -> c) -> (b -> a -> c)
flip' f y x = f x y

largest :: (Integral a) => a
largest =
  let p x = x `mod` 3829 == 0
  in head (filter p [100000,99999..])


--sumodd :: (Integer a) => a
sumodd :: (Integral a) => a
sumodd =
  sum (filter p (takeWhile (<10000) (map (^2) [1..10000])))
  where p x = x `mod` 2 == 1

sumodd2 :: (Integral a) => a
sumodd2 =
  -- sum (filter p (takeWhile (<10000) (map (^2) [1..10000])))
  sum( takeWhile (<10000) (filter p (map (^2) [1..10000])))
  where p x = x `mod` 2 == 1

collatz :: (Integral a) => a -> [a]
collatz 1 = [1]
collatz x
  | even x = x: collatz (x `div` 2)
  | odd x = x: collatz((x * 3) + 1)

colseqlen =
  length (filter (>=15) (map length (map collatz [1..100]))) 


sum1 :: (Num a) => [a] -> a
sum1 = foldl (+) 0

elem2 :: (Eq a) => a -> [a] -> Bool
elem2 n =
  foldl (\found x -> if x == n then True else found ) False 

map' :: (a -> b) -> [a] -> [b]
map' f  =
  foldr (\x acc -> f x: acc) []

