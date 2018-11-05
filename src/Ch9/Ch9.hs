module Ch9.Ch9 where

import           Control.Exception
import           Control.Monad
import qualified Data.ByteString.Lazy as B
import qualified Data.Char            as Char
import qualified Data.List            as List
import qualified System.Directory     as Dir
import qualified System.Environment   as Env
import qualified System.IO            as IO
import System.IO.Error

main1 = do
  -- Whatver stuff you want
  putStrLn "Program Start: "
  (src:dest:_) <- Env.getArgs
  cpProg src dest `catch` ioHandler

-----
reverseWordsIO :: IO String
reverseWordsIO = do
  putStrLn "Type something in!"
  line <- getLine
  if null line then
    return "Finished"
   else do
    putStrLn $ reverse line
    reverseWordsIO

reverseWords :: String -> String
reverseWords = unwords . map reverse . words


-----
whenTest :: IO ()
whenTest = do
  c <- getChar
  when (c /= ' ') $ do
    putChar c
    whenTest

-----
foreverDo :: IO ()
foreverDo = forever $ do
  putStrLn "Type something!"
  line <- getLine
  putStrLn $ map Char.toUpper line


-----
forTest :: IO [()]
forTest = do
  colors <- forM [1..4] (\a -> do
                            putStrLn "Enter a color: "
                            getLine)
  putStrLn "Your colors for 1-4 are: "
  mapM putStrLn colors


-----
capslocker :: IO ()
capslocker = do
  contents <- getContents
  putStr $ map Char.toUpper contents


-----
shorterThan10 :: IO ()
shorterThan10 = do
  contents <- getContents
  putStr $ shortLineFilter contents


shortLineFilter :: String -> String
shortLineFilter s =
  let chopper s = if length s <= 10 then s else "too long!"
      allLines = lines s
      short = map chopper allLines
      result = unlines short
  in result


shorterFilter =
  let func = unlines . filter( (<=10) . length) . lines
  in interact func


------
palindromer :: String -> String
palindromer =
  let isPalindrome st = (st == reverse st)
      response st = if isPalindrome st then "palindrome" else "not palindrome"
      doer = unlines . map response . lines
      in doer


--------------
-- File IO
------
fileReader :: IO ()
fileReader = do
  IO.withFile  "girlfriend.txt" IO.ReadMode (\h -> do
                                                contents <- IO.hGetContents h
                                                putStr contents)
  -- where
  --   logContents handle = do
  --         contents <- IO.hGetContents handle
  --         putStr contents


-----
fileWriter :: IO ()
fileWriter = do
  contents <- IO.readFile "girlfriend.txt"
  IO.writeFile "gfc.txt" $ unlines . map (++ " fool!") . lines $ contents


-----
todoAddItem :: String -> IO ()
todoAddItem fn = do
  putStrLn "Add a todo Item?: (Blank line to quit)"
  todoItem <- getLine
  if null todoItem then
    return ()
  else do
    IO.appendFile fn (todoItem ++ "\n")
    todoAddItem fn


todoItemRemover :: String -> IO ()
todoItemRemover fn = do

  contents <- IO.readFile fn

  let tasks = lines contents
      numtasks = zipWith (\i lines -> show i ++ " - " ++ lines) [0..] tasks

  putStrLn "Your tasks are: "
  putStr $ unlines numtasks

  putStr "Delete something? Enter task number: "
  numInput <- getLine
  let taskNum = read numInput
      newItems = List.delete (tasks !! taskNum) tasks

  (tname, thandle) <- IO.openTempFile "." "temp"
  IO.hPutStr thandle $ unlines newItems
  IO.hClose thandle

  Dir.removeFile fn
  Dir.renameFile tname fn

------------------
---- Todo Manager
---------

todoViewer :: String -> a -> IO ()
todoViewer fn _ = do
  contents <- IO.readFile fn
  putStrLn contents


todoAdder :: String -> String -> IO ()
todoAdder fn item = do
  return ()


todoRemover :: String -> String -> IO ()
todoRemover fn item = do
  return ()


dispatch :: [(String, (String -> String -> IO ()))]
dispatch =
  [
    ("add", todoAdder),
    ("delete", todoRemover),
    ("view", todoViewer)
  ]

todoApp :: IO ()
todoApp = do
  (action:fn:item:_) <- Env.getArgs

  putStrLn "usage: <Action: view | add | delete>, <Todo Filename: String>"
  return ()

-----

cpProg :: String -> String -> IO ()
cpProg src dest = do
  contents <- B.readFile src
  B.writeFile dest contents

ioHandler :: IOError -> IO ()
ioHandler e
  | isDoesNotExistError e =
    case ioeGetFileName e of
      Just path -> putStrLn $ "The file: " ++ path ++ " doesn't exist"
      Nothing -> putStrLn "The specified file doesn't exist!"
  | otherwise = ioError e

