module Set11b where

import Control.Monad
import Data.List
import Data.IORef
import System.IO

import Mooc.Todo


------------------------------------------------------------------------------
-- Ex 1: Given an IORef String and a list of Strings, update the value
-- in the IORef by appending to it all the strings in the list, in
-- order.
--
-- Example:
--   *Set11b> r <- newIORef "x"
--   *Set11b> appendAll r ["foo","bar","quux"]
--   *Set11b> readIORef r
--   "xfoobarquux"

appendAll :: IORef String -> [String] -> IO ()
appendAll st ls = do
  string <- readIORef st
  writeIORef st (string ++ concat (ls))
------------------------------------------------------------------------------
-- Ex 2: Given two IORefs, swap the values stored in them.
--
-- Example:
--   *Set11b> x <- newIORef "x"
--   *Set11b> y <- newIORef "y"
--   *Set11b> swapIORefs x y
--   *Set11b> readIORef x
--   "y"
--   *Set11b> readIORef y
--   "x"

swapIORefs :: IORef a -> IORef a -> IO ()
swapIORefs x y = do
  s <- readIORef x
  p <- readIORef y
  writeIORef  y s
  writeIORef x p

------------------------------------------------------------------------------
-- Ex 3: sometimes one bumps into IO operations that return IO
-- operations. For instance the type IO (IO Int) means an IO operation
-- that returns an IO operation that returns an Int.
--
-- Implement the function doubleCall which takes an operation op and
--   1. runs op
--   2. runs the operation returned by op
--   3. returns the value returned by this operation
--
-- Examples:
--   - doubleCall (return (return 3)) is the same as return 3
--
--   - let op :: IO (IO [String])
--         op = do l <- readLn
--                 return $ replicateM l getLine
--     in doubleCall op
--
--     works just like
--
--     do l <- readLn
--        replicateM l getLine

doubleCall :: IO (IO a) -> IO a
doubleCall op = do
  a <- op
  b <- a
  return b

------------------------------------------------------------------------------
-- Ex 4: implement the analogue of function composition (the (.)
-- operator) for IO operations. That is, take an operation op1 of type
--     a -> IO b
-- an operation op2 of type
--     c -> IO a
-- and a value of type
--     c
-- and returns an operation op3 of type
--     IO b
--
-- op3 should of course
--   1. take the value of type c and pass it to op2
--   2. take the resulting value (of type a) and pass it to op1
--   3. return the result (of type b)

compose :: (a -> IO b) -> (c -> IO a) -> c -> IO b
compose op1 op2 c = do
  let opa = op2 c
  unwrapa <- opa
  op1 unwrapa

------------------------------------------------------------------------------
-- Ex 5: Reading lines from a file. The module System.IO defines
-- operations for Handles, which represent open files that can be read
-- from or written to. Here are some functions that might be useful:
--
-- * hGetLine :: Handle -> IO String
--   Reads one line from the Handle. Will fail if the Handle is at the
--   end of the file
-- * hIsEOF :: Handle -> IO Bool
--   Produces True if the Handle is at the end of the file.
-- * hGetContents :: Handle -> IO String
--   Reads content from Handle until the end of the file.
--
-- Implement the function hFetchLines which returns the contents of
-- the given handle as a sequence of lines.
--
-- There are multiple ways to implement this function. You can either
-- read the lines one by one, or read the whole file and then worry
-- about splitting lines. Both approaches are fine, and you can even
-- try out both!
--
-- Example:
--   *Set11b> h <- openFile "Set11b.hs" ReadMode
--   *Set11b> ls <- hFetchLines h
--   *Set11b> take 3 ls
--   ["module Set11b where","","import Control.Monad"]
--- Implementation 1, where I am reading one line at a time
-- hFetchLines :: Handle -> IO [String]
-- hFetchLines h = do
  -- isEOF' <- hIsEOF h -- check if the handle is pointing on an EOF
  -- if isEOF' then
    -- do
      -- return []
  -- else do
        -- line <- hGetLine h -- read the line, I have a query how the handle is updated? Because we had to manually update the file pointer to the next line.
        -- lines' <- hFetchLines h
        -- return (line: lines')
-- implementation 2 where I am reading the whole content of the file
splitOn :: Char -> String -> [String]
splitOn a xs = case xs of
  "" -> [] -- the list and the string are not the same thing
  (x:xy) -> if a == x then ("" : splitOn a xy)
            else ((x: head(nextoutcome)) : tail nextoutcome) -- this is great way of keeping the information in the list. I like it.
               where
                 nextoutcome = splitOn a xy
hFetchLines :: Handle -> IO [String]
hFetchLines h = do
  contents <- hGetContents h -- Now contents will have all the content of the file, how I have to split the strings into different elements.
  let splitcontent = splitOn '\n' contents
  return splitcontent


------------------------------------------------------------------------------
-- Ex 6: Given a Handle and a list of line indexes, produce the lines
-- at those indexes from the file.
--
-- Line indexing starts from 1.
--
-- Here too, there are multiple ways to implement this. You can try
-- using hFetchLines, or writing out a loop that gets lines from the
-- handle.

hSelectLines :: Handle -> [Int] -> IO [String]
hSelectLines h nums = hSelectLines' 1 h nums
  where hSelectLines' k h num =
          do
            isEOF <- hIsEOF h
            if isEOF then
              return []
              else do
              line <- hGetLine h -- read the line anyway
              case num of
                [] ->  return []
                (x:xs) ->
                  if k == x then do
                    nlines <- hSelectLines' (k+1) h xs
                    return (line : nlines)
                    else if k < x then
                      hSelectLines' (k+1) h nums -- because you ha
                      else
                           return []

------------------------------------------------------------------------------
-- Ex 7: In this exercise we see how a program can be split into a
-- pure part that does all of the work, and a simple IO wrapper that
-- drives the pure logic.
--
-- Implement the function interact' that takes a pure function f of
-- type
--   (String, st) -> (Bool, String, st)
-- and a starting state of type st and returns an IO operation of type
-- IO st
--
-- interact' should read a line from the user, feed the line and the
-- current state to f. f then returns a boolean, a string to print and
-- a new state. The string is printed, and if the boolean is True, we
-- continue running with the new state. If the boolean is False, the
-- execution has ended and the state should be returned.
--
-- Example:
--   *Set11b> interact' counter 1
--   print
--   1
--   inc
--   done
--   inc
--   done
--   print
--   3
--   quit
--   bye bye
--   3
--   *Set11b>

-- This is used in the example above. Don't change it!
counter :: (String,Integer) -> (Bool,String,Integer)
counter ("inc",n)   = (True,"done",n+1)
counter ("print",n) = (True,show n,n)
counter ("quit",n)  = (False,"bye bye",n)

interact' :: ((String,st) -> (Bool,String,st)) -> st -> IO st
interact' f state = do
  instruction <- getLine
  case f (instruction,state) of
    (False,output,st) -> do
      putStrLn output
      return st
    (True,output,st) -> do
      putStrLn output
      interact' f st
