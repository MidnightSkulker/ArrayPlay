{-# LANGUAGE DuplicateRecordFields, FlexibleInstances, UndecidableInstances #-}

module Main where

import Control.Monad
import Data.Array
import Data.Bits
import Data.List
import Data.List.Split
import Data.Set
import Data.Text
import Debug.Trace
import System.Environment
import System.IO
import System.IO.Unsafe
--
-- Complete the 'stringAnagram' function below.
--
-- The function is expected to return an INTEGER_ARRAY.
-- The function accepts following parameters:
--  1. STRING_ARRAY dictionary
--  2. STRING_ARRAY query
--

-- Determine anagram count for a string from a dictionary
-- We are assuming sorted strings, so equality is our test for being an anagram.
anagramCounts :: String -> [String] -> Int
anagramCounts s = Data.List.length . Data.List.findIndices (s ==)

-- Sort the each of strings in a list of strings, so that each string has letters
-- in alphabetical order.
sortStrings :: [String] -> [String]
sortStrings = Data.List.map Data.List.sort

-- Start by sorting each string in the dictionary, and then sorting the dictionary.
-- Next sort each string in the query.
-- We can now determine if the dictionary entry is an anagram of the query by equality test.
stringAnagram :: [String] -> [String] -> [Int]
stringAnagram dictionary query =
  let sortedDictionary = sortStrings dictionary
      sortedQuery = sortStrings query
  in Data.List.map (\s -> anagramCounts s sortedDictionary) (sortedQuery)
    
main :: IO ()
main = do
    let as = stringAnagram ["abc", "abcabcjunk", "bac", "knuj"] ["abc", "junk", "kunj", "cba"]
    putStrLn (show as)



