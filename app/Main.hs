{-# LANGUAGE DuplicateRecordFields, FlexibleInstances, UndecidableInstances #-}

module Main where

import Control.Monad
import Data.Array
import Data.Bits
import Data.List
-- import Data.List.Split
-- import Data.Set
-- import Data.Text
import Debug.Trace
import System.Environment
import System.IO

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

-- The birth / death count problem
-- A tuple of birth and death years.
data BirthDeathYears =
  BirthDeathYears { birthYear :: Int,
                    deathYear :: Int} deriving (Eq, Ord, Show)
-- A year and either a +1 or a -1 although
-- (the data structure will handle any Int => Int function)
type Incr = (Int, Int -> Int)
plus1 :: Int -> Int
plus1 = (+1)
minus1 :: Int -> Int
minus1 x = x - 1

-- Convert BirthDeathYears to BirthDeathInc
-- (1976, 2012) becomes [(1976, \x -> x + 1), (2012, \x -> x - 1)]
-- A tuple of birth and death counts for a given year.
yearsToIncrs :: BirthDeathYears -> [Incr]
yearsToIncrs BirthDeathYears { birthYear = b, deathYear = d } = [(b, plus1), (d, minus1)]

data BirthDeathCounts =
  BirthDeathCounts { birthCount :: Int,
                     deathCount :: Int } deriving (Eq, Ord, Show)
-- A bunch of people with birth and death dates.
type Population = [ BirthDeathYears ]
-- An array with the number of births and deaths in each year
type Histogram = Array BirthDeathCounts

-- Determine min and max years
-- This might not be necessary if we used a Map to store the birth / death counts
-- We will try that later.
minMaxYears :: [BirthDeathYears] -> (Int, Int)
minMaxYears bdyears =
  let minBirthYear = foldr min 0 (map birthYear bdyears)
      maxDeathYear = foldr max 0 (map deathYear bdyears)
  in (minBirthYear, maxDeathYear)

-- Convert birth and death years into an array of birth and death counts
birthDeathCounts :: BirthDeathYears -> Array Int Int
birthDeathCounts bdyears =
  undefined

main :: IO ()
main = do
    let as = stringAnagram ["abc", "abcabcjunk", "bac", "knuj"] ["abc", "junk", "kunj", "cba"]
    putStrLn (show as)



