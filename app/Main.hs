{-# LANGUAGE DuplicateRecordFields, FlexibleInstances, UndecidableInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}

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

-- The birth / death count problem
-- A tuple of birth and death years.
data BirthDeathYears =
  BirthDeathYears { birthYear :: Int,
                    deathYear :: Int} deriving (Eq, Ord, Show)
-- A year and either a +1 or a -1 although
-- (the data structure will handle any Int => Int function)
type Bump = Int -> Int
type Incr = (Int, Bump)
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

histogram :: [BirthDeathYears] -> Array Int Int
histogram bdyears =
  let minBirthYear :: Int = foldr min 0 (map birthYear bdyears)
      maxDeathYear :: Int = foldr max 0 (map deathYear bdyears)
      -- array :: Ix i => (i, i) -> [(i, e)] -> Array i e
      -- concatMap :: Foldable t => (a -> [b]) -> t a -> [b]
      incrs :: [Incr] = concatMap yearsToIncrs bdyears
      addIncrToArray :: Int -> Bump -> Int
      addIncrToArray n bump = bump n
      -- accumArray :: Ix i => (e -> a -> e) -> e -> (i, i) -> [(i, a)] -> Array i e
      histogram :: Array Int Int =
        accumArray addIncrToArray 0 (minBirthYear, maxDeathYear) incrs
  in histogram

-- Determine min and max years
-- This might not be necessary if we used a Map to store the birth / death counts
-- We will try that later.

-- Convert birth and death years into an array of birth and death counts
birthDeathCounts :: BirthDeathYears -> Array Int Int
birthDeathCounts bdyears =
  undefined

main :: IO ()
main = do
    putStrLn "Hello"



