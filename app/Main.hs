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

minBirthYear :: [BirthDeathYears] -> Int
minBirthYear bdyears = foldr min (maxBound :: Int) (map birthYear bdyears)

maxDeathYear :: [BirthDeathYears] -> Int
maxDeathYear bdyears = foldr max 0 (map deathYear bdyears)

histogram :: [BirthDeathYears] -> Array Int Int
histogram bdyears =
  let incrs :: [Incr] = concatMap yearsToIncrs bdyears
      -- addIncrToArray :: Int -> Bump -> Int
      -- addIncrToArray previous bump = bump previous
      -- a will be (previous Population, year, bump)
      -- e will be population
      -- accumArray :: Ix i => (e -> a -> e) -> e -> (i, i) -> [(i, a)] -> Array i e
      firstYear = minBirthYear bdyears
      lastYear = maxDeathYear bdyears
      years = [firstYear .. lastYear]
      -- Initial Array, all populations are zero.
      -- array :: Ix i => (i, i) -> [(i, e)] -> Array i e
      zeroArray = array (firstYear, lastYear) [(i,0) | i <- years]
      accumBump :: Array Int Int -> Incr -> Array Int Int
      accumBump hist (year, bump) = hist // [(year, bump (hist!year))]
      -- foldr :: Foldable t => (a -> b -> b) -> b -> t a -> b
      -- foldl :: (b -> a -> b) -> b -> t a -> b
      -- Get the array of bumps
      bumpArray = foldl accumBump zeroArray incrs
      -- Now sum the bumps
      sumBumpL :: Array Int Int -> Int -> Array Int Int
      sumBumpL hist year | year == firstYear = hist
      sumBumpL hist year = hist // [(year, hist!(year-1) + hist!year)]
      populationArrayL = foldl sumBumpL bumpArray years
  in populationArrayL

-- Determine min and max year
-- This might not be necessary if we used a Map to store the birth / death counts
-- We will try that later.
maximum' :: (Int, Int) -> Int -> Int -> Array Int Int -> Int
maximum' (low, high) current max hist | current >= high = max
maximum' (low, high) current max hist | hist!current > max =
  maximum' (low, high) (current + 1) (hist!current) hist
maximum' (low, high) current max hist = maximum' (low, high) (current + 1) max hist

-- Get the one of the maximum values from the histogram
maximumPop :: Array Int Int -> Int
maximumPop hist = maximum' (bounds hist) (fst (bounds hist)) 0 hist

person1 :: BirthDeathYears = BirthDeathYears { birthYear = 1976, deathYear = 2012 }
person2 :: BirthDeathYears = BirthDeathYears { birthYear = 1977, deathYear = 2011 }
person3 :: BirthDeathYears = BirthDeathYears { birthYear = 1978, deathYear = 2014 }
person4 :: BirthDeathYears = BirthDeathYears { birthYear = 1979, deathYear = 1980 }
person5 :: BirthDeathYears = BirthDeathYears { birthYear = 1976, deathYear = 2012 }
person6 :: BirthDeathYears = BirthDeathYears { birthYear = 1976, deathYear = 1982 }
person7 :: BirthDeathYears = BirthDeathYears { birthYear = 1976, deathYear = 1980 }

test1 :: [BirthDeathYears] = [person1, person2, person3, person4, person5]
test2 :: [BirthDeathYears] = [person6, person7]

main :: IO ()
main = do
    let hist1 = histogram test1
        hist2 = histogram test2
    putStrLn $ "histogram test1 = " ++ show hist1
    putStrLn $ "maximum test1 = " ++ show (maximumPop hist1)
    putStrLn $ "histogram test2 = " ++ show hist2
    putStrLn $ "maximum test2 = " ++ show (maximumPop hist2)

m :: IO () = main
