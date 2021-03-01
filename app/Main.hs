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
      addIncrToArray :: Int -> Bump -> Int
      addIncrToArray previous bump = bump previous
      -- a will be (previous Population, year, bump)
      -- e will be population
      -- accumArray :: Ix i => (e -> a -> e) -> e -> (i, i) -> [(i, a)] -> Array i e
      firstYear = minBirthYear bdyears
      lastYear = maxDeathYear bdyears
      years = [firstYear .. lastYear]
      -- Initial Array, all populations are zero.
      -- array :: Ix i => (i, i) -> [(i, e)] -> Array i e
      zeroArray = array (firstYear, lastYear) [(i,0) | i <- years]
      accumBump :: Incr -> Array Int Int -> Array Int Int
      accumBump (year, bump) hist =
        let currentPop = hist!year
        in hist // [(year, bump currentPop)]
      -- foldr :: Foldable t => (a -> b -> b) -> b -> t a -> b
      -- Get the array of bumps
      bumpArray = foldr accumBump zeroArray incrs
      -- Now sum the bumps
      sumBump :: Int -> Array Int Int -> Array Int Int
      sumBump year hist | year == firstYear = hist
      sumBump year hist =
        let previousPop = hist!(year - 1)
            bumpBy = hist!year
        in hist // [(year, previousPop + bumpBy)]
      -- populationArray = foldr sumBump bumpArray years
      sumBump2 :: (Int, Int) -> Array Int Int -> Array Int Int
      sumBump2 (firstYear, lastYear) a = sumBump3 [firstYear .. lastYear] a
      sumBump3 :: [Int] -> Array Int Int -> Array Int Int
      sumBump3 [] a = a
      sumBump3 [n] a | n == firstYear = a
      sumBump3 [n] a | n > firstYear =
        let previousPop = a!(n-1)
            bumpBy = a!n
        in a // [(n, previousPop + bumpBy)]
      sumBump3 (n:ns) a | n == firstYear = sumBump3 ns a
      sumBump3 (n:ns) a | n > firstYear =
        let previousPop = a!(n-1)
            bumpBy = a!n
        in sumBump3 ns (a // [(n, previousPop + bumpBy)])
      -- accumArray addIncrToArray 0 (firstYear, lastYear) incrs (Old version)
      populationArray = sumBump2 (firstYear, lastYear) bumpArray
  in populationArray

-- concatMap :: Foldable t => (a -> [b]) -> t a -> [b]

-- Determine min and max year
-- This might not be necessary if we used a Map to store the birth / death counts
-- We will try that later.
maximum' :: (Int, Int) -> Int -> Int -> Array Int Int -> Int
maximum' (low, high) current max hist | current >= high = max
maximum' (low, high) current max hist | hist!current > max =
  maximum' (low, high) (current + 1) (hist!current) hist
maximum' (low, high) current max hist = maximum' (low, high) (current + 1) max hist

-- Get the one of the maximum values from the histogram
maximum :: Array Int Int -> Int
maximum hist = maximum' (bounds hist) (fst (bounds hist)) 0 hist

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
    putStrLn $ "histogram test1 = " ++ show (histogram test1)
    putStrLn $ "histogram test2 = " ++ show (histogram test2)

m :: IO () = main
