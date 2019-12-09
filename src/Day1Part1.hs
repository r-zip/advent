module Day1Part1
  ( moduleMasses
  , moduleFuelRequirement
  )
where

import           Data.Char                      ( toUpper )
import           System.IO                      ( FilePath )

moduleFuelRequirement :: Int -> Int
moduleFuelRequirement x = x `div` 3 - 2

moduleMasses :: FilePath -> IO [Int]
moduleMasses filePath = map read . lines <$> readFile filePath

fuelRequirement :: FilePath -> IO Int
fuelRequirement filePath =
  sum . map moduleFuelRequirement <$> moduleMasses filePath

