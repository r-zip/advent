module Day1Part2 where

import           Day1Part1                      ( moduleFuelRequirement
                                                , moduleMasses
                                                )
import           System.IO                      ( FilePath )

totalModuleFuelRequirement :: Int -> Int
totalModuleFuelRequirement =
  sum . tail . takeWhile (> 0) . iterate moduleFuelRequirement

totalFuelRequirement :: FilePath -> IO Int
totalFuelRequirement filePath =
  sum . map totalModuleFuelRequirement <$> moduleMasses filePath

