{-# LANGUAGE OverloadedStrings #-}
module Day3 where

import qualified Data.Text                     as T
import qualified Data.Array                    as A
import           System.IO                      ( FilePath )
import           Data.List                      ( intersect )
import           Data.Set                       ( fromList, intersection, toList )

data Direction = U | D | L | R | InvalidDirection deriving (Show, Eq)

makeDistance :: T.Text -> Int
makeDistance = read . T.unpack

toDirection :: T.Text -> Direction
toDirection "U" = U
toDirection "D" = D
toDirection "L" = L
toDirection "R" = R
toDirection _   = InvalidDirection

data WirePathComponent = InvalidWirePathComponent | WirePathComponent
  { wpDirection :: Direction
  , wpDistance :: Int
  } deriving (Show, Eq)

type WirePath = [WirePathComponent]
type WirePaths = [WirePath]

parseWirePathComponent :: T.Text -> WirePathComponent
parseWirePathComponent s
  | dir == InvalidDirection = InvalidWirePathComponent
  | otherwise = WirePathComponent { wpDirection = dir, wpDistance = dist }
 where
  dir  = toDirection $ T.take 1 s
  dist = makeDistance $ T.drop 1 s

parseWirePath :: T.Text -> WirePath
parseWirePath directions = parseWirePathComponent <$> T.splitOn "," directions

parseWirePaths :: T.Text -> WirePaths
parseWirePaths directions =
  parseWirePath <$> takeWhile (not . T.null) (T.splitOn "\n" directions)

readWirePathsFromFile :: FilePath -> IO WirePaths
readWirePathsFromFile filePath = parseWirePaths . T.pack <$> readFile filePath

coordinates :: WirePath -> [(Int, Int)]
coordinates = go [(0, 0)] where
  go coords [] = tail $ reverse coords
  go coords (wpc : wpcs) =
    go (reverse (addToCoordinates wpc (head coords)) ++ coords) wpcs
  addToCoordinates WirePathComponent { wpDirection = dir, wpDistance = dist } (x, y)
    = case dir of
      U -> [ (x, y + k) | k <- [1 .. dist] ]
      D -> [ (x, y - k) | k <- [1 .. dist] ]
      L -> [ (x - k, y) | k <- [1 .. dist] ]
      R -> [ (x + k, y) | k <- [1 .. dist] ]

intersections :: WirePaths -> [(Int, Int)]
intersections []         = []
intersections [wp1, wp2] = toList $ fromList (coordinates wp1) `intersection` fromList (coordinates wp2)

distanceToNearestIntersection :: [(Int, Int)] -> Int
distanceToNearestIntersection =
  minimum . filter (/= 0) . map (\(x, y) -> abs x + abs y)

partOneOutput :: IO Int
partOneOutput = distanceToNearestIntersection . intersections <$> readWirePathsFromFile "data/day3.txt"
