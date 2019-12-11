{-# LANGUAGE OverloadedStrings #-}
module Day3 where

import qualified Data.Text as T
import qualified Data.Map as M
import qualified Data.Array as A
import System.IO (FilePath)

data Direction = U | D | L | R | InvalidDirection deriving (Show, Eq)
data Distance = Distance Int | InvalidDistance deriving (Show, Eq, Ord)

makeDistance :: T.Text -> Distance
makeDistance s
  | dist < 0 = InvalidDistance
  | otherwise = Distance dist
  where dist :: Int
        dist = read $ T.unpack s

toInt :: Distance -> Int
toInt InvalidDistance = 0
toInt (Distance x) = x

toDirection :: T.Text -> Direction
toDirection "U" = U
toDirection "D" = D
toDirection "L" = L
toDirection "R" = R
toDirection _ = InvalidDirection

data WirePathComponent = InvalidWirePathComponent | WirePathComponent
  { wpDirection :: Direction
  , wpDistance :: Distance
  } deriving (Show, Eq)

type WirePath = [WirePathComponent]
type WirePaths = [WirePath]

parseWirePathComponent :: T.Text -> WirePathComponent
parseWirePathComponent s 
  | dir == InvalidDirection = InvalidWirePathComponent
  | dist == InvalidDistance = InvalidWirePathComponent
  | otherwise = WirePathComponent {wpDirection=dir, wpDistance=dist} 
  where
    dir = toDirection $ T.take 1 s
    dist = makeDistance $ T.drop 1 s

parseWirePath :: T.Text -> WirePath
parseWirePath directions = parseWirePathComponent <$> T.splitOn "," directions

parseWirePaths :: T.Text -> WirePaths
parseWirePaths directions = parseWirePath <$> takeWhile (not . T.null) (T.splitOn "\n" directions)

readWirePathsFromFile :: FilePath -> IO WirePaths
readWirePathsFromFile filePath = parseWirePaths . T.pack <$> readFile filePath

data Extremum = Low | High deriving (Show, Eq)

data Axis = XAxis | YAxis deriving (Show, Eq)

signedMagnitude :: WirePathComponent -> Int
signedMagnitude wpComponent
  | wpDirection wpComponent == InvalidDirection = error "InvalidDirection passed to sign."
  | wpDirection wpComponent == U = 1 * magnitude
  | wpDirection wpComponent == D = -1 * magnitude
  | wpDirection wpComponent == L = -1 * magnitude
  | wpDirection wpComponent == R = 1 * magnitude
  where magnitude = toInt $ wpDistance wpComponent

axis :: Direction -> Axis
axis InvalidDirection = error "InvalidDirection passed to axis."
axis U = YAxis
axis D = YAxis
axis L = XAxis
axis R = XAxis

extremumForAxisAndPath :: Axis -> Extremum -> WirePath -> Int
extremumForAxisAndPath ax extremum path = sum $ filter filterFunc distancesInDirection where
  distancesInDirection = map signedMagnitude $ filter ((== ax) . axis . wpDirection) path
  filterFunc = case extremum of 
                 High -> (> 0)
                 Low -> (< 0)

extremumForAxis :: Axis -> Extremum -> WirePaths -> Int
extremumForAxis ax extremum = maximum . map (extremumForAxisAndPath ax extremum)

data GridSize = GridSize
  { gXMin :: Int
  , gYMin :: Int
  , gXMax :: Int
  , gYMax :: Int
  } deriving (Eq, Show)

gridSize :: WirePaths -> GridSize
gridSize wps = GridSize xmin ymin xmax ymax where
  xmin = extremumForAxis XAxis Low wps
  ymin = extremumForAxis YAxis Low wps
  xmax = extremumForAxis XAxis High wps
  ymax = extremumForAxis YAxis High wps



