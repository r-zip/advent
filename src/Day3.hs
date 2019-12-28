{-# LANGUAGE OverloadedStrings #-}
module Day3 where

import qualified Data.Map                      as M
import qualified Data.Text                     as T
import           System.IO                      ( FilePath )
import           Data.Set                       ( fromList
                                                , intersection
                                                , toList
                                                )
import           Data.List                      ( nub
                                                , find
                                                )
import           Data.List.Extra                ( minimumOn )
import           Data.Tuple.Extra               ( fst3
                                                , snd3
                                                , thd3
                                                )
import           Data.Hashable                  ( Hashable
                                                , hash
                                                , hashWithSalt
                                                )
import           Control.Monad                  ( liftM )

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

generateWirePath :: WirePathComponent -> (Int, Int) -> [(Int, Int)]
generateWirePath WirePathComponent { wpDirection = dir, wpDistance = dist } (x, y)
  = case dir of
    U -> [ (x, y + k) | k <- [1 .. dist] ]
    D -> [ (x, y - k) | k <- [1 .. dist] ]
    L -> [ (x - k, y) | k <- [1 .. dist] ]
    R -> [ (x + k, y) | k <- [1 .. dist] ]

generateWirePathWithDistances
  :: WirePathComponent -> (Int, Int, Int) -> [(Int, Int, Int)]
generateWirePathWithDistances WirePathComponent { wpDirection = dir, wpDistance = dist } (x, y, d)
  = case dir of
    U -> [ (x, y + k, d + k) | k <- [1 .. dist] ]
    D -> [ (x, y - k, d + k) | k <- [1 .. dist] ]
    L -> [ (x - k, y, d + k) | k <- [1 .. dist] ]
    R -> [ (x + k, y, d + k) | k <- [1 .. dist] ]

coordinates :: WirePath -> [(Int, Int, Int)]
coordinates = go [(0, 0, 0)] where
  go coords []           = tail $ reverse coords
  go coords (wpc : wpcs) = go
    (reverse (generateWirePathWithDistances wpc (head coords)) ++ coords)
    wpcs

cToXYD :: (Int, Int, Int) -> ((Int, Int), Int)
cToXYD c = ((fst3 c, snd3 c), thd3 c)

coordMap :: [((Int, Int), Int)] -> M.Map (Int, Int) Int
coordMap = M.fromListWith min

coordMaps :: WirePaths -> [M.Map (Int, Int) Int]
coordMaps = map (M.fromListWith min . map cToXYD . coordinates)

minDistance :: ((Int, Int), Int) -> ((Int, Int), Int) -> ((Int, Int), Int)
minDistance x y = if snd x <= snd y then x else y

day3PartTwoOutput = foldl1 minDistance . M.toList <$> coordIntersection where
  coordIntersection = M.intersectionWith (+) <$> firstMap <*> secondMap
  wirePaths         = readWirePathsFromFile "data/day3.txt"
  maps              = coordMaps <$> wirePaths
  firstMap          = head <$> maps
  secondMap         = head . tail <$> maps
