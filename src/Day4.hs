module Day4 where

import           Data.List                      ( elemIndices
                                                , nub
                                                )



passwordLength = 6

numToDigit :: Int -> Int -> Int
numToDigit place value = 10 ^ place * value

fromDigits :: [Int] -> Int
fromDigits xs = sum (zipWith numToDigit [0 .. length xs - 1] (reverse xs))

toDigits :: Int -> [Int]
toDigits 0 = [0]
toDigits n = reverse $ go n k []    where
        go :: Int -> Int -> [Int] -> [Int]
        go p 0 res = p : res
        go p q res = go (p - quot * 10 ^ q) (q - 1) (quot : res)
                where quot = p `div` 10 ^ q
        quotient = n `div` 10 ^ k
        k        = floor $ logBase 10 $ fromIntegral n


combinations n = [ [x, y] | x <- [n .. 9], y <- [x .. 9] ]

passwords =
        [ [n1, n2, n3, n4, n5, n6]
        | n1 <- [0 .. 9]
        , n2 <- [n1 .. 9]
        , n3 <- [n2 .. 9]
        , n4 <- [n3 .. 9]
        , n5 <- [n4 .. 9]
        , n6 <- [n5 .. 9]
        ]

day4PartOnePasswords = filter
        (\x ->
                fromDigits x
                        >= 382345
                        && fromDigits x
                        <= 843167
                        && repeatedDigit x
        )
        passwords

day4PartOneOutput = length day4PartOnePasswords

repeatedDigit :: [Int] -> Bool
repeatedDigit [] = False
repeatedDigit xs = or $ zipWith (==) (init xs) (tail xs)

threeOrMoreRepeatedDigits :: [Int] -> Bool
threeOrMoreRepeatedDigits [] = False
threeOrMoreRepeatedDigits xs
        | length xs >= 3 = or
        $ zipWith3 allEqual (init xs) (tail xs) (tail (tail xs))
        | otherwise = False
        where allEqual x y z = x == y && y == z

maxRunLength :: Int -> [Int] -> Int
maxRunLength x [] = 0
maxRunLength x xs = length (filter (== 1) indexDiffs) + 1
    where
        matches    = elemIndices x xs
        indexDiffs = zipWith (-) (tail matches) (init matches)

runLengths :: [Int] -> [(Int, Int)]
runLengths xs = [ (x, maxRunLength x xs) | x <- nub xs ]

partTwoPredicate :: [Int] -> Bool
partTwoPredicate xs = 2 `elem` map snd (runLengths xs)

day4PartTwoPasswords = filter partTwoPredicate day4PartOnePasswords
day4PartTwoOutput = length day4PartTwoPasswords
