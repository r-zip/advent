module Day5 where

import Text.ParserCombinators.ReadP


isVowel :: Char -> Bool
isVowel char = char `elem` "aeiou"

vowel :: ReadP Char
vowel = satisfy isVowel

atLeastOneVowel :: ReadP String
atLeastOneVowel = many1 vowel

foo = readP_to_S atLeastOneVowel "aouibcdef"

