module Day2 where

import qualified Data.Text                     as T
import qualified Data.Text.IO                  as TIO
import           Data.List.Split                ( chunksOf )
import           System.IO                      ( FilePath
                                                , openFile
                                                )


data Program = Run [Int] | Halt [Int] deriving (Show, Eq)

toInts :: Program -> [Int]
toInts (Run  p) = p
toInts (Halt p) = p

readProgramFromFile :: FilePath -> IO Program
readProgramFromFile f =
  Run . map (read . T.unpack) . T.splitOn (T.pack ",") . T.pack <$> readFile f

getOperation :: (Num a, Eq a) => Int -> (a -> a -> a)
getOperation 1 = (+)
getOperation 2 = (*)
getOperation _ =
  error
    "Undefined operation. Valid operations are 1 (addition) and 2 (multiplication)"

readRegister :: Int -> [Int] -> Int
readRegister n prog
  | n >= length prog = error "Program is not long enough to read instruction."
  | otherwise        = prog !! n

readOperation :: Int -> [Int] -> Int
readOperation n = readRegister (n * 4)

readSrc1 :: Int -> [Int] -> Int
readSrc1 n = readRegister (n * 4 + 1)

readSrc2 :: Int -> [Int] -> Int
readSrc2 n = readRegister (n * 4 + 2)

readDest :: Int -> [Int] -> Int
readDest n = readRegister (n * 4 + 3)

writeRegister :: Int -> Int -> [Int] -> [Int]
writeRegister n result prog = take n prog ++ [result] ++ drop (n + 1) prog

executeInstruction :: Int -> Program -> Program
executeInstruction _ halt@(Halt _) = halt
executeInstruction n (Run prog)
  | op == 99  = Halt prog
  | otherwise = Run (writeRegister dest result prog)
 where
  op     = readOperation n prog
  dest   = readDest n prog
  result = f arg1 arg2
  f      = getOperation (readOperation n prog)
  arg1   = readRegister (readSrc1 n prog) prog
  arg2   = readRegister (readSrc2 n prog) prog

runProgram :: Program -> Program
runProgram halt@(Halt _) = halt
runProgram prog          = go 0 prog where
  go :: Int -> Program -> Program
  go n halt@(Halt _) = halt
  go n run@( Run  _) = go (n + 1) $ executeInstruction n run

setNounAndVerb :: Int -> Int -> Program -> Program
setNounAndVerb _ _ halt@(Halt prog) = halt
setNounAndVerb noun verb (Run prog) =
  Run $ take 1 prog ++ [noun, verb] ++ drop 3 prog

partOneOutput =
  runProgram . setNounAndVerb 12 2 <$> readProgramFromFile "data/day2.txt"

data ProgramOutput = ProgramOutput
  { output :: Int
  , noun :: Int
  , verb :: Int
  } deriving Show

runProgramForInputs :: Int -> Int -> Program -> ProgramOutput
runProgramForInputs noun verb prog = ProgramOutput output noun verb where
  output = readRegister 0 $ toInts $ runProgram (setNounAndVerb noun verb prog)

findInputsForOutput :: Int -> Program -> Maybe ProgramOutput
findInputsForOutput output prog | null searchResult = Nothing
                                | otherwise         = Just $ head searchResult
 where
  searchResult = dropWhile
    (not . outputsMatch)
    [ runProgramForInputs n v prog | n <- [0 .. 99], v <- [0 .. 99] ]
  outputsMatch :: ProgramOutput -> Bool
  outputsMatch ProgramOutput { output = o } = o == output

partTwoOutput =
  findInputsForOutput 19690720 <$> readProgramFromFile "data/day2.txt"
