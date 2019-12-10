module Day2 where

import qualified Data.Text                     as T
import qualified Data.Text.IO                  as TIO
import           Data.List.Split                ( chunksOf )
import           System.IO                      ( FilePath
                                                , openFile
                                                )


-- TODO: should have statuses for not yet run, executed successfully, failed
data ExecStatus = Run | Halt deriving (Eq, Show)

data Program =
  Program {pLength :: Int, pContents :: [Int], pExecStatus :: ExecStatus} deriving (Show, Eq)

everyNth :: Int -> [a] -> [a]
everyNth n xs = case drop (n - 1) xs of
  (y : ys) -> y : everyNth n ys
  []       -> []

makeProgram :: [Int] -> Program
makeProgram prog | 99 `notElem` opcodes = Program progLength prog Halt
                 | otherwise            = Program progLength prog Run
 where
  opcodes    = everyNth 4 (tail prog)
  progLength = length prog

readProgramFromFile :: FilePath -> IO Program
readProgramFromFile f =
  makeProgram
    .   map (read . T.unpack)
    .   T.splitOn (T.pack ",")
    .   T.pack
    <$> readFile f

getOperation :: (Num a, Eq a) => Int -> (a -> a -> a)
getOperation 1 = (+)
getOperation 2 = (*)
getOperation _ =
  error
    "Undefined operation. Valid operations are 1 (addition) and 2 (multiplication)"

-- -- TODO: instead of checking length here, add length field to program datatype
readRegister :: Int -> Program -> Int
readRegister n prog
  | n >= pLength prog = error "Program is not long enough to read instruction."
  | otherwise         = pContents prog !! n

readOperation :: Int -> Program -> Int
readOperation n = readRegister (n * 4)

readSrc1 :: Int -> Program -> Int
readSrc1 n = readRegister (n * 4 + 1)

readSrc2 :: Int -> Program -> Int
readSrc2 n = readRegister (n * 4 + 2)

readDest :: Int -> Program -> Int
readDest n = readRegister (n * 4 + 3)

writeRegister :: Int -> Int -> Program -> Program
writeRegister n result prog =
  makeProgram $ take n progContents ++ [result] ++ drop (n + 1) progContents
  where progContents = pContents prog

executeInstruction :: Int -> Program -> Program
executeInstruction _ halt@(Program _ _ Halt) = halt
executeInstruction n run@(Program len prog Run)
  | op == 99  = Program len prog Halt
  | otherwise = writeRegister dest result run
 where
  op     = readOperation n run
  dest   = readDest n run
  result = f arg1 arg2
  f      = getOperation (readOperation n run)
  arg1   = readRegister (readSrc1 n run) run
  arg2   = readRegister (readSrc2 n run) run

runProgram :: Program -> Program
runProgram halt@(Program _ _ Halt) = halt
runProgram prog                    = go 0 prog where
  go :: Int -> Program -> Program
  go n halt@(Program _ _ Halt) = halt
  go n run@( Program _ _ Run ) = go (n + 1) $ executeInstruction n run

setNounAndVerb :: Int -> Int -> Program -> Program
setNounAndVerb _ _ halt@(Program _ _ Halt) = halt
setNounAndVerb noun verb (Program _ prog Run) =
  makeProgram $ take 1 prog ++ [noun, verb] ++ drop 3 prog

partOneOutput =
  runProgram . setNounAndVerb 12 2 <$> readProgramFromFile "data/day2.txt"

data ProgramOutput = ProgramOutput
  { output :: Int
  , noun :: Int
  , verb :: Int
  } deriving Show

runProgramForInputs :: Int -> Int -> Program -> ProgramOutput
runProgramForInputs noun verb prog = ProgramOutput output noun verb
  where output = readRegister 0 $ runProgram (setNounAndVerb noun verb prog)

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
