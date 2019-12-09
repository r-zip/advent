{-# LANGUAGE OverloadedStrings #-}

module Day2 where

import           Data.Either                    ( rights )
import           Data.List                      ( partition )
import           Data.List.Split                ( chunksOf )
import qualified Data.Text                     as T
import qualified Data.Text.IO                  as TIO
import           Data.Text.Read                 ( decimal )
import           System.IO                      ( FilePath )

data Opcode = Add
            | Mul
            | End
            | InvalidOpcode
  deriving (Show, Eq)

data Operation = Operation
  { opcode :: Opcode
  , arg1   :: Int
  , arg2   :: Int
  , dest   :: Int
  } | InvalidOperation deriving (Show, Eq)

data Program = Program [Operation]
             | InvalidProgram
  deriving (Show, Eq)

toOpcode :: Int -> Opcode
toOpcode 1  = Add
toOpcode 2  = Mul
toOpcode 99 = End
toOpcode _  = InvalidOpcode

firstInt :: Int -> Int -> Int
firstInt x y = x

-- if there's no corresponding function, just return the first
toFunction :: Opcode -> (Int -> Int -> Int)
toFunction Add = (+)
toFunction Mul = (*)
toFunction End = firstInt
toFunction InvalidOpcode = firstInt

-- assumes a valid program
readProgram :: T.Text -> [Int]
readProgram = map fst . rights . map decimal . T.splitOn ","

readProgramFromFile :: FilePath -> IO [Int]
readProgramFromFile filePath = readProgram <$> TIO.readFile filePath

toOperation :: [Int] -> Operation
toOperation [99, _, _, _] = Operation End 0 0 0
toOperation [w, x, y, z] = Operation (toOpcode w) x y z
toOperation _            = InvalidOperation

-- TODO: this is unsafe for invalid programs
getOp :: Int -> [Int] -> Operation
getOp n prog = toOperation $ chunksOf 4 prog !! n

executeOp :: Operation -> [Int] -> [Int]
executeOp InvalidOperation prog = prog
executeOp Operation { opcode = opcode, arg1 = arg1, arg2 = arg2, dest = dest } prog
        = take dest prog
                ++ [toFunction opcode (prog !! arg1) (prog !! arg2)]
                ++ drop (dest + 1) prog

takeStep :: Int -> [Int] -> [Int]
takeStep n prog = executeOp (getOp n prog) prog

runProgram :: [Int] -> [Int]
runProgram prog = go 0 prog where
  go :: Int -> [Int] -> [Int]
  go n prog
    | opcode (getOp n prog) == End = prog
    | otherwise = go (n+1) $ takeStep n prog












