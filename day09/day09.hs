{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Redundant bracket" #-}
import System.Environment (getArgs)

diffList :: [Int] -> [Int]
diffList list = zipWith (-) (tail list) list

parseLine :: String -> [Int]
parseLine line = map read $ words line :: [Int]

extrapolate :: ([Int] -> Int) -> (Int -> Int -> Int) -> [Int] -> Int
extrapolate func op list = do
  if all (== 0) list
    then 0
    else do
      let diffs = diffList list
      let subDiff = extrapolate func op diffs
      op (func list) (subDiff)

part1 lines = do
  let parsedLines = map parseLine lines
  let extrapolated = map (extrapolate last (+)) parsedLines
  sum extrapolated

part2 lines = do
  let parsedLines = map parseLine lines
  let extrapolated = map (extrapolate head (-)) parsedLines
  sum extrapolated

main = do
  args <- getArgs
  rawInput <- readFile $ head args
  let inputLines = lines rawInput
  print $ part1 inputLines
  print $ part2 inputLines