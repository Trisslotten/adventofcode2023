import System.Environment (getArgs)
import Data.List.Split (splitOn)
part1 lines = lines

part2 lines = lines

main = do
  args <- getArgs
  rawInput <- readFile $ head args
  let inputLines = lines rawInput
  print $ part1 inputLines
  print $ part2 inputLines
