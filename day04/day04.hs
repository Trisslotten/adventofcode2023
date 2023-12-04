import Data.List (intersect)
import Data.List.Split (splitOn)
import System.Environment (getArgs)

calcPoints 0 = 0
calcPoints numberCount = 2 ^ (numberCount - 1)

getCardWins line = do
  let cardStr = splitOn ": " line !! 1
  let [winningNumbersStr, myNumbersStr] = splitOn " | " cardStr
  let parseNumbers numbersStr = map read $ filter (not . null) $ splitOn " " numbersStr :: [Int]
  let winningNumbers = parseNumbers winningNumbersStr
  let myNumbers = parseNumbers myNumbersStr
  let intersection = winningNumbers `intersect` myNumbers
  length intersection

part1 lines = sum $ map (calcPoints . getCardWins) lines

addNumberToSubrange start len number list = do
  let beginning = take start list
  let middle = map (+ number) (take len (drop start list))
  let end = drop (start + len) list
  beginning ++ middle ++ end

calculateCardCounts winTable cardCounts = do
  getFinalCardCount 0 cardCounts
  where
    getFinalCardCount index cardCounts =
      if index == length cardCounts
        then cardCounts
        else do
          let nextIndex = index + 1
          let len = winTable !! index
          let number = cardCounts !! index
          let updatedCardCounts = addNumberToSubrange nextIndex len number cardCounts
          getFinalCardCount nextIndex updatedCardCounts

part2 lines = do
  let winTable = map getCardWins lines
  let initialCardCount = replicate (length winTable) 1
  sum $ calculateCardCounts winTable initialCardCount

main = do
  args <- getArgs
  rawInput <- readFile $ head args
  let inputLines = lines rawInput
  print $ part1 inputLines
  print $ part2 inputLines
