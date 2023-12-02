{-# LANGUAGE OverloadedStrings #-}

import Data.Char (digitToInt, isDigit)
import Data.Foldable (maximumBy)
import Data.Function (on)
import Data.List (minimumBy)
import Data.Maybe (isJust)
import Data.Ord (comparing)
import Data.Text (pack, replace, unpack)
import Data.Text.Internal.Search (indices)

getFirstNumber :: [Char] -> Maybe Int
getFirstNumber [] = Nothing
getFirstNumber (x : xs) = if isDigit x then Just $ digitToInt x else getFirstNumber xs

combindFirstAndLast :: [Char] -> Int
combindFirstAndLast line = do
  let Just firstNum = getFirstNumber line
  let Just lastNum = getFirstNumber $ reverse line
  firstNum * 10 + lastNum

part1 inputLines = sum $ map combindFirstAndLast inputLines

findFirstOccurance x str = case indices x str of
  (idx : _) -> Just idx
  _ -> Nothing

maybeLast [] = Nothing
maybeLast xs = Just $ last xs

findLastOccurance x str = maybeLast $ indices x str

maybeInt fn Nothing x = x
maybeInt fn x Nothing = x
maybeInt fn (Just a) (Just b) = Just (fn a b)

findMin str chr line = do
  let maybeStr = findFirstOccurance str line
  let maybeChr = findFirstOccurance chr line
  maybeInt min maybeStr maybeChr

findMax str chr line = do
  let maybeStr = findLastOccurance str line
  let maybeChr = findLastOccurance chr line
  maybeInt max maybeStr maybeChr

getDigit find getBy line = do
  let to_find = [("zero", "0", 0), ("one", "1", 1), ("two", "2", 2), ("three", "3", 3), ("four", "4", 4), ("five", "5", 5), ("six", "6", 6), ("seven", "7", 7), ("eight", "8", 8), ("nine", "9", 9)]
  let mapped = map (\(str, chr, num) -> (find str chr line, num)) to_find
  let filtered = map (\(Just x, num) -> (x, num)) $ filter (\(x, _) -> isJust x) mapped
  snd $ getBy (comparing fst) filtered

combindFirstAndLastPart2 lineString = do
  let line = pack lineString
  let first = getDigit findMin minimumBy line
  let last = getDigit findMax maximumBy line
  first * 10 + last

part2 inputLines = sum $ map combindFirstAndLastPart2 inputLines

main = do
  rawInput <- readFile "day01/input.txt"
  let inputLines = lines rawInput
  print $ part1 inputLines
  print $ part2 inputLines
