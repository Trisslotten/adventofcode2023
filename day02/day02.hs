{-# LANGUAGE OverloadedStrings #-}

import Data.IntMap (mapAccum, mapWithKey)
import Data.List (elemIndex, findIndex, splitAt)
import Data.List.Split (splitOn)
import Data.Map qualified as Map

unwrap (Just x) = x

maxCubeCount "red" = 12
maxCubeCount "green" = 13
maxCubeCount "blue" = 14

containsPossibleCubes cubes = do
  let [count, color] = splitOn " " cubes
  read count <= maxCubeCount color

roundIsPossible round = do
  let picks = splitOn ", " round
  all containsPossibleCubes picks

gameIsPossible :: [Char] -> (Bool, Int)
gameIsPossible line = do
  let [number, game] = splitOn ": " line
  let rounds = splitOn "; " game
  (all roundIsPossible rounds, read number)

pickValue (True, value) = value
pickValue (False, value) = 0

part1 rawLines = do
  let games = map (drop 5) rawLines
  let possibilities = map gameIsPossible games
  let pickedValues = map pickValue possibilities
  sum pickedValues

----------------------------------------------------------------

getDefaultMap = Map.fromList [("red", 0), ("green", 0), ("blue", 0)]

parsePick :: [Char] -> ([Char], Int)
parsePick pick = do
  let [count, color] = splitOn " " pick
  (color, read count)

handleRound round colorMap = do
  let picks = splitOn ", " round
  let colorCounts = map parsePick picks
  let roundMap = foldl (flip updateIfBigger) colorMap colorCounts
  roundMap

updateIfBigger :: (Ord k) => (k, Int) -> Map.Map k Int -> Map.Map k Int
updateIfBigger (key, newValue) = Map.alter alterFunction key
  where
    alterFunction :: Maybe Int -> Maybe Int
    alterFunction existingValue =
      case existingValue of
        Just oldValue -> Just (max oldValue newValue)
        Nothing -> Nothing

handleGame line = do
  let [number, game] = splitOn ": " line
  let rounds = splitOn "; " game
  let defaultMap = getDefaultMap
  let roundsMap = foldl (flip handleRound) defaultMap rounds
  roundsMap

calculateGamePower roundsMap = product $ Map.elems roundsMap

part2 :: [[Char]] -> Int
part2 rawLines = do
  let games = map (drop 5) rawLines
  let roundsMaps = map handleGame games
  let powers = map calculateGamePower roundsMaps
  sum powers

main = do
  rawInput <- readFile "day02/input.txt"
  let inputLines = lines rawInput
  print $ part1 inputLines
  print $ part2 inputLines
