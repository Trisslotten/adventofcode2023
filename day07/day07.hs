{-# LANGUAGE BlockArguments #-}

import Data.Char (digitToInt)
import Data.List (find, sort, sortBy)
import Data.List.Split (splitOn)
import Data.Maybe (isNothing)
import Data.Ord qualified
import Debug.Trace (trace)
import GHC.Data.Maybe (isJust)
import GHC.OldList (group, maximumBy)
import System.Environment (getArgs)

data Hand = Hand
  { cards :: String,
    bid :: Int
  }
  deriving (Show)

parseInput = map parseHand
  where
    parseHand line =
      Hand
        { cards = take 5 line,
          bid = read $ drop 5 line
        }

countOccurrences list = map (\g -> (head g, length g)) $ group $ sort list

getLargestGroup cards = maximum (map snd $ countOccurrences cards)

isFive :: [Char] -> Bool
isFive cards = 5 == getLargestGroup cards

isFour cards = 4 == getLargestGroup cards

isThree cards = 3 == getLargestGroup cards

isFullHouse cards = do
  let x : y : xs = sortBy (Data.Ord.comparing Data.Ord.Down) (map snd $ countOccurrences cards)
  x == 3 && y == 2

isTwoPair cards = do
  let x : y : xs = sortBy (Data.Ord.comparing Data.Ord.Down) (map snd $ countOccurrences cards)
  x == 2 && y == 2

isPair cards = 2 == getLargestGroup cards

getCardValue 'A' = 14
getCardValue 'K' = 13
getCardValue 'Q' = 12
getCardValue 'J' = 11
getCardValue 'T' = 10
getCardValue x = digitToInt x :: Int

compareEqualStrength leftCards rightCards = do
  let leftValues = map getCardValue leftCards
  let rightValues = map getCardValue rightCards
  let valuePairs = zip leftValues rightValues
  let comparisons = map (uncurry compare) valuePairs
  let Just result = find (/= EQ) comparisons
  result

-- data  Ordering    =  LT | EQ | GT
compareHand :: Hand -> Hand -> Ordering
compareHand lhs rhs = do
  let leftCards = cards lhs
  let rightCards = cards rhs
  if leftCards == rightCards
    then EQ
    else do
      let funcs = [isFive, isFour, isFullHouse, isThree, isTwoPair, isPair]
      let leftEval = map (\f -> f leftCards) funcs
      let rightEval = map (\f -> f rightCards) funcs
      let bestType = find (uncurry (||)) $ zip leftEval rightEval
      case bestType of
        Nothing -> compareEqualStrength leftCards rightCards
        (Just (l, r)) -> if l && r then compareEqualStrength leftCards rightCards else if l then GT else LT

part1 lines = do
  let hands = parseInput lines
  let sorted = sortBy compareHand hands
  let ranked = zip sorted [1 ..]
  let calcPoints (hand, rank) = rank * bid hand
  sum $ map calcPoints ranked

------------------------------------------------------------------------------------------------

getCardValue2 'A' = 14
getCardValue2 'K' = 13
getCardValue2 'Q' = 12
getCardValue2 'T' = 10
getCardValue2 'J' = 1
getCardValue2 x = digitToInt x :: Int

compareOccurrences (_, l) (_, r) = compare l r

compareEqualStrength2 leftCards rightCards = do
  let leftValues = map getCardValue2 leftCards
  let rightValues = map getCardValue2 rightCards
  let valuePairs = zip leftValues rightValues
  let comparisons = map (uncurry compare) valuePairs
  let Just result = find (/= EQ) comparisons
  result

getReplacement occurences = do
  let sorted = sortBy (flip compareOccurrences) occurences
  let best = find (\x -> fst x /= 'J') sorted
  if isJust best
    then do
      let Just result = best
      fst result
    else 'A'

replaceJokers cards = do
  let occurences = countOccurrences cards
  let replacement = getReplacement occurences
  map (\x -> if x == 'J' then replacement else x) cards

compareHand2 :: Hand -> Hand -> Ordering
compareHand2 lhs rhs = do
  let leftCards = cards lhs
  let rightCards = cards rhs
  if leftCards == rightCards
    then EQ
    else do
      let leftReplaced = replaceJokers leftCards
      let rightReplaced = replaceJokers rightCards
      let funcs = [isFive, isFour, isFullHouse, isThree, isTwoPair, isPair]
      let leftEval = map (\f -> f leftReplaced) funcs
      let rightEval = map (\f -> f rightReplaced) funcs
      let bestType = trace (show leftReplaced ++ " " ++ show leftCards) find (uncurry (||)) $ zip leftEval rightEval
      case bestType of
        Nothing -> compareEqualStrength2 leftCards rightCards
        (Just (l, r)) -> if l && r then compareEqualStrength2 leftCards rightCards else if l then GT else LT

part2 lines = do
  let hands = parseInput lines
  let sorted = sortBy compareHand2 hands
  let ranked = zip sorted [1 ..]
  let calcPoints (hand, rank) = rank * bid hand
  sum $ map calcPoints ranked

------------------------------------------------------------------------------------------------

main = do
  args <- getArgs
  rawInput <- readFile $ head args
  let inputLines = lines rawInput
  -- print $ part1 inputLines
  print $ part2 inputLines

-- 251450793 too high