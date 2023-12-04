import Data.Char (isDigit)
import Data.List (findIndex)
import Data.Map qualified as Map
import Data.Maybe (isJust)
import Debug.Trace (trace)
import System.Environment (getArgs)

numberIsPart (x, y) numberSize engine = do
  let leftRight = ((engine !! y) !! (x - 1)) : [(engine !! y) !! (x + numberSize)]
  let upperSubStr = take (numberSize + 2) . drop (x - 1) $ engine !! (y - 1)
  let lowerSubStr = take (numberSize + 2) . drop (x - 1) $ engine !! (y + 1)
  let surrounding = upperSubStr ++ leftRight ++ lowerSubStr
  let isPartChar chr = (chr /= '.') && (not . isDigit) chr
  any isPartChar surrounding

enlargeEngine engine = do
  let (width, height) = (length $ head engine, length engine)
  let horizontal = replicate width '.'
  let engine2 = [horizontal] ++ engine ++ [horizontal]
  let engine3 = map (\line -> "." ++ line ++ ".") engine2
  engine3

data State = State
  { x :: Int,
    y :: Int,
    numberSum :: Int,
    engine :: [[Char]],
    width :: Int,
    height :: Int
  }
  deriving (Show)

setPos (x, y) state = state {x = x, y = y}

addNumber number state = state {numberSum = number + numberSum state}

gotoNextLine state = setPos (0, y state + 1) state

getNextNumberStart state = do
  if y state >= height state
    then Nothing
    else do
      let currentLine = engine state !! y state
      let nextNumIndex = findIndex isDigit $ drop (x state) currentLine
      case nextNumIndex of
        Just index -> Just $ setPos (index + x state, y state) state
        Nothing -> getNextNumberStart $ gotoNextLine state

getFinalState oldState = do
  let maybeState = getNextNumberStart oldState
  maybe oldState continue maybeState
  where
    continue state = do
      let currentLine = engine state !! y state
      let restOfLine = drop (x state) currentLine
      let (numberStr, _) = span isDigit restOfLine
      let (xp, yp) = (x state, y state)
      let numberLen = length numberStr
      let number = if numberIsPart (xp, yp) numberLen (engine state) then read numberStr else 0
      let nextState = (addNumber number . setPos (xp + numberLen, yp)) state
      getFinalState nextState

part1 input = do
  let engine = enlargeEngine input
  let (width, height) = (length $ head engine, length engine)
  let state = State {x = 0, y = 1, numberSum = 0, engine = engine, width = width, height = height}
  let finalState = getFinalState state
  numberSum finalState

-------------------------------------------------------------------------------------

data State2 = State2
  { state :: State,
    gearMap :: Map.Map (Int, Int) [Int]
  }
  deriving (Show)

hasIndex (Nothing, Nothing, Nothing) = False
hasIndex _ = True

getIndex (x, y) (Just upperIndex, Nothing, Nothing) = Just (x + upperIndex - 1, y - 1)
getIndex (x, y) (Nothing, Just middleIndex, Nothing) = Just (x + middleIndex - 1, y)
getIndex (x, y) (Nothing, Nothing, Just lowerIndex) = Just (x + lowerIndex - 1, y + 1)

numberIsPart2 :: (Int, Int) -> Int -> [[Char]] -> Maybe (Int, Int, Char)
numberIsPart2 (x, y) numberSize engine = do
  let upper = take (numberSize + 2) . drop (x - 1) $ engine !! (y - 1)
  let middle = take (numberSize + 2) . drop (x - 1) $ engine !! y
  let lower = take (numberSize + 2) . drop (x - 1) $ engine !! (y + 1)
  let isPartChar chr = (chr /= '.') && (not . isDigit) chr
  let inUpper = findIndex isPartChar upper
  let inMiddle = findIndex isPartChar middle
  let inLower = findIndex isPartChar lower
  let exists = isJust inUpper || isJust inMiddle || isJust inLower
  if exists
    then do
      let Just (px, py) = getIndex (x, y) (inUpper, inMiddle, inLower)
      let chr = (engine !! py) !! px
      Just (px, py, chr)
    else Nothing

addToMapList (key, newValue) = Map.alter alterFunction key
  where
    alterFunction existingValue =
      case existingValue of
        Just oldValue -> Just (newValue : oldValue)
        Nothing -> Just [newValue]

updateMap mapArg (Just (xp, yp, '*')) number = addToMapList ((xp, yp), number) mapArg
updateMap mapArg (Just (xp, yp, chr)) number = mapArg
updateMap mapArg Nothing number = mapArg

getFinalState2 :: State2 -> State2
getFinalState2 oldState2 = do
  let maybeState = getNextNumberStart $ state oldState2
  maybe oldState2 continue maybeState
  where
    continue state = do
      let myMap = gearMap oldState2
      let currentLine = engine state !! y state
      let restOfLine = drop (x state) currentLine
      let (numberStr, _) = span isDigit restOfLine
      let (xp, yp) = (x state, y state)
      let numberLen = length numberStr
      let number = read numberStr :: Int
      let result = numberIsPart2 (xp, yp) numberLen (engine state)
      let updatedMap = trace (show result) updateMap myMap result number
      let nextState = setPos (xp + numberLen, yp) state
      let newState2 = oldState2 {state = nextState, gearMap = updatedMap}
      getFinalState2 newState2

part2 input = do
  let engine = enlargeEngine input
  let (width, height) = (length $ head engine, length engine)
  let state2 =
        State2
          { state = State {x = 0, y = 1, numberSum = 0, engine = engine, width = width, height = height},
            gearMap = Map.empty
          }
  let finalState2 = getFinalState2 state2
  let values = filter (\x -> length x == 2) $ Map.elems $ gearMap finalState2
  sum $ map product values

-------------------------------------------------------------------------------------

main = do
  args <- getArgs
  rawInput <- readFile $ head args
  let inputLines = lines rawInput
  print $ part1 inputLines
  print $ part2 inputLines
