import Data.Char (isDigit)
import Data.List (findIndex)
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

main = do
  args <- getArgs
  rawInput <- readFile $ head args
  let inputLines = lines rawInput
  print $ part1 inputLines
