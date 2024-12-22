module Days.D12 where

import Lib ( Dispatch, dispatchWith )
import Lib.Field ( Field, Point )
import Lib.Field qualified as Field
import Control.Monad.Reader ( runReader, Reader, asks )
import Control.Monad.State ( StateT, evalStateT, gets, modify )
import Data.List qualified as List
import Data.Maybe ( fromJust, catMaybes )
import Data.Set ( Set )
import Data.Set qualified as Set
import Linear.V2 ( V2(..) )
import Control.Monad (join, filterM)
import Data.Bifunctor (first, second)

dispatch :: Dispatch
dispatch = dispatchWith part1 part2

part1 :: String -> Int
part1 = sum . fmap tally . plots . parseInput

part2 :: String -> Int
part2 = sum . fmap tally . plots2 . parseInput

type Carry a = StateT (Field Bool) (Reader (Field Char)) a

tally :: V2 Int -> Int
tally (V2 area border) = area * border

plots :: Field Char -> [V2 Int]
plots field
  = flip runReader field . flip evalStateT visited
  $ flip traverse (Field.indices field) plotFrom
  where
  visited = Field.fill (Field.extent field) False

plotFrom :: Point -> Carry (V2 Int)
plotFrom point = do
  tallied <- fmap fromJust . gets $ Field.lookup point
  if tallied then pure 0 else do
    modify $ Field.update point True
    plotType <- fmap fromJust . asks $ Field.lookup point
    let pointsAdjacent = (point +) <$> Field.cardinals
    adjacent <- fmap catMaybes . asks . traverse Field.ilookup $ pointsAdjacent
    let
      plotAdjacent = fst <$> filter ((==plotType) . snd) adjacent
      tallyHere = V2 1 (4 - length plotAdjacent)
    sum . (tallyHere:) <$> traverse plotFrom plotAdjacent

-- TODO maybe: massive refactor lol
type Carry2 a = StateT (Set Point, Field Bool) (Reader (Field Char)) a

plots2 :: Field Char -> [V2 Int]
plots2 field
  = flip runReader field . flip evalStateT (undefined, visited)
  $ traverse plotFrom2 (Field.indices field)
  where
  visited = Field.fill (Field.extent field) False

plotFrom2 :: Point -> Carry2 (V2 Int)
plotFrom2 point = do
  visited <- fromJust . Field.lookup point <$> gets snd
  if visited then pure 0 else do
    modify . first $ const Set.empty
    plot <- List.sort <$> crawlFrom point
    crawlsVisited <- gets snd   -- backup, wiping the field for tallyPlot
    modify . second . const $ Field.fill (Field.extent crawlsVisited) False
    plotTally <- sum <$> traverse tallyPlot plot
    modify . second $ const crawlsVisited   -- restore for later crawls
    pure plotTally

crawlFrom :: Point -> Carry2 [(Point, [Point])]
crawlFrom point = do
  visited <- fromJust . Field.lookup point <$> gets snd
  if visited then pure [] else do
    modify . second $ Field.update point True
    plotType <- fmap fromJust . asks $ Field.lookup point
    let pointsAdjacent = (point +) <$> Field.cardinals
    adjacent <- fmap catMaybes . asks . traverse Field.ilookup $ pointsAdjacent
    let plotAdjacent = fst <$> filter ((==plotType) . snd) adjacent
    ((point, plotAdjacent) :) . join <$> traverse crawlFrom plotAdjacent

tallyPlot :: (Point, [Point]) -> Carry2 (V2 Int)
tallyPlot (point, plotAdjacent) = do
  modify . second $ Field.update point True
  priorBorders <- gets fst
  visitedAdjacent <- flip filterM plotAdjacent
    $ \p -> fromJust . Field.lookup p <$> gets snd
  let
    borderVectors = Set.difference
      (Set.fromList Field.cardinals)
      (Set.fromList $ fmap (subtract point) plotAdjacent)
    uniqueBorders = getUBs point priorBorders visitedAdjacent borderVectors
    tallyHere = V2 1 $ length uniqueBorders
    priorBorders' = Set.union priorBorders (Set.map (+point) borderVectors)
  modify . first . const $ priorBorders'
  pure tallyHere

getUBs :: Point -> Set Point -> [Point] -> Set (V2 Int) -> Set Point
getUBs point priorBorders visitedAdjacent borderVectors
  = Set.filter (null . adjacentToPrior) borderVectors
  where
  adjacentToPrior v = let
    concrete = v + point
    candidates = flip Set.filter priorBorders . flip elem $ case v of
      (V2 _ 0) -> [concrete + Field.north, concrete + Field.south]
      (V2 0 _) -> [concrete + Field.west, concrete + Field.east]
      _ -> []
    adjacentCandidates = Set.fromList $ fmap (v +) visitedAdjacent
    in Set.intersection candidates adjacentCandidates

parseInput :: String -> Field Char
parseInput = Field.parse id
