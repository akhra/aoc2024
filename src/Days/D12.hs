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

plots2 :: Field Char -> [V2 Int]
plots2 field
  = flip runReader field . flip evalStateT (undefined, visited)
  $ traverse plotFrom2 (Field.indices field)
  where
  visited = Field.fill (Field.extent field) False

type Carry2 a = StateT (Set Point, Field Bool) (Reader (Field Char)) a

getVisited :: Point -> Carry2 Bool
getVisited point = fromJust . Field.lookup point <$> gets snd

setVisited :: Point -> Carry2 ()
setVisited point = modify . second $ Field.update point True

wipeVisited :: Field Bool -> Carry2 ()
wipeVisited = modify . second . const

modPlot :: (Set Point -> Set Point) -> Carry2 ()
modPlot = modify . first

plotFrom2 :: Point -> Carry2 (V2 Int)
plotFrom2 point = do
  visited <- getVisited point   -- inside a plot we've alrady crawled?
  if visited then pure 0 else do
    modPlot $ const Set.empty   -- new plot
    plot <- List.sort <$> crawlFrom point   -- sort avoids border logic issue
    crawlsVisited <- gets snd   -- backup; wiping the field for tallyPlot
    wipeVisited $ Field.fill (Field.extent crawlsVisited) False
    plotTally <- sum <$> traverse tallyPlot plot
    wipeVisited crawlsVisited   -- restore for later crawls
    pure plotTally

crawlFrom :: Point -> Carry2 [(Point, [Point])]
crawlFrom point = do
  visited <- getVisited point   -- point already seen via different neighbor?
  if visited then pure [] else do
    setVisited point  -- remember we've been here
    -- what letter is this plot?
    plotType <- fmap fromJust . asks $ Field.lookup point
    -- get coodinates of adjacent cells
    let pointsAdjacent = (point +) <$> Field.cardinals
    -- look up the letters for each of them, tupled with the coordinates
    -- (catMaybes excludes out-of-bounds)
    adjacent <- fmap catMaybes . asks . traverse Field.ilookup $ pointsAdjacent
    -- filter to cells which share this plot's letter; keep only coordinates
    let plotAdjacent = fst <$> filter ((==plotType) . snd) adjacent
    -- record this point in the plot list, plus its plot neighbors for later,
    --   and iterate this crawl through each of those neighbors.
    ((point, plotAdjacent) :) . join <$> traverse crawlFrom plotAdjacent

tallyPlot :: (Point, [Point]) -> Carry2 (V2 Int)
tallyPlot (point, plotAdjacent) = do
  -- note: separate visit status from crawl!
  setVisited point  -- here, this is used for border tracking
  priorBorders <- gets fst  -- borders seen from visited cells in this plot
  -- adjacent cells within the plot which have already been visited
  visitedAdjacent <- filterM getVisited plotAdjacent
  let
    -- cardinal vectors which do NOT point to adjacent plots, are borders
    borderVectors = Set.difference
      (Set.fromList Field.cardinals)
      (Set.fromList $ fmap (subtract point) plotAdjacent)
    -- ...but we need to skip borders contiguous with those already seen.
    --   this is why the plot was sorted: the raw crawl may visit two points
    --   on a contiguous border before points *between* them, breaking
    --   continuity detection. (getUBs defined below.)
    uniqueBorders = getUBs point priorBorders visitedAdjacent borderVectors
  -- add all this cell's borders to the seen history.
  modPlot . Set.union $ Set.map (+point) borderVectors
  pure $ V2 1 (length uniqueBorders)

getUBs :: Point -> Set Point -> [Point] -> Set (V2 Int) -> Set Point
getUBs point priorBorders visitedAdjacent borderVectors
  -- filter out any borders colinear with one from a previously visited cell
  = Set.filter (null . adjacentToPrior) borderVectors
  where
  -- for each border, throw it away if...
  adjacentToPrior v = let
    -- we came here with just directional vectors; convert to a cell index
    concrete = v + point
    -- get the two diagonals aligned with the cardinal direction of the vector.
    -- find any prior borders recorded at those points.
    candidates = flip Set.filter priorBorders . flip elem $ case v of
      (V2 _ 0) -> [concrete + Field.north, concrete + Field.south]
      (V2 0 _) -> [concrete + Field.west, concrete + Field.east]
      _ -> undefined  -- impossible case, but it avoids a compiler warning lol
    -- translate the list of visited adjacent cells toward the vector.
    --   if they had a border on that side, they now occupy that space.
    adjacentCandidates = Set.fromList $ fmap (v +) visitedAdjacent
    -- compare them to the candidate borders, ensuring they're actually
    --   colinear and not the inside of a corner. this is our filter set.
    in Set.intersection candidates adjacentCandidates

parseInput :: String -> Field Char
parseInput = Field.parse id
