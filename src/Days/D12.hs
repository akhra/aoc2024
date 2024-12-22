module Days.D12 where

import Lib ( Dispatch, dispatchWith )
import Lib.Field ( Field, Point, pattern Point )
import Lib.Field qualified as Field
import Control.Monad.Reader ( runReader, Reader, asks )
import Control.Monad.State ( StateT, evalStateT, gets, modify )
import Data.Maybe ( fromJust, catMaybes )
import Linear.V2 ( V2(..) )

dispatch :: Dispatch
dispatch = dispatchWith part1 part2

part1 :: String -> Int
part1 = sum . fmap tally . plots . parseInput

part2 :: String -> Int
part2 = undefined

type Carry a = StateT (Field Bool) (Reader (Field Char)) a

tally :: V2 Int -> Int
tally (V2 area border) = area * border

plots :: Field Char -> [V2 Int]
plots field
  = flip runReader field . flip evalStateT checklist
  $ flip traverse [Point x y | x <- [0..width], y <- [0..height]] plotFrom
  where
  extent@(Point width height) = Field.extent field
  checklist = Field.fill extent False

plotFrom :: Point -> Carry (V2 Int)
plotFrom point = do
  tallied <- fmap fromJust . gets $ Field.lookup point
  if tallied then pure 0 else do
    modify $ Field.update point True
    plotType <- fmap fromJust . asks $ Field.lookup point
    let pointsAdjacent = (point +) <$> Field.cardinals
    adjacent <- fmap catMaybes . asks . traverse Field.ilookup $ pointsAdjacent
    let
      plotAdjacent = fmap fst . filter ((==plotType) . snd) $ adjacent
      tallyHere = V2 1 (4 - length plotAdjacent)
    sum . (tallyHere:) <$> traverse plotFrom plotAdjacent

parseInput :: String -> Field Char
parseInput = Field.parse id
