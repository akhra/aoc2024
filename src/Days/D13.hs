{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
module Days.D13 where

import Lib ( Dispatch, dispatchWith, both, determinant )
import Lib.Point ( Point, pattern Point )
import Data.Char qualified as Char
import Data.List qualified as List
import Data.List.Split qualified as List
import Data.Maybe ( catMaybes )
import Control.Monad ( guard )

dispatch :: Dispatch
dispatch = dispatchWith part1 part2

part1 :: String -> Int
part1 = sum . catMaybes . fmap cramer . parseInput

part2 :: String -> Int
part2 = let offsetPrize cm = cm{prize = cm.prize + 10000000000000}
  in sum . catMaybes . fmap (cramer . offsetPrize) . parseInput

data ClawMachine = ClawMachine
  { buttonA :: Point
  , buttonB :: Point
  , prize   :: Point
  } deriving Show

cramer :: ClawMachine -> Maybe Int
cramer ClawMachine{..} = do
  let det  = determinant buttonA buttonB
  guard $ det /= 0
  let
    detA = determinant prize   buttonB
    detB = determinant buttonA prize
    (a, aRem) = detA `divMod` det
    (b, bRem) = detB `divMod` det
  guard $ aRem == 0 && bRem == 0
  pure $ a*3 + b


parseInput :: String -> [ClawMachine]
parseInput
  = fmap parseClawMachine
  . List.chunksOf 3
  . filter (/= "")
  . lines
  where
  parseClawMachine = let package [a, b, p] = ClawMachine a b p
    in package . fmap parseButtonOrPrize
  parseButtonOrPrize
    -- extra lazy since there are no negative inputs
    = uncurry Point
    . both (read . dropWhile (not . Char.isNumber))
    . List.break (==',')
