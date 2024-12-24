{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}
module Days.D14 where

import Lib ( Dispatch, dispatchWith, both )
import Lib.Field ( north, west )
import Lib.Point ( Point, pattern Point, divP, modP, within )
import Control.Arrow ( (&&&) )
import Data.Bifunctor ( bimap, second )
import Data.List qualified as List

dispatch :: Dispatch
dispatch = dispatchWith part1 part2

part1 :: String -> Int
part1 = uncurry (run 100) . (boundsFrom &&& id) . parseInput

part2 :: String -> Int
part2 = undefined   -- fuck off with that shit! :D

data Robot = Robot
  { loc :: Point
  , vel :: Point
  } deriving Show

boundsFrom :: [Robot] -> Point
boundsFrom input = case length input of
  12 -> Point 11 7
  _  -> Point 101 103

run :: Int -> Point -> [Robot] -> Int
run 0     = safety
run count = \bounds -> run (pred count) bounds . fmap (advance bounds)

advance :: Point -> Robot -> Robot
advance wrap robot@Robot{..} = robot{ loc = (loc + vel) `modP` wrap }

safety :: Point -> [Robot] -> Int
safety bounds bots = let
  halfBounds = bounds `divP` 2
  qBounds = halfBounds - 1
  qShift = halfBounds + 1
  shiftX = qShift * west
  shiftY = qShift * north
  q shift = filter (\r -> (r.loc + shift) `within` qBounds)
  quadrants = ($ bots) <$> [q 0, q shiftX, q shiftY, q (shiftX+shiftY)]
  in product $ length <$> quadrants

parseInput :: String -> [Robot]
parseInput = fmap parseRobot . lines
  where
  parseRobot = uncurry Robot . bimap parseLoc parseVel . break (==' ')
  parsePoint = uncurry Point . both ( read . ('(':) . (<>")") )
    . second (List.drop 1) . List.break (==',')
  parseLoc = parsePoint . drop 2
  parseVel = parsePoint . drop 3
