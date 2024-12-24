module Days.D08 where

import Lib ( Dispatch, dispatchWith )
import Lib.Point ( Point, within )
import Lib.Field qualified as Field
import Data.Vector qualified as Vec
import Data.List qualified as List
import Control.Monad ( guard )

dispatch :: Dispatch
dispatch = dispatchWith part1 part2

part1 :: String -> Int
part1 = length . List.nub . concatMap antinodes . parseInput

part2 :: String -> Int
part2 = length . List.nub . concatMap antinodes2 . parseInput

data AntennaField = AntennaField
  { frequency :: Char
  , bounds    :: Point
  , antennae  :: [Point]
  } deriving (Show)

antinodes :: AntennaField -> [Point]
antinodes AntennaField{..} = do
  [ant1,ant2] <- filter ((==2) . length) $ List.subsequences antennae
  let offset = ant1 - ant2
  antinode <- [ant1 + offset, ant2 - offset]
  guard $ antinode `within` bounds
  pure antinode

antinodes2 :: AntennaField -> [Point]
antinodes2 AntennaField{..} = do
  [ant,ant'] <- filter ((==2) . length) $ List.subsequences antennae
  let
    offset = ant - ant'
    periodic s = takeWhile (`within` bounds) $ List.iterate (s * offset +) ant
  ant : periodic 1 <> periodic (-1)

parseInput :: String -> [AntennaField]
parseInput raw = package <$> frequencies
  where
  everything = Field.parse id raw
  bounds = Field.extent everything
  frequencies = List.nub $ List.filter (not . flip elem ".\n") raw
  package frequency
    = AntennaField frequency bounds . Vec.toList
    $ Field.elemIndices frequency everything
