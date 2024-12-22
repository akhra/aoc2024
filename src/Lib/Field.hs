{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}
module Lib.Field
( module Lib.Field
, Point, pattern Point, px, py
) where

import Lib ( both, vec )
import Lib.Point ( Point, pattern Point, px, py, bounded )
import Control.Arrow ( (&&&) )
import Data.Map qualified as Map
import Data.Maybe ( listToMaybe )
import Data.Vector ( Vector, (!), (!?), (//) )
import Data.Vector qualified as Vec

type Field a = Vector (Vector a)

lookup :: Point -> Field a -> Maybe a
lookup Point{..} field = (!? px) =<< field !? py

ilookup :: Point -> Field a -> Maybe (Point, a)
ilookup point = fmap (point,) . Lib.Field.lookup point

update :: Point -> a -> Field a -> Field a
update Point{..} x v = v // [(py, v ! py // [(px, x)])]

updates :: [(Point, a)] -> Field a -> Field a
updates xs field = Vec.imap updateRow field
  where
  updateRow ix row = maybe row (row //) $ Map.lookup ix rowUpdates
  rowUpdates = Map.fromListWith (<>) $ extractRow <$> xs
  extractRow (Point{..}, x) = (py, [(px, x)])

map :: (a -> b) -> Field a -> Field b
map = Vec.map . Vec.map

imap :: (Point -> a -> b) -> Field a -> Field b
imap f field = Vec.imap byCol field
  where
  byCol y row = Vec.imap (byRow y) row
  byRow y x a = f (Point x y) a

findIndices :: Eq a => (a -> Bool) -> Field a -> Vector Point
findIndices p field = do
  (colIx, row) <- Vec.imap (,) field
  rowIx <- Vec.findIndices p row
  pure $ Point rowIx colIx

findIndex :: Eq a => (a -> Bool) -> Field a -> Maybe Point
findIndex p = listToMaybe . Vec.toList . findIndices p

elemIndices :: Eq a => a -> Field a -> Vector Point
elemIndices x = findIndices (==x)

elemIndex :: Eq a => a -> Field a -> Maybe Point
elemIndex x = findIndex (==x)

within :: Point -> Field a -> Bool
within point field = bounded point (extent field)

extent :: Field a -> Point
extent = uncurry Point . both pred . (minimum . fmap Vec.length &&& Vec.length)

parse :: (Char -> a) -> String -> Field a
parse parseChar = vec . fmap (vec . fmap parseChar) . lines

indices :: Field a -> [Point]
indices field = let Point{..} = extent field in
  [Point x y | x <- [0..px], y <- [0..py]]

fill :: Point -> a -> Field a
fill Point{..} = Vec.replicate (py+1) . Vec.replicate (px+1)

cardinals, ordinals :: [Point]
northwest, north, northeast, east, southeast, south, southwest, west :: Point
cardinals@[north, east, south, west] =
  [ Point 0 (-1)
  , Point 1 0
  , Point 0 1
  , Point (-1) 0
  ]
ordinals@[northwest, northeast, southeast, southwest] =
  [ north + west
  , north + east
  , south + east
  , south + west
  ]
