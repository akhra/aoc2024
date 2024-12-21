{-# OPTIONS_GHC -Wno-x-partial #-}
{-# LANGUAGE OverloadedStrings #-}
module Days.D04 where

import Lib ( Dispatch, dispatchWith )
import Lib.Field ( Field, Point )
import Lib.Field qualified as Field
import Control.Monad ( void, guard )
import Data.Text qualified as Text
import Data.Text ( Text )
import Data.Map qualified as Map
import Data.Vector qualified as Vec

dispatch :: Dispatch
dispatch = dispatchWith part1 part2
--

part1 :: String -> Int
part1 input = length $ do
  word <- searchWords
  line <- parse1 input
  void $ Text.breakOnAll word line
  where
  searchWords = ["XMAS"]

parse1 :: String -> [Text]
parse1 input = Text.pack <$> forward <> backward
  where
  backward = reverse <$> forward
  forward = rows <> cols <> diag1 <> diag2 <> diag3 <> diag4
  rows = lines input
  cols = columns rows
  diag1 = columns . drops $ rows
  diag2 = columns . drops $ reverse rows
  diag3 = drop 1 . columns . drops . fmap reverse $ rows
  diag4 = drop 1 . columns . drops . fmap reverse $ reverse rows

columns :: [String] -> [String]
columns rows = fmap snd . Map.toList $ Map.unionsWith (<>) ixMaps
  where
  ixMaps = fmap (Map.fromList) ixRows
  ixRows = zip [(0::Int)..] . fmap pure <$> rows

drops :: [String] -> [String]
drops = snd . foldr shrinkinator (0, [])
  where
  shrinkinator xx (n, aa) = (succ n, drop n xx : aa)

--
part2 :: String -> Int
part2 input = let field = parse2 input in
  length . Vec.catMaybes $ findXmas field <$> Field.elemIndices 'A' field

findXmas :: Field Char -> Point -> Maybe ()
findXmas field point = do
  diags <- traverse (flip Field.lookup field) diagCoords
  guard $ rightCount 'M' diags && rightCount 'S' diags
  guard $ length (Vec.group diags) < 4
  where
  diagCoords = Vec.fromList $ (point +) <$> Field.ordinals
  rightCount c = (==2) . length . Vec.filter (==c)

parse2 :: String -> Field Char
parse2 = Field.parse id
