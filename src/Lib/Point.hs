{-# OPTIONS_GHC -Wno-orphans -Wno-type-defaults #-}
module Lib.Point
( Point, pattern Point, px, py
, magSq
, divModP, divP, modP
, scalingOf, projects, similar
, within, (.<=)
, Linear.perp
) where

import Linear qualified
import GHC.Records (HasField (getField))
import Data.Maybe (isJust)

default (Int)

type Point = Linear.V2 Int

{-# COMPLETE Point #-}
pattern Point :: Int -> Int -> Linear.V2 Int
pattern Point{px, py} = Linear.V2 px py
-- NOTE: orphan instance over V2 Int
instance HasField "px" Point Int where
  getField :: Point -> Int
  getField = px
instance HasField "py" Point Int where
  getField :: Point -> Int
  getField = py

magSq :: Point -> Int
magSq p = p.px^2 + p.py^2

divModP :: Point -> Point -> (Point, Point)
divModP a b = let
  (divX, modX) = a.px `divMod` b.px
  (divY, modY) = a.py `divMod` b.py
  in (Point divX divY, Point modX modY)
divP :: Point -> Point -> Point
divP = (fst .) . divModP
modP :: Point -> Point -> Point
modP = (snd .) . divModP

scalingOf :: Point -> Point -> Maybe Int
scalingOf a b = case a `divModP` b of
  (r, 0) -> Just r.px
  _      -> Nothing

projects :: Point -> Point -> Bool
projects = (isJust .) . scalingOf

similar :: Point -> Point -> Bool
similar a b = projects a b || projects a b

within :: Point -> Point -> Bool
within point bounds = point .<= bounds && Point 0 0 .<= point

(.<=) :: Point -> Point -> Bool
a .<= b = a.px <= b.px && a.py <= b.py
