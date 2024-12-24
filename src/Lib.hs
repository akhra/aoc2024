{-# OPTIONS_GHC -Wno-type-defaults #-}
{-# LANGUAGE PatternSynonyms #-}
module Lib where

import Data.Bifunctor ( Bifunctor(bimap) )
import Data.Vector ( Vector )
import Data.Vector qualified as Vec

default (Int)

type Dispatch = String -> String -> String

dispatchWith :: (Show a) => (String -> a) -> (String -> a) -> Dispatch
dispatchWith process1 process2 part = show . process
  where
  process = case part of
    "a" -> process1
    "b" -> process2
    unk -> error $ "unknown part " <> show unk

both :: Bifunctor f => (a -> b) -> f a a -> f b b
both f = bimap f f

always, never :: a -> Bool
always = const True
never = const False

vec :: [a] -> Vector a
vec = Vec.force . Vec.fromList

unvec :: Vector a -> [a]
unvec = Vec.toList

countDigits :: Int -> Int
countDigits = (1 +) . floor . logBase @Double 10 . fromIntegral
