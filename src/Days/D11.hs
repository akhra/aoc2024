{-# OPTIONS_GHC -Wno-x-partial #-}
module Days.D11 where

import Lib ( Dispatch, dispatchWith, countDigits )
import Data.Bool ( bool )

dispatch :: Dispatch
dispatch = dispatchWith part1 part2

part1 :: String -> Int
part1 = sum . iterateRules 25 . fmap read . words

part2 :: String -> Int
part2 = sum . fmap (deepAndTight 75) . fmap read . words

iterateRules :: Int -> [Int] -> [Int]
iterateRules n = head . drop n . iterate (concatMap rules)

rules :: Int -> [Int]
rules 0 = [1]
rules x = bool boost split $ digits `mod` 2 == 0
  where
  digits = countDigits x
  boost = [x * 2024]
  split = let
    shift = 10 ^ (digits `div` 2)
    upper = x `div` shift
    lower = x - upper * shift
    in [upper, lower]

deepAndTight :: Int -> Int -> Int
deepAndTight 0 = const 1
deepAndTight i = let deeper = deepAndTight (pred i) in \case
  0 -> deeper 1
  x -> bool boost split $ digits `mod` 2 == 0
    where
    digits = countDigits x
    boost = deeper $ x * 2024
    split = (\(l,r) -> deeper l + deeper r) $ case x of
      10 -> (1,0)
      11 -> (1,1)
      12 -> (1,2)
      13 -> (1,3)
      14 -> (1,4)
      15 -> (1,5)
      16 -> (1,6)
      17 -> (1,7)
      18 -> (1,8)
      19 -> (1,9)
      20 -> (2,0)
      21 -> (2,1)
      22 -> (2,2)
      23 -> (2,3)
      24 -> (2,4)
      25 -> (2,5)
      26 -> (2,6)
      27 -> (2,7)
      28 -> (2,8)
      29 -> (2,9)
      30 -> (3,0)
      31 -> (3,1)
      32 -> (3,2)
      33 -> (3,3)
      34 -> (3,4)
      35 -> (3,5)
      36 -> (3,6)
      37 -> (3,7)
      38 -> (3,8)
      39 -> (3,9)
      40 -> (4,0)
      41 -> (4,1)
      42 -> (4,2)
      43 -> (4,3)
      44 -> (4,4)
      45 -> (4,5)
      46 -> (4,6)
      47 -> (4,7)
      48 -> (4,8)
      49 -> (4,9)
      50 -> (5,0)
      51 -> (5,1)
      52 -> (5,2)
      53 -> (5,3)
      54 -> (5,4)
      55 -> (5,5)
      56 -> (5,6)
      57 -> (5,7)
      58 -> (5,8)
      59 -> (5,9)
      60 -> (6,0)
      61 -> (6,1)
      62 -> (6,2)
      63 -> (6,3)
      64 -> (6,4)
      65 -> (6,5)
      66 -> (6,6)
      67 -> (6,7)
      68 -> (6,8)
      69 -> (6,9)
      70 -> (7,0)
      71 -> (7,1)
      72 -> (7,2)
      73 -> (7,3)
      74 -> (7,4)
      75 -> (7,5)
      76 -> (7,6)
      77 -> (7,7)
      78 -> (7,8)
      79 -> (7,9)
      80 -> (8,0)
      81 -> (8,1)
      82 -> (8,2)
      83 -> (8,3)
      84 -> (8,4)
      85 -> (8,5)
      86 -> (8,6)
      87 -> (8,7)
      88 -> (8,8)
      89 -> (8,9)
      90 -> (9,0)
      91 -> (9,1)
      92 -> (9,2)
      93 -> (9,3)
      94 -> (9,4)
      95 -> (9,5)
      96 -> (9,6)
      97 -> (9,7)
      98 -> (9,8)
      99 -> (9,9)
      2024 -> (20,24)
      4048 -> (40,48)
      6072 -> (60,72)
      8096 -> (80,96)
      _  -> let
        shift = 10 ^ (digits `div` 2)
        upper = x `div` shift
        lower = x - upper * shift
        in (upper, lower)
