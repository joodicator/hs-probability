{-# LANGUAGE RebindableSyntax, NoImplicitPrelude #-}

module Dice(die, dice) where

import Data.Ratio
import ProbDist

{-# SPECIALISE die :: Int -> Dist Rational' Int #-}
{-# SPECIALISE die :: Int -> Dist Float' Int #-}
die :: Prob p => Int -> Dist p Int
die d = uniform [1..d]

{-# SPECIALISE dice :: Int -> Int -> Dist Rational' Int #-}
{-# SPECIALISE dice :: Int -> Int -> Dist Float' Int #-}
dice :: Prob p => Int -> Int -> Dist p Int
dice 0 _ = do
    return 0
dice k d | k > 0 = do
    n <- dice (k - 1) d
    m <- roll
    return (m + n)
  where
    roll = die d
