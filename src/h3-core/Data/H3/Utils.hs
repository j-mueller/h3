module Data.H3.Utils(
  looseLabels,
  LabelCount(..),
  defaultLabelCount,
  RoundingMode(..),
  nicenum,
  linear,
  computeMidpoint,
  viewBox
  ) where

import           Data.List.NonEmpty      (NonEmpty (..))
import           Data.Semigroup          (Max (..), Min (..), Sum (..))
import           Data.Semigroup.Foldable
import           Data.String             (IsString (..))

import           Data.H3.Extent          (Extent (..))

-- | How many labels to generate
newtype LabelCount = LabelCount Integer

defaultLabelCount :: LabelCount
defaultLabelCount = LabelCount 5

-- | Generate a list of "nice" numbers that contain the given interval.
--
-- Based on "Nice numbers for label graphs" by Paul Heckbert.
-- http://www.realtimerendering.com/resources/GraphicsGems/gems/Label.c
looseLabels :: (Ord a, Num a, Fractional a, Floating a, RealFrac a) => LabelCount -> (a, a) -> (NonEmpty a, Integer)
looseLabels (LabelCount nticks) (mn, mx) = (result, nfrac) where
  range = nicenum DontRound (mx - mn)
  d = nicenum Round (range / fromIntegral (nticks - 1))
  floor' f = floor f :: Integer
  ceiling' f = ceiling f :: Integer
  graphmin = fromIntegral (floor' (mn / d)) * d
  graphmax = fromIntegral (ceiling' (mx / d)) * d
  nfrac = max 0 (negate $ floor' $ logBase 10 d) -- nfrac is the number of fractional digits to show
  first:rest = go graphmin where
    go c = c : go (c + d)
  result = first :| takeWhile (\i -> i < graphmax + 0.5*d) rest

data RoundingMode = Round | DontRound

nicenum :: (Ord a, Num a, Fractional a, Floating a, RealFrac a) => RoundingMode -> a -> a
nicenum rm x = result where
  result = nf * (10 ^^ expv)
  floor' f' = floor f' :: Integer
  expv = floor' (logBase 10 x)
  f = x / (10 ^^ expv)
  nf = case rm of
    Round
      | f < 1.5 -> 1
      | f < 3   -> 2
      | f < 7   -> 5
      | otherwise -> 10
    DontRound
      | f <= 1  -> 1
      | f <= 2  -> 2
      | f <= 5  -> 5
      | otherwise -> 10

linear :: (Fractional a, Eq a) => (a, a) -> (a, a) -> a -> a
linear (srcMin, srcMax) (tgtMin, tgtMax) = f where
  f x = tgtMin + (slope * (x - srcMin))
  dlt = srcMax - srcMin
  slope = if dlt /= 0 then (tgtMax - tgtMin) / dlt else tgtMax - tgtMin

computeMidpoint :: (Fractional x, Num x, Foldable1 f) => f x -> x
computeMidpoint f = total / count where
  (Sum total, Sum count) = foldMap1 cnt f
  cnt x = (Sum x, Sum 1)

-- | Generate a value for an SVG "viewBox" attribute based on the extents of the
--   x- and y-axis.
viewBox :: (IsString s)
  => Extent Double -- extent of the x axis
  -> Extent Double -- extent of the y axis
  -> s
viewBox mx my = fromString $
  show minX <> " " <> show minY <> " " <>
  show range <>
  " " <>
  show height
  where
    (Extent (Min by, Max ty)) = my
    (Extent (Min bx, Max tx)) = mx
    height = abs $ ty - by
    range = abs $ tx - bx
    minX = min bx tx
    minY = min by ty
