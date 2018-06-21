{-# LANGUAGE DeriveFunctor         #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies          #-}
module Data.H3.Scalable(
  Target,
  TargetRange,
  Scalable(..),
  VisualElements(..),
  ChartVisuals(..)
) where

import           Data.H3.Shape  (Shape)
import           Data.Semigroup (Semigroup (..))

type family Target (a :: * -> *) :: * -> * -- can be Identity or Extent
type family TargetRange (a :: * -> *) :: * -> * -- Identity, Extent, ..

class Scalable f a b where
  data ScaleOptions f :: * -> * -> *
  scale :: (ScaleOptions f) a b -> (TargetRange f) b -> a -> (Target f) b

data VisualElements s n = VisualElements {
  veTicks      :: [n],
  veGridLines  :: [n],
  veAxisLabels :: [(String, n)],
  veLegend     :: [Shape s n]
} deriving (Functor)

instance Semigroup (VisualElements s n) where
  (VisualElements lt lg la ll) <> (VisualElements rt rg ra rl) =
    VisualElements (lt <> rt) (lg <> rg) (la <> ra) (ll <> rl)

instance Monoid (VisualElements s n) where
  mappend = (<>)
  mempty = VisualElements [] [] [] []

class Scalable f a b => ChartVisuals f a b where
  visuals :: (ScaleOptions f) a b -> (TargetRange f) b ->  VisualElements String b
