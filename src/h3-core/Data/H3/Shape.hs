{-# LANGUAGE DeriveFoldable             #-}
{-# LANGUAGE DeriveFunctor              #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE DeriveTraversable          #-}
{-# LANGUAGE ExplicitForAll             #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase                 #-}
module Data.H3.Shape(
  Shape(..),
  mapColor,
  TextAnchor(..),
  FontSize(..),
  _FontSize,
  FontWeight(..),
  _FontWeight,
  Pixel(..),
  _Pixel
) where

import           Data.Profunctor (Profunctor (..))
import           GHC.Generics    (Generic)

import           Data.H3.Extent  (Extent)

data TextAnchor = AnchorStart | AnchorMiddle | AnchorEnd
  deriving (Eq, Ord, Show, Generic, Enum, Bounded)

newtype FontSize s = FontSize { getFontSize :: s }
  deriving (Functor, Eq, Ord, Show, Generic)

_FontSize :: forall p f a b. (Profunctor p, Functor f) => p (FontSize a) (f (FontSize b)) -> p a (f b)
_FontSize = dimap FontSize (fmap getFontSize)

newtype FontWeight s = FontWeight { getFontWeight :: s }
  deriving (Functor, Eq, Ord, Show, Generic)

_FontWeight :: forall p f a b. (Profunctor p, Functor f) => p (FontWeight a) (f (FontWeight b)) -> p a (f b)
_FontWeight = dimap FontWeight (fmap getFontWeight)

-- | Pixel unit in the target coordinate system
newtype Pixel a = Pixel { getPixel :: a }
    deriving (Functor, Eq, Ord, Show, Num, Floating, Real, Fractional, RealFrac)

_Pixel :: forall p f a b. (Profunctor p, Functor f) => p (Pixel a) (f (Pixel b)) -> p a (f b)
_Pixel = dimap Pixel (fmap getPixel)

data Shape s n =
  ALine [n]
  | ARectangle !(Extent n)
  | AnArea [n]
  | AColouredShape s !(Shape s n)
  | AGroup [Shape s n]
  | ALabel !TextAnchor !(FontSize String) !(FontWeight String) !String n
  | AnOpacity Double !(Shape s n)
  | EmptyShape
  deriving (Functor, Foldable, Traversable)

mapColor :: (s -> t) -> Shape s n -> Shape t n
mapColor f = \case
  ALine ns -> ALine ns
  ARectangle e -> ARectangle e
  AnArea ns -> AnArea ns
  AColouredShape c sh -> AColouredShape (f c) (mapColor f sh)
  AGroup sh -> AGroup (fmap (mapColor f) sh)
  ALabel ta fs fw s n -> ALabel ta fs fw s n
  AnOpacity d sh -> AnOpacity d (mapColor f sh)
  EmptyShape -> EmptyShape
