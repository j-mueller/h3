{-# LANGUAGE DeriveFoldable             #-}
{-# LANGUAGE DeriveFunctor              #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE DeriveTraversable          #-}
{-# LANGUAGE ExplicitForAll             #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase                 #-}
--------------------------------------------------------------------
-- |
-- Copyright : (c) Jann Müller 2018
-- License   :  MIT
-- Maintainer:  Jann Müller <j.mueller.11@alumni.ucl.ac.uk>
-- Stability :  experimental
-- Portability: non-portable
--
-- Shapes that make up charts.
--------------------------------------------------------------------
module Data.H3.Shape(
  Shape(..),
  mapColour,
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

-- | Indicates where a text element should be anchored, that is where its
--   origin is placed relative to the text.
data TextAnchor = AnchorStart | AnchorMiddle | AnchorEnd
  deriving (Eq, Ord, Show, Generic, Enum, Bounded)

-- | Font size
newtype FontSize s = FontSize { getFontSize :: s }
  deriving (Functor, Eq, Ord, Show, Generic)

-- | Font size iso
_FontSize :: forall p f a b. (Profunctor p, Functor f) => p (FontSize a) (f (FontSize b)) -> p a (f b)
_FontSize = dimap FontSize (fmap getFontSize)

-- | Font weight
newtype FontWeight s = FontWeight { getFontWeight :: s }
  deriving (Functor, Eq, Ord, Show, Generic)

-- | Font weight iso
_FontWeight :: forall p f a b. (Profunctor p, Functor f) => p (FontWeight a) (f (FontWeight b)) -> p a (f b)
_FontWeight = dimap FontWeight (fmap getFontWeight)

-- | Pixel unit in the target coordinate system
newtype Pixel a = Pixel { getPixel :: a }
    deriving (Functor, Eq, Ord, Show, Num, Floating, Real, Fractional, RealFrac)

-- | Pixel iso
_Pixel :: forall p f a b. (Profunctor p, Functor f) => p (Pixel a) (f (Pixel b)) -> p a (f b)
_Pixel = dimap Pixel (fmap getPixel)

-- | Shapes that make up charts. @s@ is the type of colours, and @n@ the type
--  of coordinates. For a 2D chart @n@ should be something like @(Double,
--  Double)@.
data Shape s n =
  ALine [n] -- ^ An open line of several points
  | ARectangle !(Extent n) -- ^ A rectangle, defined by its lower left and upper right corners (if @n ~ (Double, Double)@)
  | AnArea [n] -- ^ An area (closed line)
  | AColouredShape s !(Shape s n) -- ^ Set the colour of a shape
  | AGroup [Shape s n] -- ^ Group several shapes together
  | ALabel !TextAnchor !(FontSize String) !(FontWeight String) !String n -- ^ A label
  | AnOpacity Double !(Shape s n) -- ^ Set the opacity of a shape
  | EmptyShape -- ^ Empty shape
  deriving (Functor, Foldable, Traversable)

-- | Change the colours of a 'Shape'.
mapColour :: (s -> t) -> Shape s n -> Shape t n
mapColour f = \case
  ALine ns -> ALine ns
  ARectangle e -> ARectangle e
  AnArea ns -> AnArea ns
  AColouredShape c sh -> AColouredShape (f c) (mapColour f sh)
  AGroup sh -> AGroup (fmap (mapColour f) sh)
  ALabel ta fs fw s n -> ALabel ta fs fw s n
  AnOpacity d sh -> AnOpacity d (mapColour f sh)
  EmptyShape -> EmptyShape
