{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies          #-}
--------------------------------------------------------------------
-- |
-- Copyright : (c) Jann Müller 2018
-- License   :  MIT
-- Maintainer:  Jann Müller <j.mueller.11@alumni.ucl.ac.uk>
-- Stability :  experimental
-- Portability: non-portable
--
-- Scales that assign colours to values.
--------------------------------------------------------------------
module Data.H3.Colour(
  -- * Assign values to colours
  ordinalColours,
  OrdinalColours,
  -- * Utilities and re-exports
  Colour,
  PaletteType(..),
  paletteFor,
  toCSSColour
  ) where

import           Data.Colour.Names             (black)
import           Data.Colour.Palette.BrewerSet (ColorCat (..), brewerSet)
import           Data.Colour.RGBSpace          (Colour)
import           Data.Colour.SRGB              (sRGB24show)
import           Data.Functor.Identity         (Identity (..))
import           Data.H3.Scalable
import qualified Data.Map                      as Map
import           Data.Proxy                    (Proxy (..))
import           GHC.Generics                  (Generic)

data OrdinalColours a

type instance Target OrdinalColours = Identity
type instance TargetRange OrdinalColours = Proxy

-- | Create a scale that maps discrete values to colours. Colours will repeat
--   if there are more than 12 different values.
ordinalColours :: [a] -> ScaleOptions OrdinalColours a (Colour Double)
ordinalColours = flip OrdColourScaleOptions black

instance (Ord a, Eq a) => Scalable OrdinalColours a (Colour Double) where
  data ScaleOptions OrdinalColours a b =
    OrdColourScaleOptions
      [a]
      (Colour Double)
  scale (OrdColourScaleOptions ex k) _ = scMap where
    n  = length ex
    pl = paletteFor Qualitative n
    theMap = Map.fromList
      $ zip ex
      $ cycle pl
    scMap av = Identity $ Map.findWithDefault k av theMap


data PaletteType =
  Sequential
  | Diverging
  | Qualitative
  deriving (Eq, Ord, Show, Generic)

-- | Get a palette of the specified size for a palette type.
-- The actual size of the palette may be smaller or larger than the argument.
paletteFor :: PaletteType -> Int -> [Colour Double]
paletteFor pt i = brewerSet cat i' where
  (cat, i') = case pt of
    Sequential  -> (Greens, clamp (3, 9) i)
    Diverging   -> (PiYG, clamp (3, 11) i)
    Qualitative -> (Paired, clamp (3, 12) i)
  clamp (mn, mx) n = max mn (min mx n)

-- | Convert a colour to hexadecimal form so that it can be used in style
-- attributes. Example: "#00aaff"
toCSSColour :: Colour Double -> String
toCSSColour = sRGB24show
