{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}
--------------------------------------------------------------------
-- |
-- Copyright : (c) Jann Müller 2018
-- License   :  MIT
-- Maintainer:  Jann Müller <j.mueller.11@alumni.ucl.ac.uk>
-- Stability :  experimental
-- Portability: non-portable
--
-- Render 'Shape's to SVG using the @svg-builder@ package.
--------------------------------------------------------------------
module Data.H3.Svg(
  renderShape,
  renderSvg
  ) where

import           Data.Bifunctor    (Bifunctor (..))
import           Data.H3.Extent    (Extent, toTuple)
import           Data.H3.Visuals
import           Data.String       (IsString (..))
import           Data.Text         (Text)
import qualified Data.Text         as Text
import           Graphics.Svg
import qualified Graphics.Svg.Path as Path

-- | Render a 'Shape' as an SVG 'Element'
renderShape :: Shape Text (Pixel Double, Pixel Double) -> Element
renderShape = go "black" "1.0" . fmap (bimap (Path.toText . getPixel) (Path.toText . getPixel)) where
  go color opac = \case
    EmptyShape -> mempty
    ALine pts -> polyline_ [
      Stroke_ <<- color,
      Fill_ <<- "none",
      Opacity_ <<- opac,
      Stroke_width_ <<- "1",
      Points_ <<- mkPath pts]
    ARectangle ex ->
      let ((lwx, lwy), (hgx, hgy)) = toTuple ex
          thePoints = [(lwx, lwy), (hgx, lwy), (hgx, hgy), (lwx, hgy)] in
      polygon_ [
        Fill_ <<- color,
        Opacity_ <<- opac,
        Points_ <<- mkPath thePoints
        ]
    AnArea pts -> polygon_ [
        Fill_ <<- color,
        Opacity_ <<- opac,
        Points_ <<- mkPath pts
      ]
    AColouredShape c rest -> go c opac rest
    AGroup g -> g_ [] (foldMap (go color opac) g)
    ALabel ta (FontSize fs) (FontWeight fw) lbl (x, y) ->
      let anchor = case ta of
            AnchorStart  -> "start"
            AnchorMiddle -> "middle"
            AnchorEnd    -> "end" in
      text_ [
        Font_size_ <<- Text.pack fs,
        Font_weight_ <<- Text.pack fw,
        Text_anchor_ <<- anchor,
        X_ <<- x,
        Y_ <<- y
      ] (fromString lbl :: Element)
    AnOpacity opa sh -> go color (Text.pack $ show opa) sh

-- | Render a shape to an SVG document using the provided dimensions
renderSvg :: ((Extent Double, Extent Double) -> Shape Text (Pixel Double, Pixel Double)) -> (Extent Double, Extent Double) -> Element
renderSvg f dims = doctype <> with (svg11_ content) [Version_ <<- "1.1"] where
  content = renderShape $ f dims
  -- TODO: Viewport / width + height

mkPath :: [(Text, Text)] -> Text
mkPath =
    Text.unwords
  . fmap makePoint where
    makePoint (x, y) = x <> "," <> y
