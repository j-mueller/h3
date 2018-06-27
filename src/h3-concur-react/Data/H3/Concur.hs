{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE LambdaCase            #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
--------------------------------------------------------------------
-- |
-- Copyright : (c) Jann Müller 2018
-- License   :  MIT
-- Maintainer:  Jann Müller <j.mueller.11@alumni.ucl.ac.uk>
-- Stability :  experimental
-- Portability: non-portable
--
-- Render 'Shape's to react-svg using @concur@ and @concur-react@ packages.
--------------------------------------------------------------------
module Data.H3.Concur(
  renderShape,
  renderSvg
  ) where

import           Concur.Core
import           Concur.React
import           Control.Applicative (Alternative (..))
import           Data.Bifunctor      (Bifunctor (..))
import           Data.H3.Extent      (toTuple)
import           Data.H3.Visuals     (FontSize (..), FontWeight (..),
                                      Pixel (..), Shape (..), TextAnchor (..))
import           Data.List           (intersperse)
import           Data.String         (IsString (..))
import           GHCJS.Types         (JSString)

-- | Render a shape to react svg elements. The result has to be enclosed in an
--   @\<svg/\>@ node (this is what 'renderSvg' does)
renderShape ::
  Shape String (Pixel String, Pixel String)
  -> Widget HTML a
renderShape = go "black" "1.0" . fmap (bimap getPixel getPixel) where
  go color opac = \case
    EmptyShape -> empty
    ALine pts -> el "polyline" [
      vstyleStr "stroke" (fromString color),
      vstyleStr "fill" "none",
      vstyleStr "opacity" (fromString opac),
      vstyleStr "strokeWidth" "1",
      vattr "points" $ makePoints pts] []
    ARectangle ex ->
      let ((lwx, lwy), (hgx, hgy)) = toTuple ex
          thePoints = [(lwx, lwy), (hgx, lwy), (hgx, hgy), (lwx, hgy)]
      in
      el "polygon" [
        vstyleStr "fill" (fromString color),
        vstyleStr "opacity" (fromString opac),
        vattr "points" $ makePoints thePoints] []
    AnArea pts -> el "polygon" [
      vstyleStr "fill" (fromString color),
      vstyleStr "opacity" (fromString opac),
      vattr "points" $ makePoints pts] []
    AColouredShape cl rst -> go cl opac rst
    AGroup gn -> el "g" [] (go color opac <$> gn)
    ALabel ta (FontSize fs) (FontWeight fw) lbl (x, y) ->
      let anchor = case ta of
            AnchorStart  -> "start"
            AnchorMiddle -> "middle"
            AnchorEnd    -> "end" in
      svgText [
        vstyleStr "fontSize" (fromString fs),
        vstyleStr "fontWeight" (fromString fw),
        vstyleStr "textAnchor" anchor,
        vattr "x" $ fromString x,
        vattr "y" $ fromString y] (fromString lbl)
    AnOpacity opa sh -> go color (show opa) sh

-- | Render a shape to a react SVG node using the provided dimensions
renderSvg ::
  ((Pixel String, Pixel String)
  -> Shape String (Pixel String, Pixel String))
  -> (Pixel String, Pixel String)
  -> Widget HTML a
renderSvg f dims = el "svg" [] [content] where
  content = renderShape $ f dims
  -- TODO: Viewport / width + height

-- | Create a @<text>@ node with some text.
svgText ::
     [VAttr]
  -> JSString
  -> Widget HTML a
svgText attrs t = el "text" attrs [display [vtext t]]

makePoints :: IsString s => [(String, String)] -> s
makePoints =
  fromString
  . foldl (<>) ""
  . intersperse " "
  . fmap makePoint where
    makePoint (x, y) = x <> "," <> y
