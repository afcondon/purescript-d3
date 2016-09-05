module Graphics.D3.SVG.Axis
  ( Axis()
  , axis
  , scale
  , orient
  , ticks
  , tickFormat
  , renderAxis
  ) where

import Control.Monad.Eff (Eff)
import Data.Function.Eff (EffFn2, runEffFn2)

import Graphics.D3.Base (D3, D3Eff)
import Graphics.D3.Selection (class Existing)
import Graphics.D3.Scale (class Scale)

foreign import data Axis :: *
foreign import axis :: D3Eff Axis

foreign import scaleImpl :: forall eff s d. (Scale s) =>
  EffFn2 (d3::D3|eff) (s d Number) Axis Axis
foreign import orientImpl :: forall eff.
  EffFn2 (d3::D3|eff) String Axis Axis
foreign import ticksImpl :: forall eff.
  EffFn2 (d3::D3|eff) Number Axis Axis
foreign import tickFormatImpl :: forall eff.
  EffFn2 (d3::D3|eff) String Axis Axis
foreign import renderAxisImpl :: forall eff s d. (Existing s) =>
  EffFn2 (d3::D3|eff) Axis (s d) (s d)

-- scale = ffi ["scale", "axis", ""] "axis.scale(scale)"
scale :: forall eff s d. (Scale s) => (s d Number) -> Axis -> Eff (d3::D3|eff) Axis
scale = runEffFn2 scaleImpl
-- orient = ffi ["orientation", "axis", ""] "axis.orient(orientation)"
orient :: forall eff. String -> Axis -> Eff (d3::D3|eff) Axis
orient = runEffFn2 orientImpl
-- ticks = ffi ["count", "axis", ""] "axis.ticks(count)"
ticks :: forall eff. Number -> Axis -> Eff (d3::D3|eff) Axis
ticks = runEffFn2 ticksImpl
-- tickFormat = ffi ["format", "axis", ""] "axis.tickFormat(d3.format(format))"
tickFormat :: forall eff. String -> Axis -> Eff (d3::D3|eff) Axis
tickFormat = runEffFn2 tickFormatImpl
-- renderAxis = ffi ["axis", "selection", ""] "selection.call(axis)"
renderAxis :: forall eff s d. (Existing s) => Axis -> s d -> Eff (d3::D3|eff) (s d)
renderAxis = runEffFn2 renderAxisImpl
