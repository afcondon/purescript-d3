module Graphics.D3.Behavior.Zoom where

import Prelude (Unit, show, (<>))
import Data.Array ((!!))
import Data.Maybe (fromMaybe)
import Control.Monad.Eff (Eff)
import Data.Function.Eff
import Graphics.D3.Base (D3)

foreign import data Zoomable :: *
type Translate = Array Number -- actually only two element array in D3
type Scale     = Number
type ZoomEvent = { translate :: Translate, scale :: Scale }

foreign import createZoomableImpl  :: forall eff. Eff (d3::D3|eff) Zoomable

createZoom :: forall eff. Eff (d3::D3|eff) Zoomable
createZoom = createZoomableImpl

-- simple utilities to aid in constructing a transform from a ZoomEvent
showTranslate :: Translate -> String
showTranslate t = "translate(" <> show x <> "," <> show y <> ")" where
  x = fromMaybe 0.0 (t !! 0) -- in fact the array is guaranteed to have two elements by D3 spec
  y = fromMaybe 0.0 (t !! 1) -- thus, fromMaybe default values will never be used

showScale :: Scale -> String
showScale s = "scale(" <> show s <> ")"

-- defition of the handler setter using Data.Functions.Eff utilities
onZoom :: forall eff. (ZoomEvent -> Eff (d3::D3|eff) Unit) -> Zoomable -> Eff (d3::D3|eff) Zoomable
onZoom handler zoomable  = runEffFn2 onZoomImpl zoomable (mkEffFn1 handler)

foreign import onZoomImpl :: forall eff.
  EffFn2 (d3::D3|eff)
        Zoomable               -- 1st argument for EffFn2, the zoomable itself
        (EffFn1 (d3::D3|eff)   -- 2nd argument for EffFn2, the handler function
                ZoomEvent          -- arg for handler EffFn1, a D3 ZoomEvent
                Unit)              -- Unit, result of handler
        Zoomable               -- result of EffFn2, returns zoomable for "fluid interface" / monadic chain
