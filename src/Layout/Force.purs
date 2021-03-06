module Graphics.D3.Layout.Force
  ( ForceLayout(..)
  , forceLayout
  , linkDistance
  , linkStrength
  , friction
  , charge
  , chargeDistance
  , theta
  , gravity
  , start
  , alpha
  , resume
  , stop
  , tick
  , drag
  , onDragStart
  , onTick
  , createDrag
  ) where

import Prelude (Unit)
import Control.Monad.Eff (Eff)
import Data.Function.Eff (EffFn2, EffFn1, runEffFn2, mkEffFn1, runEffFn1)
import Data.Foreign (Foreign)

import Graphics.D3.Base (D3)
import Graphics.D3.Selection (Selection)
import Graphics.D3.Layout.Base (class GraphLayout, Link, Node)

foreign import data ForceLayout :: *

foreign import forceLayout :: forall eff. Eff (d3::D3|eff) ForceLayout

instance forceGraphLayout :: GraphLayout ForceLayout where
  size dims = (runEffFn2 sizeImpl) [dims.width, dims.height] -- first param Dims, second ForceLayout
  nodes     = runEffFn2 nodesImpl
  links     = runEffFn2 linksImpl

linkDistance    :: forall eff. Number -> ForceLayout -> Eff (d3::D3|eff) ForceLayout
linkDistance = runEffFn2 linkDistanceImpl

linkStrength    :: forall eff. Number -> ForceLayout -> Eff (d3::D3|eff) ForceLayout
linkStrength = runEffFn2 linkStrengthImpl

friction        :: forall eff. Number -> ForceLayout -> Eff (d3::D3|eff) ForceLayout
friction = runEffFn2 frictionImpl

charge          :: forall eff. Number -> ForceLayout -> Eff (d3::D3|eff) ForceLayout
charge = runEffFn2 chargeImpl

chargeDistance  :: forall eff. Number -> ForceLayout -> Eff (d3::D3|eff) ForceLayout
chargeDistance = runEffFn2 chargeDistanceImpl

theta           :: forall eff. Number -> ForceLayout -> Eff (d3::D3|eff) ForceLayout
theta = runEffFn2 thetaImpl

gravity         :: forall eff. Number -> ForceLayout -> Eff (d3::D3|eff) ForceLayout
gravity = runEffFn2 gravityImpl

alpha           :: forall eff. Number -> ForceLayout -> Eff (d3::D3|eff) ForceLayout
alpha = runEffFn2 alphaImpl

start           :: forall eff. ForceLayout           -> Eff (d3::D3|eff) ForceLayout
start = runEffFn1 startImpl

resume          :: forall eff. ForceLayout           -> Eff (d3::D3|eff) ForceLayout
resume = runEffFn1 resumeImpl

stop            :: forall eff. ForceLayout           -> Eff (d3::D3|eff) ForceLayout
stop = runEffFn1 stopImpl

tick            :: forall eff. ForceLayout           -> Eff (d3::D3|eff) ForceLayout
tick = runEffFn1 tickImpl

drag            :: forall eff. ForceLayout           -> Eff (d3::D3|eff) ForceLayout
drag = runEffFn1 dragImpl

createDrag      :: forall s eff. ForceLayout -> Selection s -> Eff (d3::D3|eff) (Selection s)
createDrag = runEffFn2 createDragImpl

-- foreign import function signatures

foreign import sizeImpl  :: forall eff. EffFn2 (d3::D3|eff) (Array Number) ForceLayout ForceLayout
foreign import nodesImpl :: forall eff. EffFn2 (d3::D3|eff) (Array Node)   ForceLayout ForceLayout
foreign import linksImpl :: forall eff. EffFn2 (d3::D3|eff) (Array Link)   ForceLayout ForceLayout

foreign import linkDistanceImpl    ::  forall eff. EffFn2 (d3::D3|eff) Number    ForceLayout     ForceLayout
foreign import linkStrengthImpl    ::  forall eff. EffFn2 (d3::D3|eff) Number    ForceLayout     ForceLayout
foreign import frictionImpl        ::  forall eff. EffFn2 (d3::D3|eff) Number    ForceLayout     ForceLayout
foreign import chargeImpl          ::  forall eff. EffFn2 (d3::D3|eff) Number    ForceLayout     ForceLayout
foreign import chargeDistanceImpl  ::  forall eff. EffFn2 (d3::D3|eff) Number    ForceLayout     ForceLayout
foreign import thetaImpl           ::  forall eff. EffFn2 (d3::D3|eff) Number    ForceLayout     ForceLayout
foreign import gravityImpl         ::  forall eff. EffFn2 (d3::D3|eff) Number    ForceLayout     ForceLayout
foreign import alphaImpl           ::  forall eff. EffFn2 (d3::D3|eff) Number    ForceLayout     ForceLayout
foreign import startImpl           ::  forall eff. EffFn1 (d3::D3|eff) ForceLayout               ForceLayout
foreign import resumeImpl          ::  forall eff. EffFn1 (d3::D3|eff) ForceLayout               ForceLayout
foreign import stopImpl            ::  forall eff. EffFn1 (d3::D3|eff) ForceLayout               ForceLayout
foreign import tickImpl            ::  forall eff. EffFn1 (d3::D3|eff) ForceLayout               ForceLayout
foreign import dragImpl            ::  forall eff.  EffFn1 (d3::D3|eff) ForceLayout ForceLayout
foreign import createDragImpl      ::  forall eff s. EffFn2 (d3::D3|eff) ForceLayout (Selection s) (Selection s)

-- | these are the tricky ones, callbacks for Ticks (in force update) and Drags (generally)
onTick          :: forall eff e r. (Foreign -> Eff (d3::D3|eff) (e r)) -> ForceLayout -> Eff (d3::D3|eff) ForceLayout
onTick callback force       = runEffFn2 onTickImpl        (mkEffFn1 callback) force

foreign import onTickImpl :: forall eff e r.
  EffFn2 (d3::D3|eff)
         (EffFn1 (d3::D3|eff) Foreign (e r)) -- callback wrapped by mkEffFn1
         ForceLayout
         ForceLayout

onDragStart     :: forall eff. (Eff (d3::D3|eff) Unit) -> ForceLayout -> Eff (d3::D3|eff) ForceLayout
onDragStart callback force  = runEffFn2 onDragStartImpl force callback

foreign import onDragStartImpl :: forall eff.
  EffFn2 (d3::D3|eff)
          ForceLayout
         (Eff (d3::D3|eff) Unit) -- callback
          ForceLayout
