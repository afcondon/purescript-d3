module Graphics.D3.EffFnExtra
   (ElementAndDatum()
  , CallbackParamBlock()
  , PropertyName
  , mkCallbackWithT
  , mkCallbackWithProp
  , D3EffCallback
  , D3EffCallbackP) where

import Graphics.D3.Base
import Control.Monad.Eff
import Data.Tuple

type PropertyName = String

foreign import data D3EffCallback :: # ! -> * -> * -> *
foreign import data D3EffCallbackP :: # ! -> * -> * -> * -> *

type ElementAndDatum d = Tuple d D3Element
foreign import mkCallbackWithT    :: forall eff d r.    (ElementAndDatum d -> Eff eff r)
  -> D3EffCallback eff (ElementAndDatum d) r

type CallbackParamBlock d p = { datum :: d, elem :: D3Element, prop :: p }
foreign import mkCallbackWithProp :: forall eff d p r.  (CallbackParamBlock d p -> Eff eff r) -> PropertyName
  -> D3EffCallbackP eff (CallbackParamBlock d p) PropertyName r
