module Graphics.D3.Events where

import Prelude
import Data.Maybe
import DOM.Events

foreign import currentD3Event :: forall eff. Eff(d3::D3|eff) Maybe Event
