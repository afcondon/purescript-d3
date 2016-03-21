module Graphics.D3.Event where

import Prelude
import Data.Maybe
import Control.Monad.Eff
import Graphics.D3.Base
import DOM.Event.Types

foreign import currentD3Event :: forall eff. Eff(d3::D3|eff) (Maybe Event)
