module Graphics.D3.Event where

import Prelude
import Data.Maybe
import Control.Monad.Eff
import Graphics.D3.Base
import DOM.Event.Types

-- | Might be necessary to expose D3Event sometime but for now it suffices to pull the SourceEvent out of it
-- | and since this is a DOM Event we can use the types defined in purescript-dom

-- foreign import data D3Event :: *

-- | returning a Maybe Event because the SourceEvent might be null 
foreign import currentD3Event :: forall eff. Eff(d3::D3|eff) (Maybe Event)
