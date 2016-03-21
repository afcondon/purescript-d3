module Graphics.D3.EffFnThis (mkEffFnThis1, EffFnThis1) where

import Graphics.D3.Base
import Control.Monad.Eff

foreign import data EffFnThis1 :: # ! -> * -> * -> *

foreign import mkEffFnThis1  :: forall eff a r. (D3Element -> Eff eff r) -> EffFnThis1 eff D3Element r
