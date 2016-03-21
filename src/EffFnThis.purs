module Graphics.D3.EffFnThis (ElementAndDatum(), mkEffFnThis1, EffFnThis1, mkEffFnTuple1, EffFnTuple1) where

import Graphics.D3.Base
import Control.Monad.Eff
import Data.Tuple

foreign import data EffFnThis1  :: # ! -> * -> * -> *
foreign import data EffFnTuple1 :: # ! -> * -> * -> *

type ElementAndDatum d = Tuple d D3Element

foreign import mkEffFnThis1  :: forall eff d r. (D3Element -> Eff eff r) -> EffFnThis1 eff D3Element r

foreign import mkEffFnTuple1  :: forall eff d r. (ElementAndDatum d -> Eff eff r) -> EffFnTuple1 eff (ElementAndDatum d) r
