module Graphics.D3.Util
  ( Magnitude
  , min
  , max
  , min'
  , max'
  , extent
  , extent'
  , (..)
  , (...)
  ) where

import Prelude ( ($), (>>=), flip )
import Data.Date
import Control.Monad.Eff
import Data.Function.Eff

-- import Data.Foreign.EasyFFI

class Magnitude n

instance numberMagnitude :: Magnitude Number
instance dateMagnitude   :: Magnitude JSDate

foreign import min'     :: forall d m. (Magnitude m) => (d -> m) -> Array d -> m
foreign import max'     :: forall d m. (Magnitude m) => (d -> m) -> Array d -> m
foreign import min      :: forall m.   (Magnitude m) => Array m -> m
foreign import max      :: forall d m. (Magnitude m) => Array m -> m
-- extent takes a data array and returns [min,max]
-- not restricted to Number, i.e. also works with time
foreign import extent   :: forall m.   (Magnitude m) => Array m -> Array m
foreign import extent'  :: forall d m. (Magnitude m) => (d->m) -> Array d -> Array m

-- Syntactic sugar to make chained monadic statements look similar to the
-- "fluid interface" style of chained method calls in JavaScript
(..) = (>>=)

-- Reversed function application, useful for applying extended monadic chains
-- to already-obtained values
(...) = flip ($)
