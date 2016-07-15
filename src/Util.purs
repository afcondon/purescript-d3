module Graphics.D3.Util
  ( class Magnitude
  , min
  , max
  , minFn
  , maxFn
  , extent
  , extentFn
  , (..)
  , flipply, (...)
  ) where

import Prelude ( bind, ($) )
import Data.Date (Date)
import Data.Function (flip)

-- import Data.Foreign.EasyFFI

class Magnitude n

instance numberMagnitude :: Magnitude Number
instance dateMagnitude   :: Magnitude Date

foreign import minFn     :: forall d m. (Magnitude m) => (d -> m) -> Array d -> m
foreign import maxFn     :: forall d m. (Magnitude m) => (d -> m) -> Array d -> m
foreign import min      :: forall m.    (Magnitude m) =>             Array m -> m
foreign import max      :: forall m.    (Magnitude m) =>             Array m -> m
-- extent takes a data array and returns [min,max]
-- not restricted to Number, i.e. also works with time
foreign import extent   :: forall m.    (Magnitude m) =>           Array m -> Array m
foreign import extentFn  :: forall d m. (Magnitude m) => (d->m) -> Array d -> Array m

-- Syntactic sugar to make chained monadic statements look similar to the
-- "fluid interface" style of chained method calls in JavaScript
infixl 1 bind as ..        -- (..) = (>>=)
-- >>= is defined in Prelude as
-- infixl 1 >>=


-- Reversed function application, useful for applying extended monadic chains
-- to already-obtained values
flipply = flip ($)
infixr 0 flipply as ...   -- (...) = flip ($)
-- ($) is defined in Prelude as
-- infixr 0 $
