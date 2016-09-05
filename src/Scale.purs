module Graphics.D3.Scale
  ( class Scale
  , class Quantitative
  , LinearScale()
  , PowerScale()
  , LogScale()
  , QuantizeScale()
  , QuantileScale()
  , ThresholdScale()
  , OrdinalScale()
  , linearScale
  , powerScale
  , sqrtScale
  , logScale
  , quantizeScale
  , quantileScale
  , thresholdScale
  , ordinalScale
  , domain
  , range
  , copy
  , toFunction
  , invert
  , rangeRound
  , interpolate
  , clamp
  , nice
  , getTicks
  , getTickFormat
  , exponent
  , base
  , rangePoints
  , rangeBands
  , rangeRoundBands
  , rangeBand
  , rangeExtent
  ) where

import Control.Monad.Eff (Eff)
import Data.Function.Eff (EffFn1, EffFn5, EffFn4, EffFn2, EffFn3, runEffFn1, runEffFn5, runEffFn4, runEffFn2, runEffFn3)
import Graphics.D3.Base (D3)
import Graphics.D3.Interpolate (Interpolator)
import Graphics.D3.Unsafe (unsafeToFunction, unsafeCopy, unsafeRange, unsafeDomain)

import Data.Tuple (Tuple)
import Data.Maybe (Maybe(..))

-- A base class for all scale types

class Scale s where
  domain ::     forall d r eff. Array d -> s d r -> Eff (d3::D3|eff) (s d r)
  range ::      forall d r eff. Array r -> s d r -> Eff (d3::D3|eff) (s d r)
  copy ::       forall d r eff. s d r            -> Eff (d3::D3|eff) (s d r)
  toFunction :: forall d r eff. s d r            -> Eff (d3::D3|eff) (d -> r)

-- Quantitative (numeric domain) scales

class Quantitative q where
  invert ::         forall eff.   q Number Number                       -> Eff (d3::D3|eff) (Number -> Number)
  rangeRound ::     forall eff.   Array Number -> q Number Number       -> Eff (d3::D3|eff) (q Number Number)
  interpolate ::    forall r eff. Interpolator r -> q Number r          -> Eff (d3::D3|eff) (q Number r)
  clamp ::          forall r eff. Boolean -> q Number r                 -> Eff (d3::D3|eff) (q Number r)
  nice ::           forall r eff. Maybe Number -> q Number r            -> Eff (d3::D3|eff) (q Number r)
  getTicks ::       forall r eff. Maybe Number -> q Number r            -> Eff (d3::D3|eff) (Array Number)
  getTickFormat ::  forall r eff. Number -> Maybe String -> q Number r  -> Eff (d3::D3|eff) (Number -> String)

foreign import unsafeInvertImpl         :: forall q eff.    EffFn1 (d3::D3|eff) (q Number Number)                (Number -> Number)
foreign import unsafeRangeRoundImpl     :: forall q eff.    EffFn2 (d3::D3|eff) (Array Number) (q Number Number) (q Number Number)
foreign import unsafeInterpolateImpl    :: forall q r eff.  EffFn2 (d3::D3|eff) (Interpolator r) (q Number r)    (q Number r)
foreign import unsafeClampImpl          :: forall q r eff.  EffFn2 (d3::D3|eff) Boolean (q Number r)             (q Number r)
-- these next foreign functions handle the separated cases for their PureScript wrappers
foreign import unsafeNiceImplC          :: forall q r eff.  EffFn2 (d3::D3|eff) Number (q Number r) (q Number r)              -- 1st param Just
foreign import unsafeNiceImpl           :: forall q r eff.  EffFn1 (d3::D3|eff) (q Number r) (q Number r)                     -- 1st param Nothing
foreign import unsafeGetTicksImplC      :: forall q r eff.  EffFn2 (d3::D3|eff) Number (q Number r) (Array Number)            -- 1st param Just
foreign import unsafeGetTicksImpl       :: forall q r eff.  EffFn1 (d3::D3|eff) (q Number r) (Array Number)                   -- 1st param Nothing
foreign import unsafeGetTickFormatImplS :: forall q r eff.  EffFn3 (d3::D3|eff) Number String (q Number r) (Number -> String) -- 2nd param Just
foreign import unsafeGetTickFormatImpl  :: forall q r eff.  EffFn2 (d3::D3|eff) Number (q Number r) (Number -> String)        -- 2nd param Nothing

unsafeInvert :: forall q eff.   q Number Number -> Eff (d3::D3|eff) (Number -> Number)
unsafeInvert = runEffFn1 unsafeInvertImpl

unsafeRangeRound :: forall q eff.   Array Number -> q Number Number -> Eff (d3::D3|eff) (q Number Number)
unsafeRangeRound = runEffFn2 unsafeRangeRoundImpl

unsafeInterpolate :: forall q r eff. Interpolator r -> q Number r -> Eff (d3::D3|eff) (q Number r)
unsafeInterpolate = runEffFn2 unsafeInterpolateImpl

unsafeClamp :: forall q r eff. Boolean -> q Number r -> Eff (d3::D3|eff) (q Number r)
unsafeClamp = runEffFn2 unsafeClampImpl

unsafeNice :: forall q r eff. Maybe Number -> q Number r -> Eff (d3::D3|eff) (q Number r)
unsafeNice (Just count) q = (runEffFn2 unsafeNiceImplC) count q
unsafeNice Nothing      q = (runEffFn1 unsafeNiceImpl)        q

unsafeGetTicks :: forall q r eff. Maybe Number -> q Number r -> Eff (d3::D3|eff) (Array Number)
unsafeGetTicks (Just count) q = (runEffFn2 unsafeGetTicksImplC) count q
unsafeGetTicks Nothing      q = (runEffFn1 unsafeGetTicksImpl)        q

unsafeGetTickFormat :: forall q r eff. Number -> (Maybe String) -> q Number r -> Eff (d3::D3|eff) (Number -> String)
unsafeGetTickFormat count (Just format) q = (runEffFn3 unsafeGetTickFormatImplS) count format q
unsafeGetTickFormat count Nothing       q = (runEffFn2 unsafeGetTickFormatImpl)  count        q

-- Scale types

foreign import data LinearScale :: * -> * -> *
foreign import data IdentityScale :: * -> * -> *
foreign import data PowerScale :: * -> * -> *
foreign import data LogScale :: * -> * -> *
foreign import data QuantizeScale :: * -> * -> *
foreign import data QuantileScale :: * -> * -> *
foreign import data ThresholdScale :: * -> * -> *
foreign import data OrdinalScale :: * -> * -> *

-- Scale constructors

foreign import linearScale          :: forall r eff.    Eff (d3::D3|eff) (LinearScale Number r)
foreign import powerScale           :: forall r eff.    Eff (d3::D3|eff) (PowerScale Number r)
foreign import sqrtScale            :: forall r eff.    Eff (d3::D3|eff) (PowerScale Number r)
foreign import logScale             :: forall r eff.    Eff (d3::D3|eff) (LogScale Number r)
foreign import quantizeScale        :: forall r eff.    Eff (d3::D3|eff) (QuantizeScale Number r)
foreign import quantileScale        :: forall r eff.    Eff (d3::D3|eff) (QuantileScale Number r)
foreign import thresholdScale       :: forall r eff.    Eff (d3::D3|eff) (ThresholdScale Number r)
foreign import ordinalScale         :: forall d r eff.  Eff (d3::D3|eff) (OrdinalScale d r)

-- foreign Scale methods

foreign import exponentImpl         :: forall r eff.    EffFn2 (d3::D3|eff) Number (PowerScale Number r) (PowerScale Number r)
foreign import baseImpl             :: forall r eff.    EffFn2 (d3::D3|eff) Number (LogScale Number r) (LogScale Number r)
foreign import quantilesImpl        :: forall r eff.    EffFn1 (d3::D3|eff) (QuantileScale Number r) (QuantileScale Number r)
foreign import rangePointsImpl      :: forall d eff.    EffFn4 (d3::D3|eff) Number Number Number (OrdinalScale d Number) (OrdinalScale d Number)
foreign import rangeBandsImpl       :: forall d eff.    EffFn5 (d3::D3|eff) Number Number Number Number (OrdinalScale d Number) (OrdinalScale d Number)
foreign import rangeRoundBandsImpl  :: forall d eff.    EffFn5 (d3::D3|eff) Number Number Number Number (OrdinalScale d Number) (OrdinalScale d Number)
foreign import rangeBandImpl        :: forall d eff.  EffFn1 (d3::D3|eff) (OrdinalScale d Number) Number
foreign import rangeExtentImpl      :: forall d eff.  EffFn1 (d3::D3|eff) (OrdinalScale d Number) (Tuple Number Number)

-- Power scale methods
exponent :: forall r eff. Number -> PowerScale Number r -> Eff (d3::D3|eff) (PowerScale Number r)
exponent = runEffFn2 exponentImpl

-- Log scale methods
base :: forall r eff. Number -> LogScale Number r -> Eff (d3::D3|eff) (LogScale Number r)
base = runEffFn2 baseImpl

-- Quantile scale methods
quantiles :: forall r eff. QuantileScale Number r -> Eff (d3::D3|eff) (QuantileScale Number r)
quantiles = runEffFn1 quantilesImpl

-- Ordinal scale methods
rangePoints :: forall d eff. Number -> Number -> Number -> OrdinalScale d Number -> Eff (d3::D3|eff) (OrdinalScale d Number)
rangePoints = runEffFn4 rangePointsImpl

rangeBands :: forall d eff. Number -> Number -> Number -> Number -> OrdinalScale d Number -> Eff (d3::D3|eff) (OrdinalScale d Number)
rangeBands = runEffFn5 rangeBandsImpl

rangeRoundBands :: forall d eff. Number -> Number -> Number -> Number -> OrdinalScale d Number -> Eff (d3::D3|eff) (OrdinalScale d Number)
rangeRoundBands = runEffFn5 rangeRoundBandsImpl

rangeBand :: forall d eff. OrdinalScale d Number -> Eff (d3::D3|eff) Number                  -- suspect no Eff here TODO
rangeBand = runEffFn1 rangeBandImpl

rangeExtent :: forall d eff. OrdinalScale d Number -> Eff (d3::D3|eff) (Tuple Number Number) -- suspect no Eff here TODO
rangeExtent = runEffFn1 rangeExtentImpl

-- Scale class instances

instance scaleLinear :: Scale LinearScale where
  domain = unsafeDomain
  range = unsafeRange
  copy = unsafeCopy
  toFunction = unsafeToFunction

instance quantitativeLinear :: Quantitative LinearScale where
  invert = unsafeInvert
  rangeRound = unsafeRangeRound
  interpolate = unsafeInterpolate
  clamp = unsafeClamp
  nice = unsafeNice
  getTicks = unsafeGetTicks
  getTickFormat = unsafeGetTickFormat

instance scalePower :: Scale PowerScale where
  domain = unsafeDomain
  range = unsafeRange
  copy = unsafeCopy
  toFunction = unsafeToFunction

instance quantitativePower :: Quantitative PowerScale where
  invert = unsafeInvert
  rangeRound = unsafeRangeRound
  interpolate = unsafeInterpolate
  clamp = unsafeClamp
  nice = unsafeNice
  getTicks = unsafeGetTicks
  getTickFormat = unsafeGetTickFormat

instance scaleLog :: Scale LogScale where
  domain = unsafeDomain
  range = unsafeRange
  copy = unsafeCopy
  toFunction = unsafeToFunction

instance quantitativeLog :: Quantitative LogScale where
  invert = unsafeInvert
  rangeRound = unsafeRangeRound
  interpolate = unsafeInterpolate
  clamp = unsafeClamp
  nice = unsafeNice
  getTicks = unsafeGetTicks
  getTickFormat = unsafeGetTickFormat

instance scaleQuantize :: Scale QuantizeScale where
  domain = unsafeDomain
  range = unsafeRange
  copy = unsafeCopy
  toFunction = unsafeToFunction

instance scaleQuantile :: Scale QuantileScale where
  domain = unsafeDomain
  range = unsafeRange
  copy = unsafeCopy
  toFunction = unsafeToFunction

instance scaleThreshold :: Scale ThresholdScale where
  domain = unsafeDomain
  range = unsafeRange
  copy = unsafeCopy
  toFunction = unsafeToFunction

instance scaleOrdinal :: Scale OrdinalScale where
  domain = unsafeDomain
  range = unsafeRange
  copy = unsafeCopy
  toFunction = unsafeToFunction
