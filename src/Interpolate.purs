module Graphics.D3.Interpolate
  ( Interpolator()
  , makeInterpolator
  ) where


-- | No need for EffFn stuff here AFAIK - Interpolator not used in current examples and i think
-- | probably needs a lot more coverage than this one function anyway (see comprehensive D3 docs) - afc

-- import Graphics.D3.Base
-- import Data.Function.Eff (EffFn1, runEffFn1)

foreign import data Interpolator :: * -> *

foreign import makeInterpolator :: forall a. (a -> a -> Number -> a) -> Interpolator a
