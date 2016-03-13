module Graphics.D3.Unsafe (
	unsafeDomain,
	unsafeRange,
	unsafeCopy,
	unsafeToFunction
  ) where

import Graphics.D3.Base
import Control.Monad.Eff
import Data.Function.Eff
-- import Data.Foreign.EasyFFI

foreign import unsafeDomainImpl     :: forall s d r eff.  EffFn2 (d3::D3|eff) (Array d) (s d r) (s d r)
foreign import unsafeRangeImpl      :: forall s d r eff.  EffFn2 (d3::D3|eff) (Array r) (s d r) (s d r)
foreign import unsafeCopyImpl       :: forall s d r eff.  EffFn1 (d3::D3|eff) (s d r) (s d r)
foreign import unsafeToFunctionImpl :: forall s d r eff.  EffFn1 (d3::D3|eff) (s d r) (d -> r)

unsafeDomain :: forall s d r eff. Array d -> s d r  -> Eff (d3::D3|eff) (s d r)
unsafeDomain =      runEffFn2 unsafeDomainImpl
unsafeRange :: forall s d r eff. Array r -> s d r   -> Eff (d3::D3|eff) (s d r)
unsafeRange =       runEffFn2 unsafeRangeImpl
unsafeCopy :: forall s d r eff. s d r               -> Eff (d3::D3|eff) (s d r)
unsafeCopy =        runEffFn1 unsafeCopyImpl
unsafeToFunction :: forall s d r eff. s d r         -> Eff (d3::D3|eff) (d -> r)
unsafeToFunction =  runEffFn1 unsafeToFunctionImpl
