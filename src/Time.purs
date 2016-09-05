module Graphics.D3.Time (
	TimeScale(),
	timeScale
  ) where
-- import Data.Foreign.EasyFFI
import Control.Monad.Eff (Eff)
import Data.Date (Date)
import Graphics.D3.Base (D3)
import Graphics.D3.Scale (class Scale)
import Graphics.D3.Unsafe (unsafeToFunction, unsafeCopy, unsafeRange, unsafeDomain)

foreign import data TimeScale :: * -> * -> *
foreign import timeScale :: forall r eff. Eff (d3::D3|eff) (TimeScale Date r)

instance scaleTime :: Scale TimeScale where
  domain = unsafeDomain
  range = unsafeRange
  copy = unsafeCopy
  toFunction = unsafeToFunction
