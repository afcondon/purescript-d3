module Graphics.D3.Time (
	TimeScale(),
	timeScale
  ) where
-- import Data.Foreign.EasyFFI
import Control.Monad.Eff
import Data.Date
import Graphics.D3.Base
import Graphics.D3.Scale
import Graphics.D3.Unsafe

foreign import data TimeScale :: * -> * -> *
foreign import timeScale :: forall r eff. Eff (d3::D3|eff) (TimeScale Date r)

instance scaleTime :: Scale TimeScale where
  domain = unsafeDomain
  range = unsafeRange
  copy = unsafeCopy
  toFunction = unsafeToFunction
