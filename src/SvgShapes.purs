module Graphics.D3.SvgShapes (svgDiagonal, Diagonal) where

import Control.Monad.Eff
import Data.Function.Eff
import Graphics.D3.Base (D3)

type Diagonal = { source :: { x :: Number, y :: Number}, target :: { x :: Number, y :: Number }}

-- foreign import svgDiagonalImpl :: forall eff. EffFn1 (d3::D3|eff) Diagonal String
--
-- svgDiagonal :: forall eff. Diagonal -> Eff (d3::D3|eff) String
-- svgDiagonal = runEffFn1 svgDiagonalImpl

foreign import svgDiagonal :: Diagonal -> String
