module Graphics.D3.SvgShapes where

import Control.Monad.Eff
import Graphics.D3.Base (D3)

foreign import svgDiagonal :: forall eff. { source :: { x :: Number, y :: Number}
                                          , target :: { x :: Number, y :: Number } }
                                          -> Eff (d3::D3|eff) String
