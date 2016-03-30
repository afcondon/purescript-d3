module Graphics.D3.SvgShapes (svgDiagonal, Diagonal) where

type Diagonal = { source :: { x :: Number, y :: Number}, target :: { x :: Number, y :: Number }}

foreign import svgDiagonal :: Diagonal -> String
