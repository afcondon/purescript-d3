module Graphics.D3.Layout.Base
  ( class GraphLayout
  , size
  , nodes
  , links
  , Node()
  , Link()
  , GraphData
  ) where

import Control.Monad.Eff
import Graphics.D3.Base

type GraphData =
  { nodes :: Array Node
  , links :: Array Link
  }

type Node = { x :: Number, y :: Number }
type Link = { source :: Node, target :: Node }

class GraphLayout l where
  nodes :: forall a eff. Array Node -> l -> Eff (d3::D3|eff) l
  links :: forall a eff. Array Link -> l -> Eff (d3::D3|eff) l
  size  :: forall d eff. { width :: Number, height :: Number | d } -> l -> Eff (d3::D3|eff) l

class (GraphLayout l) <= HierarchyLayout l where
  -- TODO: children, sort, value, revalue
