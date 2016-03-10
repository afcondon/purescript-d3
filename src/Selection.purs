module Graphics.D3.Selection
  ( Selection()
  , Update()
  , Enter()
  , Exit()
  , Transition()
  , Void()
  , class AttrValue
  , class Existing
  , class Appendable
  , class Clickable
  , rootSelect
  , rootSelectAll
  , select
  , selectAll
  , bindData
  , enter
  , exit
  , transition
  , append
  , remove
  , attr
  , attr'
  , attr''
  , style
  , style'
  , style''
  , text
  , text'
  , text''
  , delay
  , delay'
  , delay''
  , duration
  , duration'
  , duration''
  , onClick
  , onDoubleClick
  ) where

import Graphics.D3.Base
import Control.Monad.Eff
import Control.Monad.Eff.Console

import Data.Foreign
import Data.Foreign.EasyFFI

import Data.Nullable
import Data.Function.Eff

import Prelude ( Unit() )

ffi = unsafeForeignFunction

-- The "selection-y" types, parameterized by the type of their bound data
foreign import data Selection :: * -> *
foreign import data Update :: * -> *
foreign import data Enter :: * -> *
foreign import data Transition :: * -> *

-- Exit selections have the same semantics as regular selections
type Exit d = Selection d

-- The (uninhabited) type of an unbound selection's data
data Void

-- The class of types which element attribute values can have (numbers and strings)
class AttrValue a

instance attrValNumber :: AttrValue Number
instance attrValString :: AttrValue String

-- | OLD "EasyFFI" way
-- rootSelect = ffi ["selector", ""] "d3.select(selector)"

-- | NEW "FunctionEff" way
-- rootSelect :: String -> D3Eff (Selection Void)
foreign import rootSelectImpl :: forall eff.
  EffFn1 (d3 :: D3 | eff) String (Selection Void)

rootSelect :: forall eff.
  String -> Eff (d3 :: D3 | eff) (Selection Void)
rootSelect = runEffFn1 rootSelectImpl


-- | OLD "EasyFFI" way
-- unsafeRemove :: forall s. s -> D3Eff Unit
-- unsafeRemove = ffi ["selection", ""] "selection.remove()"

-- | NEW "FunctionEff" way
foreign import unsafeRemoveImpl :: forall s eff.
  EffFn1 (d3 :: D3 | eff) s Unit

unsafeRemove :: forall s eff.
  s -> Eff (d3 :: D3 | eff) Unit
unsafeRemove = runEffFn1 unsafeRemoveImpl

-- | Now try to generalize example to the actual callbacks we need in D3
-- original typeclass definition
-- class Clickable c where
--   onClick :: forall eff r. (Foreign -> Eff eff r) -> c -> D3Eff c
-- instance clickableSelection :: Clickable (Selection a) where
--   onClick = ffi ["callback", "clickable", ""] "clickable.on('click', function(data) { callback(data)(); })"

-- | new typeclass definition
class Clickable c where
  onClick :: forall eff. (Foreign -> Eff (d3 :: D3 | eff) Unit) -> c -> Eff (d3 :: D3 | eff) c
  onDoubleClick :: forall eff. (Foreign -> Eff (d3 :: D3 | eff) Unit) -> c -> Eff (d3 :: D3 | eff) c
instance clickableSelectionI :: Clickable (Selection a) where
  onClick callback clickableSelection       = runEffFn2 onClickImpl       clickableSelection (mkEffFn1 callback)
  onDoubleClick callback clickableSelection = runEffFn2 onDoubleClickImpl clickableSelection (mkEffFn1 callback)

-- foreign function that will attach a callback to our clickable selection
foreign import onClickImpl :: forall eff a d. -- (Clickable => c)
 EffFn2 (d3 :: D3 | eff)
        (Selection a)             -- 1st argument for EffFn2, the selection itself
        (EffFn1 (d3 :: D3 | eff)  -- 2nd argument for EffFn2, the callback function
                 d                  -- 1st and only argument for EffFn1, the datum given to the callback
                 Unit)              -- result of EffFn1, callback result is just Unit
        (Selection a)             -- result of EffFn2, returns selection so that it can be chained

foreign import onDoubleClickImpl :: forall eff a d. -- (Clickable => c)
 EffFn2 (d3 :: D3 | eff)
        (Selection a)             -- 1st argument for EffFn2, the selection itself
        (EffFn1 (d3 :: D3 | eff)  -- 2nd argument for EffFn2, the callback function
                 d                  -- 1st and only argument for EffFn1, the datum given to the callback
                 Unit)              -- result of EffFn1, callback result is just Unit
        (Selection a)             -- result of EffFn2, returns selection so that it can be chained




-- purescript function that wraps the foreign function to attach callback to a clickable selection
-- attachCallBackPS :: forall eff a d. -- (Clickable => c)
--   (d -> Eff (d3 :: D3 | eff) Unit) ->
--   (Selection a) ->
--   Eff (d3 :: D3 | eff) Unit
-- attachCallBackPS callback clickableSelection =
--   runEffFn2 onSClickImpl clickableSelection (mkEffFn1 callback)







rootSelectAll :: String -> D3Eff (Selection Void)
rootSelectAll = ffi ["selector", ""] "d3.selectAll(selector)"

select :: forall d. String -> Selection d -> D3Eff (Selection d)
select = ffi ["selector", "selection", ""] "selection.select(selector)"

selectAll :: forall d. String -> Selection d -> D3Eff (Selection Void)
selectAll = ffi ["selector", "selection", ""] "selection.selectAll(selector)"

bindData :: forall oldData newData. Array newData -> Selection oldData -> D3Eff (Update newData)
bindData = ffi ["array", "selection", ""] "selection.data(array)"

enter :: forall d. Update d -> D3Eff (Enter d)
enter = ffi ["update", ""] "update.enter()"

exit :: forall d. Update d -> D3Eff (Exit d)
exit = ffi ["update", ""] "update.exit()"

transition :: forall s d. (Existing s) => s d -> D3Eff (Transition d)
transition = ffi ["selection", ""] "selection.transition()"

unsafeAppend :: forall x y. String -> x -> D3Eff y
unsafeAppend = ffi ["tag", "selection", ""] "selection.append(tag)"

unsafeAttr :: forall d v s. (AttrValue v) => String -> v -> s -> D3Eff s
unsafeAttr = ffi ["key", "val", "selection", ""] "selection.attr(key, val)"

unsafeAttr' :: forall d v s. (AttrValue v) => String -> (d -> v) -> s -> D3Eff s
unsafeAttr' = ffi ["key", "val", "selection", ""] "selection.attr(key, val)"

unsafeAttr'' :: forall d v s. (AttrValue v) => String -> (d -> Number -> v) -> s -> D3Eff s
unsafeAttr'' = ffi
  ["key", "val", "selection", ""]
  "selection.attr(key, function (d, i) { return val(d)(i); })"

unsafeStyle :: forall d s. String -> String -> s -> D3Eff s
unsafeStyle = ffi ["key", "val", "selection", ""] "selection.style(key, val)"

unsafeStyle' :: forall d s. String -> (d -> String) -> s -> D3Eff s
unsafeStyle' = ffi ["key", "val", "selection", ""] "selection.style(key, val)"

unsafeStyle'' :: forall d s. String -> (d -> Number -> String) -> s -> D3Eff s
unsafeStyle'' = ffi
  ["key", "val", "selection", ""]
  "selection.style(key, function (d, i) { return val(d)(i); })"

unsafeText :: forall d s. String -> s -> D3Eff s
unsafeText = ffi ["text", "selection", ""] "selection.text(text)"

unsafeText' :: forall d s. (d -> String) -> s -> D3Eff s
unsafeText' = ffi ["text", "selection", ""] "selection.text(text)"

unsafeText'' :: forall d s. (d -> Number -> String) -> s -> D3Eff s
unsafeText'' = ffi
  ["text", "selection", ""]
  "selection.text(function (d, i) { return text(d)(i); })"

unsafeOnClick :: forall eff c i r. (Clickable c) => (i -> Eff eff r) -> c -> D3Eff c
unsafeOnClick = ffi ["callback", "clickable", ""] "clickable.on('click', function(data) { callback(data)(); })"

unsafeOnDoubleClick :: forall eff c i r. (Clickable c) => (i -> Eff eff r) -> c -> D3Eff c
unsafeOnDoubleClick = ffi ["callback", "clickable", ""] "clickable.on('dblclick', function (data) { callback(data)(); })"

-- Transition-only stuff
delay :: forall d. Number -> Transition d -> D3Eff (Transition d)
delay = ffi ["delay", "transition", ""] "transition.delay(delay)"

delay' :: forall d. (d -> Number) -> Transition d -> D3Eff (Transition d)
delay' = ffi ["delay", "transition", ""] "transition.delay(delay)"

delay'' :: forall d. (d -> Number -> Number) -> Transition d -> D3Eff (Transition d)
delay'' = ffi
  ["delay", "transition", ""]
  "transition.delay(function (d, i) { return delay(d)(i); })"

duration :: forall d. Number -> Transition d -> D3Eff (Transition d)
duration = ffi ["duration", "transition", ""] "transition.duration(duration)"

duration' :: forall d. (d -> Number) -> Transition d -> D3Eff (Transition d)
duration' = ffi ["duration", "transition", ""] "transition.duration(duration)"

duration'' :: forall d. (d -> Number -> Number) -> Transition d -> D3Eff (Transition d)
duration'' = ffi
  ["duration", "transition", ""]
  "transition.duration(function (d, i) { return duration(d)(i); })"

-- Selection-y things which can be appended to / inserted into
class Appendable s where
  append :: forall d. String -> s d -> D3Eff (Selection d)

instance appendableSelection  :: Appendable Selection where
  append = unsafeAppend

instance appendableUpdate     :: Appendable Update where
  append = unsafeAppend

instance appendableEnter      :: Appendable Enter where
  append = unsafeAppend

-- Selection-y things that contain existing DOM elements
class Existing s where
  attr :: forall d v. (AttrValue v) => String -> v -> s d -> D3Eff (s d)
  attr' :: forall d v. (AttrValue v) => String -> (d -> v) -> s d -> D3Eff (s d)
  attr'' :: forall d v. (AttrValue v) => String -> (d -> Number -> v) -> s d -> D3Eff (s d)
  style :: forall d. String -> String -> s d -> D3Eff (s d)
  style' :: forall d. String -> (d -> String) -> s d -> D3Eff (s d)
  style'' :: forall d. String -> (d -> Number -> String) -> s d -> D3Eff (s d)
  text :: forall d. String -> s d -> D3Eff (s d)
  text' :: forall d. (d -> String) -> s d -> D3Eff (s d)
  text'' :: forall d. (d -> Number -> String) -> s d -> D3Eff (s d)
  remove :: forall d. s d -> D3Eff Unit

instance existingSelection :: Existing Selection where
  attr = unsafeAttr
  attr' = unsafeAttr'
  attr'' = unsafeAttr''
  style = unsafeStyle
  style' = unsafeStyle'
  style'' = unsafeStyle''
  text = unsafeText
  text' = unsafeText'
  text'' = unsafeText''
  remove = unsafeRemove

instance existingUpdate :: Existing Update where
  attr = unsafeAttr
  attr' = unsafeAttr'
  attr'' = unsafeAttr''
  style = unsafeStyle
  style' = unsafeStyle'
  style'' = unsafeStyle''
  text = unsafeText
  text' = unsafeText'
  text'' = unsafeText''
  remove = unsafeRemove

instance existingTransition :: Existing Transition where
  attr = unsafeAttr
  attr' = unsafeAttr'
  attr'' = unsafeAttr''
  style = unsafeStyle
  style' = unsafeStyle'
  style'' = unsafeStyle''
  text = unsafeText
  text' = unsafeText'
  text'' = unsafeText''
  remove = unsafeRemove
