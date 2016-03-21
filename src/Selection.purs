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
  , select'
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
  , classed
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
import Graphics.D3.EffFnThis
import Control.Monad.Eff
import Control.Monad.Eff.Console

import Data.Foreign

import Data.Nullable
import Data.Function.Eff

import Prelude ( Unit() )

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

instance attrValNumber    :: AttrValue Number
-- instance attrValNumberFn  :: AttrValue (d -> Number)
-- instance attrValNumberFn' :: AttrValue (d -> Number -> Number)
instance attrValString    :: AttrValue String
-- instance attrValStringFn  :: AttrValue (d -> String)
-- instance attrValStringFn' :: AttrValue (d -> Number -> String)
instance attrValBoolean   :: AttrValue Boolean

foreign import bindDataImpl       :: forall o n eff.   EffFn2 (d3::D3|eff) (Array n) (Selection o)                 (Update n)
foreign import selectImpl         :: forall d eff.     EffFn2 (d3::D3|eff) String (Selection d)                    (Selection d)
foreign import selectElementImpl  :: forall d eff.     EffFn1 (d3::D3|eff) D3Element                               (Selection d)
foreign import selectAllImpl      :: forall d eff.     EffFn2 (d3::D3|eff) String (Selection d)                    (Selection Void)
foreign import rootSelectImpl     :: forall eff.       EffFn1 (d3::D3|eff) String                                  (Selection Void)
foreign import rootSelectAllImpl  :: forall eff.       EffFn1 (d3::D3|eff) String                                  (Selection Void)
foreign import unsafeRemoveImpl   :: forall s eff.     EffFn1 (d3::D3|eff) s                                       Unit
foreign import enterImpl          :: forall d eff.     EffFn1 (d3::D3|eff) (Update d)                              (Enter d)
foreign import exitImpl           :: forall d eff.     EffFn1 (d3::D3|eff) (Update d)                              (Exit d)
foreign import unsafeAppendImpl   :: forall x s eff.   EffFn2 (d3::D3|eff) String x                                 s

foreign import unsafeClassedImpl  :: forall s eff.     EffFn3 (d3::D3|eff) String Boolean s                          s

foreign import unsafeStyleImpl    :: forall s eff.     EffFn3 (d3::D3|eff) String String s                          s
foreign import unsafeStyleImplP   :: forall d s eff.   EffFn3 (d3::D3|eff) String (d -> String) s                   s
foreign import unsafeStyleImplPP  :: forall d s eff.   EffFn3 (d3::D3|eff) String (d -> Number -> String) s         s

foreign import unsafeTextImpl     :: forall s eff.     EffFn2 (d3::D3|eff) String s                                 s
foreign import unsafeTextImplP    :: forall d s eff.   EffFn2 (d3::D3|eff) (d -> String) s                          s
foreign import unsafeTextImplPP   :: forall d s eff.   EffFn2 (d3::D3|eff) (d -> Number -> String) s                s

foreign import transitionImpl     :: forall s d eff.   (Existing s)  => EffFn1 (d3::D3|eff) (s d)                  (Transition d)

foreign import delayImpl          :: forall d eff.     EffFn2 (d3::D3|eff) Number (Transition d)                   (Transition d)
foreign import delayImplP         :: forall d eff.     EffFn2 (d3::D3|eff) (d -> Number) (Transition d)            (Transition d)
foreign import delayImplPP        :: forall d eff.     EffFn2 (d3::D3|eff) (d -> Number -> Number) (Transition d)  (Transition d)

foreign import durationImpl       :: forall d eff.     EffFn2 (d3::D3|eff) Number (Transition d)                   (Transition d)
foreign import durationImplP      :: forall d eff.     EffFn2 (d3::D3|eff) (d -> Number) (Transition d)            (Transition d)
foreign import durationImplPP     :: forall d eff.     EffFn2 (d3::D3|eff) (d -> Number -> Number) (Transition d)  (Transition d)

foreign import unsafeAttrImpl     :: forall s v eff.   EffFn3 (d3::D3|eff) String v s                               s

-- | ===================================================================================
rootSelect    :: forall eff.      String -> Eff (d3::D3|eff) (Selection Void)
rootSelect = runEffFn1 rootSelectImpl

unsafeRemove  :: forall s eff.    s -> Eff (d3::D3|eff) Unit
unsafeRemove = runEffFn1 unsafeRemoveImpl

rootSelectAll :: forall eff.      String -> Eff (d3::D3|eff) (Selection Void)
rootSelectAll = runEffFn1 rootSelectAllImpl

select        :: forall d eff.    String -> Selection d -> Eff (d3::D3|eff) (Selection d)
select = runEffFn2 selectImpl

select'       :: forall d eff.    D3Element -> Eff (d3::D3|eff) (Selection d)
select' = runEffFn1 selectElementImpl

selectAll     :: forall d eff.    String -> Selection d -> Eff (d3::D3|eff) (Selection Void)
selectAll = runEffFn2 selectAllImpl

enter         :: forall d eff.    Update d -> Eff (d3::D3|eff) (Enter d)
enter = runEffFn1 enterImpl

exit          :: forall d eff.    Update d -> Eff (d3::D3|eff) (Exit d)
exit = runEffFn1 exitImpl

unsafeAppend  :: forall x y eff.  String -> x -> Eff (d3::D3|eff) y
unsafeAppend = runEffFn2 unsafeAppendImpl

unsafeClassed :: forall s eff. String -> Boolean -> s -> Eff (d3::D3|eff) s
unsafeClassed = runEffFn3 unsafeClassedImpl

unsafeStyle   :: forall s eff.  String -> String -> s -> Eff (d3::D3|eff) s
unsafeStyle = runEffFn3 unsafeStyleImpl

unsafeStyle'  :: forall d s eff.  String -> (d -> String) -> s -> Eff (d3::D3|eff) s
unsafeStyle' = runEffFn3 unsafeStyleImplP

unsafeStyle'' :: forall d s eff.  String -> (d -> Number -> String) -> s -> Eff (d3::D3|eff) s
unsafeStyle'' = runEffFn3 unsafeStyleImplPP

unsafeText    :: forall s eff.  String -> s -> Eff (d3::D3|eff) s
unsafeText = runEffFn2 unsafeTextImpl

unsafeText'   :: forall d s eff.  (d -> String) -> s -> Eff (d3::D3|eff) s
unsafeText' = runEffFn2 unsafeTextImplP

unsafeText''  :: forall d s eff.  (d -> Number -> String) -> s -> Eff (d3::D3|eff) s
unsafeText'' = runEffFn2 unsafeTextImplPP

delay         :: forall d eff.    Number -> Transition d -> Eff (d3::D3|eff) (Transition d)
delay = runEffFn2 delayImpl

delay'        :: forall d eff.    (d -> Number) -> Transition d -> Eff (d3::D3|eff) (Transition d)
delay' = runEffFn2 delayImplP

delay''       :: forall d eff.    (d -> Number -> Number) -> Transition d -> Eff (d3::D3|eff) (Transition d)
delay'' = runEffFn2 delayImplPP

duration      :: forall d eff.    Number -> Transition d -> Eff (d3::D3|eff) (Transition d)
duration = runEffFn2 durationImpl

duration'     :: forall d eff.    (d -> Number) -> Transition d -> Eff (d3::D3|eff) (Transition d)
duration' = runEffFn2 durationImplP

duration''    :: forall d eff.    (d -> Number -> Number) -> Transition d -> Eff (d3::D3|eff) (Transition d)
duration'' = runEffFn2 durationImplPP

bindData      :: forall od nd eff. Array nd -> Selection od -> Eff (d3::D3|eff) (Update nd)
bindData = runEffFn2 bindDataImpl

transition    :: forall s d eff.    (Existing s)  =>  s d -> Eff (d3::D3|eff) (Transition d)
transition    = runEffFn1 transitionImpl

unsafeAttr    :: forall v s eff.    (AttrValue v) =>  String -> v -> s -> Eff (d3::D3|eff) s
unsafeAttr    = runEffFn3 unsafeAttrImpl

unsafeAttr'   :: forall d v s eff.    (AttrValue v) =>  String -> (d -> v) -> s -> Eff (d3::D3|eff) s
unsafeAttr'   = runEffFn3 unsafeAttrImpl

unsafeAttr''   :: forall d v s eff.    (AttrValue v) =>  String -> (d -> Number -> v) -> s -> Eff (d3::D3|eff) s
unsafeAttr''   = runEffFn3 unsafeAttrImpl

-- Selection-y things which can be appended to / inserted into
class Appendable s where
  append :: forall d eff. String -> s d -> Eff (d3::D3|eff) (Selection d)

instance appendableSelection  :: Appendable Selection where
  append = unsafeAppend

instance appendableUpdate     :: Appendable Update where
  append = unsafeAppend

instance appendableEnter      :: Appendable Enter where
  append = unsafeAppend

-- Selection-y things that contain existing DOM elements
class Existing s where
  attr    :: forall d v eff. (AttrValue v) => String -> v ->        s d -> Eff (d3::D3|eff) (s d)
  attr'   :: forall d v eff. (AttrValue v) => String -> (d -> v) -> s d -> Eff (d3::D3|eff) (s d)
  attr''  :: forall d v eff. (AttrValue v) => String -> (d -> Number -> Number) ->  s d -> Eff (d3::D3|eff) (s d)
  classed :: forall d eff.   String -> Boolean ->                   s d -> Eff (d3::D3|eff) (s d)
  style   :: forall d eff.   String -> String ->                    s d -> Eff (d3::D3|eff) (s d)
  style'  :: forall d eff.   String -> (d -> String) ->             s d -> Eff (d3::D3|eff) (s d)
  style'' :: forall d eff.   String -> (d -> Number  -> String) ->  s d -> Eff (d3::D3|eff) (s d)
  text    :: forall d eff.             String ->                    s d -> Eff (d3::D3|eff) (s d)
  text'   :: forall d eff.             (d -> String) ->             s d -> Eff (d3::D3|eff) (s d)
  text''  :: forall d eff.             (d -> Number -> String) ->   s d -> Eff (d3::D3|eff) (s d)
  remove  :: forall d eff.                                          s d -> Eff (d3::D3|eff) Unit

instance existingSelection :: Existing Selection where
  attr    = unsafeAttr
  attr'   = unsafeAttr'
  attr''  = unsafeAttr''
  classed = unsafeClassed
  style   = unsafeStyle
  style'  = unsafeStyle'
  style'' = unsafeStyle''
  text    = unsafeText
  text'   = unsafeText'
  text''  = unsafeText''
  remove  = unsafeRemove

instance existingUpdate :: Existing Update where
  attr    = unsafeAttr
  attr'   = unsafeAttr'
  attr''  = unsafeAttr''
  classed = unsafeClassed
  style   = unsafeStyle
  style'  = unsafeStyle'
  style'' = unsafeStyle''
  text    = unsafeText
  text'   = unsafeText'
  text''  = unsafeText''
  remove  = unsafeRemove

instance existingTransition :: Existing Transition where
  attr    = unsafeAttr
  attr'   = unsafeAttr'
  attr''  = unsafeAttr''
  classed = unsafeClassed
  style   = unsafeStyle
  style'  = unsafeStyle'
  style'' = unsafeStyle''
  text    = unsafeText
  text'   = unsafeText'
  text''  = unsafeText''
  remove  = unsafeRemove


-- So, you're _setting_ a click handler on a _Selection_ but it gets _called_ with the HTML element that
-- received the click event
-- Now, you can get the selection in JS by just doing d3.select(this) but because of our wrapper EffFn1
-- around the callback there is no "this" in the PureScript callback
-- So instead, i'm using a custom _mkEffFnThis1_ which passes on the "this" INSTEAD of the datum
    -- perhaps it will pass a Tuple of the two of them in the future
    -- perhaps this can be formalized / librarized / templatized if it works

class Clickable c where
  onClick       :: forall eff. (D3Element -> Eff (d3::D3|eff) Unit) -> c -> Eff (d3::D3|eff) c
  onDoubleClick :: forall eff. (D3Element -> Eff (d3::D3|eff) Unit) -> c -> Eff (d3::D3|eff) c
instance clickableSelectionI :: Clickable (Selection a) where
  onClick       callback clickableSelection = runEffFn2 onClickImpl        clickableSelection (mkEffFnThis1 callback)
  onDoubleClick callback clickableSelection = runEffFn2 onDoubleClickImpl  clickableSelection (mkEffFnThis1 callback)

foreign import onClickImpl :: forall eff a. -- (Clickable => c)
 EffFn2 (d3::D3|eff)
        (Selection a)               -- 1st argument for EffFn2, the selection itself
        (EffFnThis1 (d3::D3|eff) -- 2nd argument for EffFn2, the callback function
                D3Element             -- 1st and only argument for EffFn1, d3 element, ie "this", passed thru to callback
                Unit)                 -- result of EffFn1, callback result is just Unit
        (Selection a)               -- result of EffFn2, returns selection for "fluid interface" / monadic chain

foreign import onDoubleClickImpl :: forall eff a. -- (Clickable => c)
 EffFn2 (d3::D3|eff)
        (Selection a)               -- 1st argument for EffFn2, the selection itself
        (EffFnThis1 (d3::D3|eff) -- 2nd argument for EffFn2, the callback function
                D3Element             -- 1st and only argument for EffFn1, d3 element, ie "this", passed thru to callback
                Unit)                 -- result of EffFn1, callback result is just Unit
        (Selection a)               -- result of EffFn2, returns selection for "fluid interface" / monadic chain
