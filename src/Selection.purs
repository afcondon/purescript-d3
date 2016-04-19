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
  , rootSelect
  , rootSelectAll
  , filter
  , filter'
  , insert
  , select
  , select'
  , selectAll
  , bindData
  , bindDataNK
  , bindDataSK
  , order
  , enter
  , exit
  , transition
  , transition'
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
  , on
  ) where

import Prelude ( Unit() )
import Control.Monad.Eff
import Control.Monad.Eff.Console
import DOM.Event.Types (EventType(..))
import Data.Foreign
import Data.Nullable
import Data.Function.Eff

import Graphics.D3.Base
import Graphics.D3.EffFnExtra

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
foreign import bindDataImplN      :: forall d o n eff. EffFn3 (d3::D3|eff) (Array n) (d -> Number) (Selection o)   (Update n)
foreign import bindDataImplS      :: forall d o n eff. EffFn3 (d3::D3|eff) (Array n) (d -> String) (Selection o)   (Update n)
foreign import selectElementImpl  :: forall d eff.     EffFn1 (d3::D3|eff) D3Element                               (Selection d)
foreign import rootSelectImpl     :: forall eff.       EffFn1 (d3::D3|eff) String                                  (Selection Void)
foreign import rootSelectAllImpl  :: forall eff.       EffFn1 (d3::D3|eff) String                                  (Selection Void)
foreign import unsafeRemoveImpl   :: forall s eff.     EffFn1 (d3::D3|eff) s                                       Unit
foreign import enterImpl          :: forall d eff.     EffFn1 (d3::D3|eff) (Update d)                              (Enter d)
foreign import exitImpl           :: forall d eff.     EffFn1 (d3::D3|eff) (Update d)                              (Exit d)

-- following functions are for typeclass (Appendable)
foreign import unsafeAppendImpl   :: forall x s eff.   EffFn2 (d3::D3|eff) String x                                 s
foreign import unsafeInsertImpl   :: forall x s eff.   EffFn2 (d3::D3|eff) String x                                 s
-- following functions are for typeclass (Existing)
foreign import filterPImpl        :: forall s d eff.   EffFn2 (d3::D3|eff) (d -> Boolean) s                         s
foreign import filterImpl         :: forall s eff.     EffFn2 (d3::D3|eff) String s                                 s
foreign import selectImpl         :: forall s eff.     EffFn2 (d3::D3|eff) String s                                 s
foreign import orderImpl          :: forall s eff.     EffFn1 (d3::D3|eff) s                                        s
foreign import unsafeClassedImpl  :: forall s eff.     EffFn3 (d3::D3|eff) String Boolean s                         s
foreign import selectAllImpl      :: forall s eff.     EffFn2 (d3::D3|eff) String s                                 s
foreign import unsafeStyleImpl    :: forall s v eff.   EffFn3 (d3::D3|eff) String v s                               s
foreign import unsafeStyleImplP   :: forall d s eff.   EffFn3 (d3::D3|eff) String (d -> String) s                   s
foreign import unsafeStyleImplPP  :: forall d s eff.   EffFn3 (d3::D3|eff) String (d -> Number -> String) s         s
foreign import unsafeTextImpl     :: forall s eff.     EffFn2 (d3::D3|eff) String s                                 s
foreign import unsafeTextImplP    :: forall d s eff.   EffFn2 (d3::D3|eff) (d -> String) s                          s
foreign import unsafeTextImplPP   :: forall d s eff.   EffFn2 (d3::D3|eff) (d -> Number -> String) s                s
foreign import unsafeAttrImpl     :: forall s v eff.   EffFn3 (d3::D3|eff) String v s                               s
foreign import transitionImplP    :: forall s d eff.   EffFn2 (d3::D3|eff) String s                                (Transition d)
foreign import transitionImpl     :: forall s d eff.   EffFn1 (d3::D3|eff) s                                       (Transition d)

foreign import delayImpl          :: forall d eff.     EffFn2 (d3::D3|eff) Number (Transition d)                   (Transition d)
foreign import delayImplP         :: forall d eff.     EffFn2 (d3::D3|eff) (d -> Number) (Transition d)            (Transition d)
foreign import delayImplPP        :: forall d eff.     EffFn2 (d3::D3|eff) (d -> Number -> Number) (Transition d)  (Transition d)
foreign import durationImpl       :: forall d eff.     EffFn2 (d3::D3|eff) Number (Transition d)                   (Transition d)
foreign import durationImplP      :: forall d eff.     EffFn2 (d3::D3|eff) (d -> Number) (Transition d)            (Transition d)
foreign import durationImplPP     :: forall d eff.     EffFn2 (d3::D3|eff) (d -> Number -> Number) (Transition d)  (Transition d)


-- | ===================================================================================
rootSelect :: forall eff.      String -> Eff (d3::D3|eff) (Selection Void)
rootSelect = runEffFn1 rootSelectImpl

unsafeRemove  :: forall s eff.    s -> Eff (d3::D3|eff) Unit
unsafeRemove  = runEffFn1 unsafeRemoveImpl

rootSelectAll :: forall eff.      String -> Eff (d3::D3|eff) (Selection Void)
rootSelectAll = runEffFn1 rootSelectAllImpl

select'       :: forall d eff.    D3Element -> Eff (d3::D3|eff) (Selection d)
select'       = runEffFn1 selectElementImpl

enter         :: forall d eff.    Update d -> Eff (d3::D3|eff) (Enter d)
enter         = runEffFn1 enterImpl

exit          :: forall d eff.    Update d -> Eff (d3::D3|eff) (Exit d)
exit          = runEffFn1 exitImpl

unsafeSelect  :: forall s eff.  String -> s -> Eff (d3::D3|eff) s
unsafeSelect  = runEffFn2 selectImpl

unsafeFilter  :: forall s eff.  String -> s -> Eff (d3::D3|eff) s
unsafeFilter  = runEffFn2 filterImpl

unsafeFilter'  :: forall s d eff.  (d -> Boolean) -> s -> Eff (d3::D3|eff) s
unsafeFilter'  = runEffFn2 filterPImpl

unsafeOrder   :: forall s eff.   s -> Eff (d3::D3|eff) s
unsafeOrder    = runEffFn1 orderImpl

unsafeInsert  :: forall x y eff.  String -> x -> Eff (d3::D3|eff) y
unsafeInsert  = runEffFn2 unsafeInsertImpl

unsafeAppend  :: forall x y eff.  String -> x -> Eff (d3::D3|eff) y
unsafeAppend  = runEffFn2 unsafeAppendImpl

unsafeClassed :: forall s eff. String -> Boolean -> s -> Eff (d3::D3|eff) s
unsafeClassed = runEffFn3 unsafeClassedImpl

unsafeSelectAll :: forall s eff. String -> s -> Eff (d3::D3|eff) s
unsafeSelectAll = runEffFn2 selectAllImpl

unsafeStyle   :: forall v s eff.  (AttrValue v) =>  String -> v -> s -> Eff (d3::D3|eff) s
unsafeStyle   = runEffFn3 unsafeStyleImpl

unsafeStyle'  :: forall d s eff.  String -> (d -> String) -> s -> Eff (d3::D3|eff) s
unsafeStyle'  = runEffFn3 unsafeStyleImplP

unsafeStyle'' :: forall d s eff.  String -> (d -> Number -> String) -> s -> Eff (d3::D3|eff) s
unsafeStyle'' = runEffFn3 unsafeStyleImplPP

unsafeText    :: forall s eff.  String -> s -> Eff (d3::D3|eff) s
unsafeText    = runEffFn2 unsafeTextImpl

unsafeText'   :: forall d s eff.  (d -> String) -> s -> Eff (d3::D3|eff) s
unsafeText'   = runEffFn2 unsafeTextImplP

unsafeText''  :: forall d s eff.  (d -> Number -> String) -> s -> Eff (d3::D3|eff) s
unsafeText''  = runEffFn2 unsafeTextImplPP

delay         :: forall d eff.    Number -> Transition d -> Eff (d3::D3|eff) (Transition d)
delay         = runEffFn2 delayImpl

delay'        :: forall d eff.    (d -> Number) -> Transition d -> Eff (d3::D3|eff) (Transition d)
delay'        = runEffFn2 delayImplP

delay''       :: forall d eff.    (d -> Number -> Number) -> Transition d -> Eff (d3::D3|eff) (Transition d)
delay''       = runEffFn2 delayImplPP

duration      :: forall d eff.    Number -> Transition d -> Eff (d3::D3|eff) (Transition d)
duration      = runEffFn2 durationImpl

duration'     :: forall d eff.    (d -> Number) -> Transition d -> Eff (d3::D3|eff) (Transition d)
duration'     = runEffFn2 durationImplP

duration''    :: forall d eff.    (d -> Number -> Number) -> Transition d -> Eff (d3::D3|eff) (Transition d)
duration''    = runEffFn2 durationImplPP

bindData      :: forall od nd eff. Array nd -> Selection od -> Eff (d3::D3|eff) (Update nd)
bindData      = runEffFn2 bindDataImpl

bindDataNK     :: forall d od nd eff. Array nd -> (d -> Number) -> Selection od -> Eff (d3::D3|eff) (Update nd)
bindDataNK     = runEffFn3 bindDataImplN

bindDataSK     :: forall d od nd eff. Array nd -> (d -> String) -> Selection od -> Eff (d3::D3|eff) (Update nd)
bindDataSK     = runEffFn3 bindDataImplS

unsafeTransition    :: forall s d eff.    s -> Eff (d3::D3|eff) (Transition d)
unsafeTransition    = runEffFn1 transitionImpl

unsafeTransitionN    :: forall s d eff.    String -> s -> Eff (d3::D3|eff) (Transition d)
unsafeTransitionN    = runEffFn2 transitionImplP

unsafeAttr    :: forall v s eff.    (AttrValue v) =>  String -> v -> s -> Eff (d3::D3|eff) s
unsafeAttr    = runEffFn3 unsafeAttrImpl

unsafeAttr'   :: forall d v s eff.    (AttrValue v) =>  String -> (d -> v) -> s -> Eff (d3::D3|eff) s
unsafeAttr'   = runEffFn3 unsafeAttrImpl

unsafeAttr''   :: forall d v s eff.    (AttrValue v) =>  String -> (d -> Number -> v) -> s -> Eff (d3::D3|eff) s
unsafeAttr''   = runEffFn3 unsafeAttrImpl

-- Selection-y things which can be appended to / inserted into
class Appendable s where
  append :: forall d eff. String -> s d -> Eff (d3::D3|eff) (Selection d)
  insert :: forall d eff. String -> s d -> Eff (d3::D3|eff) (Selection d)

instance appendableSelection  :: Appendable Selection where
  append = unsafeAppend
  insert = unsafeInsert

instance appendableUpdate     :: Appendable Update where
  append = unsafeAppend
  insert = unsafeInsert

instance appendableEnter      :: Appendable Enter where
  append = unsafeAppend
  insert = unsafeInsert

-- Selection-y things that contain existing DOM elements
class Existing s where
  attr   :: forall d v eff. (AttrValue v) => String -> v ->                        s d -> Eff (d3::D3|eff) (s d)
  attr'  :: forall d v eff. (AttrValue v) => String -> (d -> v) ->                 s d -> Eff (d3::D3|eff) (s d)
  attr'' :: forall d v eff. (AttrValue v) => String -> (d -> Number -> Number) ->  s d -> Eff (d3::D3|eff) (s d)
  classed   :: forall d eff.   String -> Boolean ->                   s d -> Eff (d3::D3|eff) (s d)
  selectAll :: forall d eff.   String ->                              s d -> Eff (d3::D3|eff) (s d)
  style     :: forall d v eff. (AttrValue v) =>  String -> v ->       s d -> Eff (d3::D3|eff) (s d)
  style'    :: forall d eff.   String -> (d -> String) ->             s d -> Eff (d3::D3|eff) (s d)
  style''   :: forall d eff.   String -> (d -> Number  -> String) ->  s d -> Eff (d3::D3|eff) (s d)
  text      :: forall d eff.             String ->                    s d -> Eff (d3::D3|eff) (s d)
  text'     :: forall d eff.             (d -> String) ->             s d -> Eff (d3::D3|eff) (s d)
  text''    :: forall d eff.             (d -> Number -> String) ->   s d -> Eff (d3::D3|eff) (s d)
  remove    :: forall d eff.                                          s d -> Eff (d3::D3|eff) Unit
  select    :: forall d eff.  String ->                               s d -> Eff (d3::D3|eff) (s d)
  -- insert    :: forall d eff.  String ->                               s d -> Eff (d3::D3|eff) (s d)
  filter    :: forall d eff.  String ->                               s d -> Eff (d3::D3|eff) (s d)
  filter'   :: forall d eff.  (d -> Boolean) ->                       s d -> Eff (d3::D3|eff) (s d)
  order     :: forall d eff.                                          s d -> Eff (d3::D3|eff) (s d)
  transition' :: forall d eff. String ->                              s d -> Eff (d3::D3|eff) (Transition d)
  transition :: forall d eff.                                         s d -> Eff (d3::D3|eff) (Transition d)

instance existingSelection :: Existing Selection where
  attr      = unsafeAttr
  attr'     = unsafeAttr'
  attr''    = unsafeAttr''
  classed   = unsafeClassed
  selectAll = unsafeSelectAll
  style     = unsafeStyle
  style'    = unsafeStyle'
  style''   = unsafeStyle''
  text      = unsafeText
  text'     = unsafeText'
  text''    = unsafeText''
  remove    = unsafeRemove
  select    = unsafeSelect
  -- insert    = unsafeInsert
  filter    = unsafeFilter
  filter'   = unsafeFilter'
  order     = unsafeOrder
  transition = unsafeTransition
  transition' = unsafeTransitionN

instance existingUpdate :: Existing Update where
  attr      = unsafeAttr
  attr'     = unsafeAttr'
  attr''    = unsafeAttr''
  classed   = unsafeClassed
  selectAll = unsafeSelectAll
  style     = unsafeStyle
  style'    = unsafeStyle'
  style''   = unsafeStyle''
  text      = unsafeText
  text'     = unsafeText'
  text''    = unsafeText''
  remove    = unsafeRemove
  select    = unsafeSelect
  -- insert    = unsafeInsert
  filter    = unsafeFilter
  filter'   = unsafeFilter'
  order     = unsafeOrder
  transition = unsafeTransition
  transition' = unsafeTransitionN

instance existingTransition :: Existing Transition where
  attr      = unsafeAttr
  attr'     = unsafeAttr'
  attr''    = unsafeAttr''
  classed   = unsafeClassed
  selectAll = unsafeSelectAll
  style     = unsafeStyle
  style'    = unsafeStyle'
  style''   = unsafeStyle''
  text      = unsafeText
  text'     = unsafeText'
  text''    = unsafeText''
  remove    = unsafeRemove
  select    = unsafeSelect
  -- insert    = unsafeInsert
  filter    = unsafeFilter
  filter'   = unsafeFilter'
  order     = unsafeOrder
  transition = unsafeTransition
  transition' = unsafeTransitionN


-- So, say you're _setting_ a click handler on a _Selection_ but it gets _called_ with the HTML element that
-- received the click event
-- Now, you can get the selection in JS by just doing d3.select(this) but because of our wrapper EffFn1
-- around the callback there is no "this" in the PureScript callback
-- So instead, i'm using a custom _mkEffFnTuple1_ which passes on the "this" INSTEAD of the datum
    -- perhaps it will pass a Tuple of the two of them in the future
    -- perhaps this can be formalized / librarized / templatized if it works

-- generic "on" function replaces single and double click functions and works for any DOM event
on :: forall a d eff. EventType
                -> (ElementAndDatum d -> Eff (d3::D3|eff) Unit)
                -> (Selection a)
                -> Eff (d3::D3|eff) (Selection a)
on event callback selection  = runEffFn3 onImpl selection event (mkEffFnTuple1 callback)

foreign import onImpl :: forall eff a d.
  EffFn3 (d3::D3|eff)
        (Selection a)               -- 1st argument for EffFn3, the selection itself
        EventType                   -- 2nd argument for EffFn3, the type of the event being bound
        (EffFnTuple1 (d3::D3|eff)   -- 3rd argument for EffFn3, the callback function
            (ElementAndDatum d)       -- arg for callback EffFn1, a d3 element
            Unit)                     -- result of EffFn1, ie only Unit
        (Selection a)               -- result of EffFn2, returns selection for "fluid interface" / monadic chain
