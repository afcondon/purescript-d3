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
  , attrS
  , attrN
  , attrS'
  , attrN'
  , attrS''
  , attrN''
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

instance attrValNumber :: AttrValue Number
instance attrValString :: AttrValue String


foreign import bindDataImpl       :: forall o n eff.   EffFn2 (d3::D3|eff) (Array n) (Selection o)                       (Update n)
foreign import selectImpl         :: forall d eff.     EffFn2 (d3::D3|eff) String (Selection d)                          (Selection d)
foreign import selectAllImpl      :: forall d eff.     EffFn2 (d3::D3|eff) String (Selection d)                          (Selection Void)
foreign import rootSelectImpl     :: forall eff.       EffFn1 (d3::D3|eff) String                                        (Selection Void)
foreign import rootSelectAllImpl  :: forall eff.       EffFn1 (d3::D3|eff) String                                        (Selection Void)
foreign import unsafeRemoveImpl   :: forall s eff.     EffFn1 (d3::D3|eff) s                                             Unit
foreign import enterImpl          :: forall d eff.     EffFn1 (d3::D3|eff) (Update d)                                    (Enter d)
foreign import exitImpl           :: forall d eff.     EffFn1 (d3::D3|eff) (Update d)                                    (Exit d)
foreign import unsafeAppendImpl   :: forall x s eff.   EffFn2 (d3::D3|eff) String x                                      s

foreign import unsafeStyleImpl    :: forall s eff.     EffFn3 (d3::D3|eff) String String s                               s
foreign import unsafeStyleImplP   :: forall d s eff.   EffFn3 (d3::D3|eff) String (d -> String) s                        s
foreign import unsafeStyleImplPP  :: forall d s eff.   EffFn3 (d3::D3|eff) String (d -> Number -> String) s              s

foreign import unsafeTextImpl     :: forall s eff.     EffFn2 (d3::D3|eff) String s                                      s
foreign import unsafeTextImplP    :: forall d s eff.   EffFn2 (d3::D3|eff) (d -> String) s                               s
foreign import unsafeTextImplPP   :: forall d s eff.   EffFn2 (d3::D3|eff) (d -> Number -> String) s                     s

foreign import transitionImpl     :: forall s d eff.   (Existing s)  => EffFn1 (d3::D3|eff) (s d)                        (Transition d)

foreign import delayImpl          :: forall d eff.     EffFn2 (d3::D3|eff) Number (Transition d)                         (Transition d)
foreign import delayImplP         :: forall d eff.     EffFn2 (d3::D3|eff) (d -> Number) (Transition d)                  (Transition d)
foreign import delayImplPP        :: forall d eff.     EffFn2 (d3::D3|eff) (d -> Number -> Number) (Transition d)        (Transition d)

foreign import durationImpl       :: forall d eff.     EffFn2 (d3::D3|eff) Number (Transition d)                         (Transition d)
foreign import durationImplP      :: forall d eff.     EffFn2 (d3::D3|eff) (d -> Number) (Transition d)                  (Transition d)
foreign import durationImplPP     :: forall d eff.     EffFn2 (d3::D3|eff) (d -> Number -> Number) (Transition d)        (Transition d)

foreign import unsafeAttrImpl     :: forall s v eff.   EffFn3 (d3::D3|eff) String v s                                     s
foreign import unsafeAttrImplP    :: forall d v s eff. EffFn3 (d3::D3|eff) String (d -> v) s                              s
foreign import unsafeAttrImplPP   :: forall d v s eff. EffFn3 (d3::D3|eff) String (d -> Number -> v) s                    s


-- | ===================================================================================
rootSelect    :: forall eff.      String -> Eff (d3::D3|eff) (Selection Void)
rootSelect = runEffFn1 rootSelectImpl

unsafeRemove  :: forall s eff.    s -> Eff (d3::D3|eff) Unit
unsafeRemove = runEffFn1 unsafeRemoveImpl

rootSelectAll :: forall eff.      String -> Eff (d3::D3|eff) (Selection Void)
rootSelectAll = runEffFn1 rootSelectAllImpl

select        :: forall d eff.    String -> Selection d -> Eff (d3::D3|eff) (Selection d)
select = runEffFn2 selectImpl

selectAll     :: forall d eff.    String -> Selection d -> Eff (d3::D3|eff) (Selection Void)
selectAll = runEffFn2 selectAllImpl

enter         :: forall d eff.    Update d -> Eff (d3::D3|eff) (Enter d)
enter = runEffFn1 enterImpl

exit          :: forall d eff.    Update d -> Eff (d3::D3|eff) (Exit d)
exit = runEffFn1 exitImpl

unsafeAppend  :: forall x y eff.  String -> x -> Eff (d3::D3|eff) y
unsafeAppend = runEffFn2 unsafeAppendImpl

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

unsafeAttr    :: forall v s eff.                      String -> v -> s -> Eff (d3::D3|eff) s
unsafeAttr    = runEffFn3 unsafeAttrImpl

unsafeAttr'   :: forall d v s eff.  (AttrValue v) =>  String -> (d -> v) -> s -> Eff (d3::D3|eff) s
unsafeAttr'   = runEffFn3 unsafeAttrImplP

unsafeAttr''  :: forall d v s eff.  (AttrValue v) =>  String -> (d -> Number -> v) -> s -> Eff (d3::D3|eff) s
unsafeAttr''  = runEffFn3 unsafeAttrImplPP


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
  attrS   :: forall d v eff. String -> String ->                   s d -> Eff (d3::D3|eff) (s d)
  attrN   :: forall d v eff. String -> Number ->                   s d -> Eff (d3::D3|eff) (s d)
  attrS'  :: forall d v eff. String -> (d -> String) ->            s d -> Eff (d3::D3|eff) (s d)
  attrN'  :: forall d v eff. String -> (d -> Number) ->            s d -> Eff (d3::D3|eff) (s d)
  attrS'' :: forall d v eff. String -> (d -> Number -> String) ->  s d -> Eff (d3::D3|eff) (s d)
  attrN'' :: forall d v eff. String -> (d -> Number -> Number) ->  s d -> Eff (d3::D3|eff) (s d)
  style   :: forall d eff.   String -> String ->                   s d -> Eff (d3::D3|eff) (s d)
  style'  :: forall d eff.   String -> (d -> String) ->            s d -> Eff (d3::D3|eff) (s d)
  style'' :: forall d eff.   String -> (d -> Number  -> String) -> s d -> Eff (d3::D3|eff) (s d)
  text    :: forall d eff.             String ->                   s d -> Eff (d3::D3|eff) (s d)
  text'   :: forall d eff.             (d -> String) ->            s d -> Eff (d3::D3|eff) (s d)
  text''  :: forall d eff.             (d -> Number -> String) ->  s d -> Eff (d3::D3|eff) (s d)
  remove  :: forall d eff.                                         s d -> Eff (d3::D3|eff) Unit

instance existingSelection :: Existing Selection where
  attrS   = unsafeAttr
  attrN   = unsafeAttr
  attrS'  = unsafeAttr'
  attrN'  = unsafeAttr'
  attrS'' = unsafeAttr''
  attrN'' = unsafeAttr''
  style   = unsafeStyle
  style'  = unsafeStyle'
  style'' = unsafeStyle''
  text    = unsafeText
  text'   = unsafeText'
  text''  = unsafeText''
  remove  = unsafeRemove

instance existingUpdate :: Existing Update where
  attrS   = unsafeAttr
  attrN   = unsafeAttr
  attrS'  = unsafeAttr'
  attrN'  = unsafeAttr'
  attrS'' = unsafeAttr''
  attrN'' = unsafeAttr''
  style   = unsafeStyle
  style'  = unsafeStyle'
  style'' = unsafeStyle''
  text    = unsafeText
  text'   = unsafeText'
  text''  = unsafeText''
  remove  = unsafeRemove

instance existingTransition :: Existing Transition where
  attrS   = unsafeAttr
  attrN   = unsafeAttr
  attrS'  = unsafeAttr'
  attrN'  = unsafeAttr'
  attrS'' = unsafeAttr''
  attrN'' = unsafeAttr''
  style   = unsafeStyle
  style'  = unsafeStyle'
  style'' = unsafeStyle''
  text    = unsafeText
  text'   = unsafeText'
  text''  = unsafeText''
  remove  = unsafeRemove


-- TODO - why are the parameter orders switched here on the Impl - did i do that? or blindly copy? - afc
-- foreign function that will attach a callback to our clickable selection
foreign import onClickImpl :: forall eff a d. -- (Clickable => c)
 EffFn2 (d3::D3|eff)
        (Selection a)             -- 1st argument for EffFn2, the selection itself
        (EffFn1 (d3::D3|eff)  -- 2nd argument for EffFn2, the callback function
                 d                  -- 1st and only argument for EffFn1, the datum given to the callback
                 Unit)              -- result of EffFn1, callback result is just Unit
        (Selection a)             -- result of EffFn2, returns selection so that it can be chained

foreign import onDoubleClickImpl :: forall eff a d. -- (Clickable => c)
 EffFn2 (d3::D3|eff)
        (Selection a)             -- 1st argument for EffFn2, the selection itself
        (EffFn1 (d3::D3|eff)  -- 2nd argument for EffFn2, the callback function
                 d                  -- 1st and only argument for EffFn1, the datum given to the callback
                 Unit)              -- result of EffFn1, callback result is just Unit
        (Selection a)             -- result of EffFn2, returns selection so that it can be chained

class Clickable c where
  onClick :: forall eff. (Foreign -> Eff (d3::D3|eff) Unit) -> c -> Eff (d3::D3|eff) c
  onDoubleClick :: forall eff. (Foreign -> Eff (d3::D3|eff) Unit) -> c -> Eff (d3::D3|eff) c
instance clickableSelectionI :: Clickable (Selection a) where
  onClick callback clickableSelection       = runEffFn2 onClickImpl       clickableSelection (mkEffFn1 callback)
  onDoubleClick callback clickableSelection = runEffFn2 onDoubleClickImpl clickableSelection (mkEffFn1 callback)

foreign import unsafeOnClickImpl :: forall eff c i r. (Clickable c) =>
  EffFn2 (d3::D3|eff)
         (EffFn1 (d3::D3|eff) -- callback, original sig (i -> Eff eff r)
                  i
                  r)
         c
         c
foreign import unsafeOnDoubleClickImpl :: forall eff c i r. (Clickable c) =>
  EffFn2 (d3::D3|eff)
         (EffFn1 (d3::D3|eff) -- callback, original sig (i -> Eff eff r)
                  i
                  r)
         c
         c

unsafeOnClick :: forall eff c i r. (Clickable c) =>
  (i -> Eff (d3::D3|eff) r) -> c -> Eff (d3::D3|eff) c
unsafeOnClick callback clickableSelection = runEffFn2 unsafeOnClickImpl (mkEffFn1 callback) clickableSelection

unsafeOnDoubleClick :: forall eff c i r. (Clickable c) =>
  (i -> Eff (d3::D3|eff) r) -> c -> Eff (d3::D3|eff) c
unsafeOnDoubleClick callback clickableSelection = runEffFn2 unsafeOnDoubleClickImpl (mkEffFn1 callback) clickableSelection
