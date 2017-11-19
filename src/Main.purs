module Main where

import Prelude

import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (CONSOLE, log)
import DOM (DOM)
import DOM.Classy.Element (fromElement, toElement)
import DOM.Event.EventTarget (addEventListener, eventListener)
import DOM.Event.Types (Event)
import DOM.HTML.Event.EventTypes (click)
import DOM.HTML (window)
import DOM.HTML.Types (HTMLButtonElement, htmlDocumentToParentNode)
import DOM.HTML.Window (document)
import DOM.Node.ParentNode (QuerySelector(QuerySelector), querySelector)
import DOM.Node.Types (ParentNode, elementToEventTarget)
import Data.Maybe (Maybe, maybe)

-- [ ] chrome devtools bridge to android phone
-- [ ] button click -> prompt for microphone
-- [ ] with microphone access, show record button
-- [ ] record to blob
-- [ ] record length should be constrained / show error if too long
-- [ ] 'play audio' button
-- [ ] 'stop' button
-- [ ] PWA stuff - how can we make this installable / good on android?

-------------------------------------------------------------------------------

data Elements = Elements HTMLButtonElement

-------------------------------------------------------------------------------

x :: forall eff. Event -> Eff (console :: CONSOLE | eff) Unit
x = const (log "click")

queryElements :: forall eff. ParentNode -> Eff (dom :: DOM | eff) (Maybe Elements)
queryElements docNode = do

  btn <- (=<<) fromElement <$> querySelector (QuerySelector "button") docNode

  pure $ Elements <$> btn

bindDOM :: forall eff. Elements -> Eff (dom :: DOM, console :: CONSOLE | eff) Unit
bindDOM (Elements button) = addEventListener click listener true eventTarget
  where eventTarget = (elementToEventTarget <<< toElement) button
        listener    = eventListener x

main :: forall eff. Eff (dom :: DOM, console :: CONSOLE | eff) Unit
main = window
  >>= document
  >>= (queryElements <<< htmlDocumentToParentNode)
  >>= (maybe (log "Could not access elements") bindDOM)
