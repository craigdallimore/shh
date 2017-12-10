module Main where

import Prelude

import Control.Monad.Eff (Eff)
import Control.Monad.Eff.AVar (AVAR)
import Control.Monad.Eff.Exception (EXCEPTION)
import Control.Monad.Eff.Ref (REF)
import Control.Monad.Eff.Console (CONSOLE, log)
import DOM (DOM)
import DOM.Classy.Element (fromElement, toElement)
import DOM.Event.EventTarget (addEventListener, eventListener)
import DOM.Event.Types (Event)
import DOM.HTML (window)
import DOM.HTML.Event.EventTypes (click)
import DOM.HTML.Types (HTMLButtonElement, HTMLDivElement, htmlDocumentToParentNode, htmlDivElementToHTMLElement)
import DOM.HTML.Window (document)
import DOM.Node.ParentNode (QuerySelector(QuerySelector), querySelector)
import DOM.Node.Types (ParentNode, elementToEventTarget)
import Data.Maybe (Maybe(..), maybe)
import Halogen as H
import Halogen.Aff as HA
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Halogen.VDom.Driver (runUI)

-- [ ] Add README, LiCENCE
-- [ ] chrome devtools bridge to android phone
-- [ ] button click -> prompt for microphone
-- [ ] with microphone access, show record button
-- [ ] record to blob
-- [ ] record length should be constrained / show error if too long
-- [ ] 'play audio' button
-- [ ] 'stop' button
-- [ ] PWA stuff - how can we make this installable / good on android?
  -- [ ] web app manifest
  -- [ ] icons

-- UX
-- If we fail to have the required browser features, we should show a "please use another browser"
-- Given the required features are available, we see
-- - a record button
-- - an empty list of past recordings

-- Clicking the record button will
-- - prompt for the ability to use the microphone (if it is not already given)
-- - start recording
-- - change the record button to a "stop" button
-- - present a recording waveform

-- Clicking the stop button should
-- - add a new recording to the list
-- - hide the stop button
-- - show the record button

-- list items should include
-- - the ability to name a recording
-- - change the start and end
-- - remove the recording
-- - present the waveform
-- - play once
-- - play on loop


-------------------------------------------------------------------------------

type State = Boolean

data Query a = Toggle a | IsOn (Boolean -> a)

type Input = Unit

data Message = Toggled Boolean

btnOne :: forall m. H.Component HH.HTML Query Input Message m
btnOne = H.component
  { initialState: const initialState
  , render
  , eval
  , receiver : const Nothing
  }
  where
  initialState :: State
  initialState = false

  render :: State -> H.ComponentHTML Query
  render state =
    let
      label = if state then "On" else "Off"
    in
      HH.button [ HP.title label
                , HE.onClick (HE.input_ Toggle)
                ]
                [ HH.text label ]

  eval :: Query ~> H.ComponentDSL State Query Message m
  eval = case _ of
    Toggle next -> do
      state <- H.get
      let nextState = not state
      H.put nextState
      H.raise $ Toggled nextState
      pure next
    IsOn reply -> do
      state <- H.get
      pure (reply state)

-------------------------------------------------------------------------------

data Elements = Elements HTMLButtonElement HTMLDivElement

-------------------------------------------------------------------------------

x :: forall eff. Event -> Eff (console :: CONSOLE | eff) Unit
x = const (log "click")

queryElements :: forall eff. ParentNode -> Eff (dom :: DOM | eff) (Maybe Elements)
queryElements docNode = do

  recordBtn   <- (=<<) fromElement <$> querySelector (QuerySelector ".btn--record") docNode
  halogenRoot <- (=<<) fromElement <$> querySelector (QuerySelector ".halogen-root") docNode

  pure $ Elements <$> recordBtn <*> halogenRoot

bindDOM
  :: forall eff
   . Elements
  -> Eff
    ( dom       :: DOM
    , console   :: CONSOLE
    , avar      :: AVAR
    , exception :: EXCEPTION
    , ref       :: REF
    | eff
    ) Unit
bindDOM (Elements button root) = do
  addEventListener click listener true eventTarget
  HA.runHalogenAff (runUI btnOne unit (htmlDivElementToHTMLElement root))

  where
    eventTarget = (elementToEventTarget <<< toElement) button
    listener    = eventListener x


main
  :: forall eff
   . Eff
    ( dom       :: DOM
    , console   :: CONSOLE
    , avar      :: AVAR
    , exception :: EXCEPTION
    , ref       :: REF
    | eff
    ) Unit
main = window
  >>= document
  >>= (queryElements <<< htmlDocumentToParentNode)
  >>= maybe (log "Could not access elements") bindDOM

