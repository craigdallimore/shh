module Components.Btn where

import Prelude

import Control.Monad.Aff (Aff)
import Control.Monad.Error.Class (catchError)
import Control.Monad.Eff.Exception(Error())
import Data.Maybe (Maybe(Nothing, Just), maybe)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Media.GetUserMedia (MediaStream, WEBRTC, getUserMedia)

type State = { isRecording :: Boolean
             , stream      :: Maybe MediaStream
             , error       :: Maybe Error
             }

data Query a = Toggle a

type Input = Unit

data Message = Toggled Boolean

-- Think it through, what are the states?

-- Initial
-- label: Record
-- error: absent
-- stream: absent

-- Success
-- label: Recording
-- error: absent
-- stream: present

-- Failure
-- label: Record
-- error: present
-- stream: absent

btnRecord :: forall eff. H.Component HH.HTML Query Input Message (Aff (webrtc :: WEBRTC | eff))
btnRecord = H.component
  { initialState: const initialState
  , render
  , eval
  , receiver : const Nothing
  }
  where
  initialState :: State
  initialState = { isRecording : false
                 , stream      : Nothing
                 , error       : Nothing
                 }

  render :: State -> H.ComponentHTML Query
  render state =
    let label = if state.isRecording then "Stop" else "Record"
    in
      HH.div_ [ HH.button
                [ HP.title label , HE.onClick (HE.input_ Toggle) ]
                [ HH.text label ]
              , HH.p_
                [ HH.text (maybe "" show state.error) ]
              ]

  eval :: Query ~> H.ComponentDSL State Query Message (Aff (webrtc :: WEBRTC | eff))
  eval = case _ of
    Toggle next -> do
      state <- H.get
      state' <- H.liftAff (getUserAudio state)

      let nextState = state' { isRecording = not state.isRecording }
      H.put nextState
      H.raise $ Toggled nextState.isRecording
      pure next


-- Rather than be Eff e Unit, this wants to be Eff e state
-- where the state contains a possible reference to the mediastream


getUserAudio :: forall eff. State -> Aff (webrtc :: WEBRTC | eff) State
getUserAudio state = (getUserMedia >>= \ms -> pure (state { stream = Just ms }))
                     `catchError`     \e  -> pure (state { error  = Just e })


