module Components.Root where

import Prelude

import Data.Maybe (Maybe(Nothing))
import Control.Monad.Aff (Aff)
import Halogen as H
import Halogen.HTML as HH
import Types (Env)
import Components.Btn as Btn
import Media.GetUserMedia (WEBRTC)

type State = Boolean

data Query a = Passthrough a

type Input = Unit

data Message = Void

data Slot = RecordButtonSlot
derive instance eqRecordbuttonSlot  :: Eq Slot
derive instance ordRecordButtonSlot :: Ord Slot

ui :: forall eff. Env -> H.Component HH.HTML Query Input Void (Aff (webrtc :: WEBRTC | eff))
ui env = H.parentComponent
  { initialState: const initialState
  , render
  , eval
  , receiver : const Nothing
  }
  where
  initialState :: State
  initialState = env.hasGetUserMedia

  render :: State -> H.ParentHTML Query Btn.Query Slot (Aff (webrtc :: WEBRTC | eff))
  render state =
    if state
      then HH.slot RecordButtonSlot Btn.btnRecord unit (const Nothing)
      else HH.div [] [ HH.h2_ [ HH.text "I was not able to find the features necessary for this app to work" ] ]

  eval :: Query ~> H.ParentDSL State Query Btn.Query Slot Void (Aff (webrtc :: WEBRTC | eff))
  eval (Passthrough next) = pure next
