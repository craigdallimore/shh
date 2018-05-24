module Media.GetUserMedia where

import Control.Monad.Aff (Aff)
import Control.Monad.Aff.Compat (EffFnAff(), fromEffFnAff)
import Control.Monad.Eff (kind Effect)

foreign import data MediaStream :: Type
foreign import data WEBRTC :: Effect

foreign import _getUserMedia :: forall eff. EffFnAff (webrtc :: WEBRTC | eff) MediaStream

getUserMedia :: forall eff. Aff (webrtc :: WEBRTC | eff) MediaStream
getUserMedia = fromEffFnAff _getUserMedia
