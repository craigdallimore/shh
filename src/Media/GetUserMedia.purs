module Media.GetUserMedia where

import Prelude (Unit())
import Control.Monad.Eff (kind Effect, Eff())
import Control.Monad.Eff.Exception(Error())

foreign import data MediaStream :: Type
foreign import data WEBRTC :: Effect

foreign import getUserMedia :: forall eff
  . (MediaStream -> Eff (webrtc :: WEBRTC | eff) Unit)
 -> (Error       -> Eff (webrtc :: WEBRTC | eff) Unit)
                 -> Eff (webrtc :: WEBRTC | eff) Unit
