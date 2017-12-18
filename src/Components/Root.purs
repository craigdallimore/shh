module Components.Root where

import Prelude

import Data.Maybe (Maybe(Nothing))
import Halogen as H
import Halogen.HTML as HH
import Types (Env)

type State = Boolean

data Query a = Passthrough a

type Input = Unit

data Message = Void

ui :: forall m. Env -> H.Component HH.HTML Query Input Message m
ui env = H.component
  { initialState: const initialState
  , render
  , eval
  , receiver : const Nothing
  }
  where
  initialState :: State
  initialState = env.hasGetUserMedia

  render :: State -> H.ComponentHTML Query
  render state =
    if state
      then HH.div [] [ HH.h2_ [ HH.text "Supports getUserMedia" ] ]
      else HH.div [] [ HH.h2_ [ HH.text "I was not able to find the features necessary for this app to work" ] ]

  eval :: Query ~> H.ComponentDSL State Query Message m
  eval (Passthrough next) = do
     s <- H.get
     H.put s
     pure next
