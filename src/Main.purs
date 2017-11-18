module Main where

import Prelude
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (CONSOLE, log)

-- [ ] console.log on a webpage
-- [ ] button click -> console.log
-- [ ] chrome devtools bridge to android phone
-- [ ] button click -> prompt for microphone
-- [ ] with microphone access, show record button
-- [ ] record to blob
-- [ ] record length should be constrained / show error if too long
-- [ ] 'play audio' button
-- [ ] 'stop' button
-- [ ] PWA stuff - how can we make this installable / good on android?

main :: forall e. Eff (console :: CONSOLE | e) Unit
main = do
  log "Hello sailor!"
