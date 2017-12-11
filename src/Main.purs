module Main where

import Prelude

import Control.Monad.Eff (Eff)
import Control.Monad.Eff.AVar (AVAR)
import Control.Monad.Eff.Console (CONSOLE, log)
import Control.Monad.Eff.Exception (EXCEPTION)
import Control.Monad.Eff.Ref (REF)
import DOM (DOM)
import DOM.Classy.Element (fromElement)
import DOM.HTML (window)
import DOM.HTML.Types (HTMLElement, htmlDocumentToParentNode)
import DOM.HTML.Window (document)
import DOM.Node.ParentNode (QuerySelector(QuerySelector), querySelector)
import DOM.Node.Types (ParentNode)
import Data.Maybe (Maybe, maybe)
import Halogen.Aff as HA
import Halogen.VDom.Driver (runUI)
import Components.Root (btnOne)

-------------------------------------------------------------------------------

rootSelector :: String
rootSelector = ".halogen-root"

queryElements :: forall eff. ParentNode -> Eff (dom :: DOM | eff) (Maybe HTMLElement)
queryElements docNode = do
  e <- querySelector (QuerySelector rootSelector) docNode
  pure (e >>= fromElement)

bindDOM
  :: forall eff
   . HTMLElement
  -> Eff
    ( dom       :: DOM
    , console   :: CONSOLE
    , avar      :: AVAR
    , exception :: EXCEPTION
    , ref       :: REF
    | eff
    ) Unit
bindDOM root = HA.runHalogenAff (runUI btnOne unit root)

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
  >>= maybe (log ("Could not find selector " <> rootSelector)) bindDOM

