module Main where

import Prelude

import Components.Root (ui)
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (CONSOLE, log)
import DOM (DOM)
import DOM.Classy.Element (fromElement)
import DOM.HTML (window)
import DOM.HTML.Types (HTMLElement, htmlDocumentToParentNode)
import DOM.HTML.Window (document, navigator)
import DOM.Node.ParentNode (QuerySelector(QuerySelector), querySelector)
import DOM.Node.Types (ParentNode)
import Data.Maybe (Maybe(Just, Nothing))
import Halogen.Aff as HA
import Halogen.VDom.Driver (runUI)
import Media.SupportsGetUserMedia (supportsGetUserMedia)
import Types (Env)

-------------------------------------------------------------------------------

rootSelector :: String
rootSelector = ".halogen-root"

queryElements :: forall eff. ParentNode -> Eff (dom :: DOM | eff) (Maybe HTMLElement)
queryElements docNode = do
  e <- querySelector (QuerySelector rootSelector) docNode
  pure (e >>= fromElement)

bindDOM
  :: HTMLElement
  -> Env
  -> Eff (HA.HalogenEffects (console :: CONSOLE)) Unit
bindDOM root env = HA.runHalogenAff (runUI (ui env) unit root)

main :: Eff (HA.HalogenEffects (console :: CONSOLE )) Unit
main = do
  doc <- window >>= document
  nav <- window >>= navigator
  maybeRootNode <- (queryElements <<< htmlDocumentToParentNode) doc
  case maybeRootNode of
    Nothing       -> log $ "Could not find selector " <> rootSelector
    Just rootNode -> bindDOM rootNode { hasGetUserMedia : supportsGetUserMedia nav }
