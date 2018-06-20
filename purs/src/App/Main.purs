module App.Main where

import Prelude

import Control.Monad.Aff.AVar (AVAR)
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Exception (EXCEPTION)
import Control.Monad.Eff.Ref (REF)
import Data.Maybe (Maybe(..))
import DOM (DOM)
import DOM.Node.ParentNode (QuerySelector(..))
import Halogen.Aff as HA
import Halogen.VDom.Driver (runUI)
import Partial.Unsafe (unsafeCrashWith)
import App.Component.List (list)

main
  :: forall eff.
     Eff ( avar :: AVAR
         , ref :: REF
         , exception :: EXCEPTION
         , dom :: DOM
         ) Unit
main = HA.runHalogenAff do
  body <- HA.awaitBody
  el <- HA.selectElement $ QuerySelector "#app"
  case el of
    Nothing -> unsafeCrashWith "div#app has to be defined"
    Just el' -> runUI list unit el'
