module App.Main where

import Prelude

import App.Component.List (list)
import Control.Monad.Aff (liftEff')
import Control.Monad.Aff.AVar (AVAR)
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Exception (EXCEPTION)
import Control.Monad.Eff.Ref (REF)
import DOM (DOM)
import DOM.Node.ParentNode (QuerySelector(..))
import Data.Maybe (Maybe(..))
import Halogen.Aff as HA
import Halogen.VDom.Driver (runUI)
import Network.Ethereum.Web3.Api (eth_blockNumber)
import Network.Ethereum.Web3.Types (BlockNumber(..), ETH, embed, runWeb3)
import Network.Ethereum.Web3.Types.Provider (metamaskProvider)
import Partial.Unsafe (unsafeCrashWith)

main
  :: forall eff.
     Eff ( avar :: AVAR
         , ref :: REF
         , exception :: EXCEPTION
         , dom :: DOM
         , eth :: ETH
         ) Unit
main = HA.runHalogenAff do
  body <- HA.awaitBody
  el <- HA.selectElement $ QuerySelector "#app"
  case el of
    Nothing -> unsafeCrashWith "div#app has to be defined"
    Just el' -> do
      io <- runUI list unit el'
      metamask <- liftEff' metamaskProvider
      void $ runWeb3 metamask $ do
        BlockNumber bn <- eth_blockNumber
        let startingBlock = BlockNumber (bn - embed 500)
            filter = eventFilter 
        pure unit
