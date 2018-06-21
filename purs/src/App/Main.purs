module App.Main where

import Prelude

import App.Component.List (ListQuery(..), list)
import App.Config as Config
import Contracts.KittyCore as KC
import Control.Monad.Aff (liftEff')
import Control.Monad.Aff.AVar (AVAR)
import Control.Monad.Aff.Class (liftAff)
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Class (liftEff)
import Control.Monad.Eff.Exception (EXCEPTION, throw)
import Control.Monad.Eff.Ref (REF)
import Control.Monad.Reader (ask)
import DOM (DOM)
import DOM.Node.ParentNode (QuerySelector(..))
import Data.Either (either)
import Data.Lens ((.~))
import Data.Maybe (Maybe(..))
import Halogen as H
import Halogen.Aff as HA
import Halogen.VDom.Driver (runUI)
import Network.Ethereum.Web3.Api (eth_blockNumber)
import Network.Ethereum.Web3.Contract (event, eventFilter)
import Network.Ethereum.Web3.Types (BlockNumber(..), ChainCursor(..), Change(..), ETH, EventAction(..), _fromBlock, embed, runWeb3)
import Network.Ethereum.Web3.Types.Provider (metamaskProvider)
import Partial.Unsafe (unsafeCrashWith)
import Type.Proxy (Proxy(..))

main
  :: forall eff.
     Eff ( avar :: AVAR
         , ref :: REF
         , exception :: EXCEPTION
         , dom :: DOM
         , eth :: ETH
         | eff
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
        etokAddress <- Config.tokenContract
        tokAddress <- liftEff $ either (throw <<< show) pure etokAddress
        BlockNumber bn <- eth_blockNumber
        let startingBlock = BlockNumber (bn - embed 500)
            fltr = eventFilter (Proxy :: Proxy KC.Transfer) tokAddress # _fromBlock .~ BN startingBlock
        event fltr $ \(KC.Transfer t) -> do
          (Change c) <- ask
          let newAssetTransfer =
                { to: t.to
                , from: t.from
                , tokenId: t.tokenId
                , transactionHash: c.transactionHash
                , blockNumber: c.blockNumber
                }
          liftAff $ io.query $ H.action (AddAssetTransfer newAssetTransfer)
          pure ContinueEvent
