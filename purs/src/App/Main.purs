module App.Main (main) where

import Prelude

import App.Component.SRList (SRListQuery(..), srList)
import App.Config as Config
import Contracts.SuperRare as SuperRare
import Control.Monad.Aff (Aff, liftEff')
import Control.Monad.Aff.Class (liftAff)
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Exception (throw)
import Control.Monad.Reader (ask)
import Control.Monad.Trans.Class (lift)
import DOM.Node.ParentNode (QuerySelector(..))
import Data.Argonaut (Json, _String, _Object)
import Data.Either (either)
import Data.Lens ((.~), (?~), (^?))
import Data.Lens.Index (ix)
import Data.Maybe (Maybe(..), maybe)
import Halogen as H
import Halogen.Aff as HA
import Halogen.Aff.Effects (HalogenEffects)
import Halogen.VDom.Driver (runUI)
import Network.Ethereum.Web3.Contract (event', eventFilter)
import Network.Ethereum.Web3.Types (BlockNumber(..), ChainCursor(..), Change(..), ETH, EventAction(..), Web3, _fromBlock, embed, runWeb3, _to, defaultTransactionOptions)
import Network.Ethereum.Web3.Types.Provider (metamaskProvider)
import Network.HTTP.Affjax (AJAX, get)
import Partial.Unsafe (unsafeCrashWith)
import Type.Proxy (Proxy(..))

main
  :: forall eff.
     Eff (HalogenEffects (ajax :: AJAX, eth :: ETH | eff)) Unit
main = HA.runHalogenAff do
    body <- HA.awaitBody
    el <- HA.selectElement $ QuerySelector "#app"
    case el of
      Nothing -> unsafeCrashWith "div#app has to be defined"
      Just el' -> do
        io <- runUI srList unit el'
        metamask <- liftEff' metamaskProvider
        void $ runWeb3 metamask $
          superRareEventMonitor io.query

superRareEventMonitor
  :: forall eff.
     SRListQuery ~> Aff (HalogenEffects (ajax :: AJAX, eth :: ETH | eff))
  -> Web3 (HalogenEffects (ajax :: AJAX| eff))  Unit
superRareEventMonitor query =
  let
    startingBlock = BlockNumber $ embed 5479393
    fltr = eventFilter (Proxy :: Proxy SuperRare.Transfer) Config.srAddress
                # _fromBlock .~ BN startingBlock
  in
    event' fltr 100 \(SuperRare.Transfer t) -> do
      let tokenURIOpts = defaultTransactionOptions # _to ?~ Config.srAddress
      eIPFSUrl <- lift $ SuperRare.tokenURI tokenURIOpts Latest {_tokenId: t._tokenId}
      url <- liftAff $ either (liftEff' <<< throw <<< show) getImageUrl eIPFSUrl
      Change c <- ask
      let newAssetTransfer =
            { to: t._to
            , from: t._from
            , tokenId: t._tokenId
            , transactionHash: c.transactionHash
            , blockNumber: c.blockNumber
            , imageURL: url
            }
      _ <- liftAff <<< query <<< H.action $ AddAssetTransfer newAssetTransfer
      pure ContinueEvent
  where
    getImageUrl ipfsUrl = do
      (ipfsResp :: Json) <-  _.response <$> get ipfsUrl
      let mImageUrl = ipfsResp  ^? _Object <<< ix "image" <<< _String
      maybe (liftEff' $ throw "Couldn't parse ipfs response") pure $ mImageUrl
