module App.Main where

import Prelude

import App.Component.List (ListQuery(..), list)
import App.Config as Config
import Contracts.SuperRare as SuperRare
import Control.Monad.Aff (liftEff')
import Control.Monad.Aff.AVar (AVAR)
import Control.Monad.Aff.Class (liftAff)
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Exception (EXCEPTION, throw)
import Control.Monad.Eff.Ref (REF)
import Control.Monad.Reader (ask)
import DOM (DOM)
import DOM.Node.ParentNode (QuerySelector(..))
import Data.Either (either)
import Data.Lens ((.~), (?~), (^?))
import Data.Maybe (Maybe(..), maybe)
import Halogen as H
import Halogen.Aff as HA
import Halogen.VDom.Driver (runUI)
import Network.Ethereum.Web3.Api (eth_blockNumber)
import Network.Ethereum.Web3.Contract (event', eventFilter)
import Network.Ethereum.Web3.Types (BlockNumber(..), ChainCursor(..), Change(..), ETH, EventAction(..), _fromBlock, embed, runWeb3, _to, defaultTransactionOptions)
import Network.Ethereum.Web3.Types.Provider (metamaskProvider)
import Partial.Unsafe (unsafeCrashWith)
import Type.Proxy (Proxy(..))
import Control.Monad.Trans.Class (lift)
import Debug.Trace as Trace
import Network.HTTP.Affjax (AJAX, get)
import Data.Argonaut (Json, _String, _Object)
import Data.Lens.Index (ix)



main
  :: forall eff.
     Eff ( avar :: AVAR
         , ref :: REF
         , exception :: EXCEPTION
         , dom :: DOM
         , eth :: ETH
         , ajax :: AJAX
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
          BlockNumber bn <- eth_blockNumber
          let startingBlock = BlockNumber (embed 5777352)
              fltr = eventFilter (Proxy :: Proxy SuperRare.SalePriceSet) Config.srAddress # _fromBlock .~ BN startingBlock
          event' fltr 100 $ \(SuperRare.SalePriceSet sps) -> do
            (Change c) <- ask
            eIPFSUrl <- lift $ SuperRare.tokenURI (defaultTransactionOptions # _to ?~ Config.srAddress) Latest {_tokenId: sps._tokenId}
            url <- liftAff $ either (liftEff' <<< throw <<< show) getImageUrl eIPFSUrl
            Trace.traceA url
            let newAssetTransfer =
                  { to: c.address
                  , from: c.address
                  , tokenId: sps._tokenId
                  , transactionHash: c.transactionHash
                  , blockNumber: c.blockNumber
                  , imageURL: url
                  }
            liftAff $ io.query $ H.action (AddAssetTransfer newAssetTransfer)
            pure ContinueEvent
  where
    getImageUrl ipfsUrl = do
      (ipfsResp :: Json) <-  _.response <$> get ipfsUrl
      let mImageUrl = ipfsResp  ^? _Object <<< ix "image" <<< _String
      maybe (liftEff' $ throw "Couldn't parse ipfs response") pure $ mImageUrl
