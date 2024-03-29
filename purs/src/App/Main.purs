module App.Main (main) where

import Prelude
import Data.Either (either, hush)
import Data.Maybe (Maybe(..), maybe, fromJust)
import Affjax as AX
import Affjax.ResponseFormat as ResponseFormat
import App.Component.SRList as SRList
import App.Config as Config
import Contracts.SuperRare as SuperRare
import Control.Monad.Reader (ask)
import Control.Monad.Trans.Class (lift)
import Data.Argonaut (_String, _Object)
import Data.Lens ((.~), (?~), (^?))
import Data.Lens.Index (ix)
import Effect (Effect)
import Effect.Aff.Class (liftAff)
import Effect.Class (liftEffect)
import Effect.Exception (throw)
import Halogen as H
import Halogen.Aff as HA
import Halogen.VDom.Driver (runUI)
import Network.Ethereum.Web3 (event', eventFilter)
import Network.Ethereum.Web3.Types (BlockNumber(..), ChainCursor(..), Change(..), EventAction(..), _fromBlock, embed, runWeb3, _to, defaultTransactionOptions)
import Network.Ethereum.Web3.Types.Provider (metamaskProvider)
import Partial.Unsafe (unsafeCrashWith, unsafePartial)
import Type.Proxy (Proxy(..))
import Web.DOM.ParentNode (QuerySelector(..))
import Effect.Class.Console as Console

main :: Effect Unit
main =
  HA.runHalogenAff do
    Console.log $ "hello app"
    _ <- HA.awaitBody
    el <- HA.selectElement $ QuerySelector "#app"
    case el of
      Nothing -> unsafeCrashWith "div#app has to be defined"
      Just el' -> do
        io <- runUI SRList.srList unit el'
        metamask <- liftEffect metamaskProvider
        Console.log $ "got metamask"
        void $ runWeb3 metamask
          $ superRareEventMonitor io.query
  where
  superRareEventMonitor query =
    let
      startingBlock = BlockNumber $ embed 5479393

      filters =
        { transfer:
            eventFilter (Proxy :: Proxy SuperRare.Transfer) Config.srAddress
              # _fromBlock
              .~ BN startingBlock
        }

      handlers =
        { transfer: handlerTransfer query
        }
    in
      event' filters handlers { windowSize: 100, trailBy: 0 }

  handlerTransfer query (SuperRare.Transfer t) = do
    let
      tokenURIOpts = defaultTransactionOptions # _to ?~ Config.srAddress
    eIPFSUrl <- lift $ SuperRare.tokenURI tokenURIOpts Latest { _tokenId: t._tokenId }
    url <- liftAff $ either (liftEffect <<< throw <<< show) getImageUrl eIPFSUrl
    Change c <- ask
    let
      newAssetTransfer =
        { to: t._to
        , from: t._from
        , tokenId: t._tokenId
        , transactionHash: c.transactionHash
        , blockNumber: c.blockNumber
        , imageURL: url
        }
    _ <- liftAff <<< query <<< H.mkTell $ SRList.AddAssetTransfer newAssetTransfer
    pure ContinueEvent

  getImageUrl ipfsUrl = do
    resp <- AX.get ResponseFormat.json ipfsUrl
    let
      parsedResp = unsafePartial $ fromJust $ hush resp

      mImageUrl = parsedResp.body ^? _Object <<< ix "image" <<< _String
    maybe (liftEffect $ throw "Couldn't parse ipfs response") pure $ mImageUrl
