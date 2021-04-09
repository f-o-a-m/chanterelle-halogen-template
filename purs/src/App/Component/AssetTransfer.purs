module App.Component.AssetTransfer where

import Prelude
import App.Component.Image as Image
import App.Model (AssetTransfer, initialImage)
import Data.Maybe (Maybe(..))
import Data.String as Str
import Data.Symbol (SProxy(..))
import Effect.Aff.Class (class MonadAff)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Network.Ethereum.Web3.Types (Address, HexString)

{-
  This component is for the AssetCard, which is comprised of the image and
  the asset metadata with links to etherscan.
-}
data Query a
  = Next a

data Action
  = SelectUserAddress Address

type Slots
  = ( "tokenImage" :: H.Slot Image.Query Image.Message Unit )

type Input
  = Unit

type Message
  = Void

_tokenImage :: SProxy "tokenImage"
_tokenImage = SProxy

assetTransfer ::
  forall m.
  MonadAff m =>
  AssetTransfer ->
  H.Component HH.HTML Query Image.Input Message m
assetTransfer initialState =
  H.mkComponent
    { initialState: const initialState
    , render
    , eval
    }
  where
  render :: AssetTransfer -> H.ComponentHTML Action Slots m
  render at =
    HH.li [ HP.class_ (HH.ClassName "sr-tile") ]
      [ HH.div [ HP.class_ (HH.ClassName "sr-pic") ]
          [ renderImage at.imageURL ]
      , HH.div [ HP.class_ (HH.ClassName "sr-info") ]
          [ HH.div [ HP.class_ (HH.ClassName "sr-info-headings") ]
              [ HH.h6_ [ HH.text "to: " ]
              , HH.h6_ [ HH.text "from: " ]
              , HH.h6_ [ HH.text "tokenId: " ]
              , HH.h6_ [ HH.text "transactionHash: " ]
              , HH.h6_ [ HH.text "blockNumber: " ]
              ]
          , HH.div [ HP.class_ (HH.ClassName "sr-info-details") ]
              [ HH.h5
                  [ HP.class_ (HH.ClassName "user-address-link")
                  , HE.onClick $ const $ Just (SelectUserAddress at.to)
                  ]
                  [ addressLink at.to ]
              , HH.h5
                  [ HP.class_ (HH.ClassName "user-address-link")
                  , HE.onClick $ const $ Just (SelectUserAddress at.from)
                  ]
                  [ addressLink at.from ]
              , HH.h5_ [ HH.text $ show at.tokenId ]
              , HH.h5_ [ txLink at.transactionHash ]
              , HH.h5_ [ HH.text $ show $ at.blockNumber ]
              ]
          ]
      ]

  renderImage ::
    String ->
    HH.ComponentHTML Action Slots m
  renderImage url =
    HH.slot
      _tokenImage
      unit
      (Image.imageComponent $ initialImage url)
      unit -- ?
      absurd -- ?

  eval ::
    H.HalogenQ Query Action Input
      ~> H.HalogenM AssetTransfer Action Slots Message m
  eval =
    H.mkEval
      H.defaultEval
        { handleQuery = handleQuery
        }

  handleQuery :: forall a. Query a -> H.HalogenM AssetTransfer Action Slots Message m (Maybe a)
  handleQuery (Next next) = pure $ Just next

  handleAction :: Action -> H.HalogenM AssetTransfer Action Slots Message m Unit
  handleAction (SelectUserAddress a) = pure unit

  addressLink :: forall w i. Address -> HH.HTML w i
  addressLink address =
    HH.a [ HP.href $ "https://etherscan.io/address/" <> show address, HP.target "_blank" ]
      [ HH.text $ show address ]

  txLink :: forall w i. HexString -> HH.HTML w i
  txLink txHash =
    HH.a [ HP.href $ "https://etherscan.io/tx/" <> show txHash, HP.target "_blank" ]
      [ HH.text $ shortenLink $ show txHash ]

  shortenLink ::
    String ->
    String
  shortenLink str
    | Str.length str < 20 = str
    | otherwise = short
      where
      short = Str.take 7 str <> "..." <> Str.drop (Str.length str - 5) str
