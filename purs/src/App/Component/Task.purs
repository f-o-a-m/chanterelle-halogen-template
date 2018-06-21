module App.Component.AssetTransfer where

import Prelude

import App.Model (AssetTransfer)
import Data.Maybe (Maybe(..))
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Network.Ethereum.Web3.Types (Address)
import Data.String as Str

-- | The task component query algebra.
data AssetTransferQuery a
  = AssetTransfered AssetTransfer a
  | SelectUserAddress Address a

-- | The task component definition.
assetTransfer :: forall m. AssetTransfer -> H.Component HH.HTML AssetTransferQuery Unit Void m
assetTransfer initialState =
  H.component
    { initialState: const initialState
    , render
    , eval
    , receiver: const Nothing
    }
  where

  render :: AssetTransfer -> H.ComponentHTML AssetTransferQuery
  render at =
    HH.li [HP.class_ (HH.ClassName "kitty-info")]
      [ HH.div [HP.class_ (HH.ClassName "kitty-info-headings")]
        [ HH.h6_ [HH.text "to: "]
        , HH.h6_ [HH.text "from: "]
        , HH.h6_ [HH.text "tokenId: "]
        , HH.h6_ [HH.text "transactionHash: "]
        , HH.h6_ [HH.text "blockNumber: "]
        ]
      , HH.div [HP.class_ (HH.ClassName "kitty-info-details")]
        [ HH.h5 [ HP.class_ (HH.ClassName "user-address-link")
                , HE.onClick (HE.input_ $ SelectUserAddress at.to)
                ] [addressLink at.to]
        , HH.h5 [ HP.class_ (HH.ClassName "user-address-link")
                , HE.onClick (HE.input_ $ SelectUserAddress at.from)
                ] [addressLink at.from]
        , HH.h5_ [HH.text $ show at.tokenId]
        , HH.h5_ [txLink at.transactionHash]
        , HH.h5_ [HH.text $ show $ at.blockNumber]
        ]
      ]

  eval :: AssetTransferQuery ~> H.ComponentDSL AssetTransfer AssetTransferQuery Void m
  eval (AssetTransfered _ next) = pure next
  eval (SelectUserAddress _ next) = pure next

  addressLink address =
    HH.a [ HP.href $ "https://etherscan.io/address/" <> show address, HP.target "_blank" ]
         [ HH.text $ show address ]

  txLink txHash =
    HH.a [ HP.href $ "https://etherscan.io/tx/" <> show txHash, HP.target "_blank" ]
         [ HH.text $ shortenLink $ show txHash ]

  shortenLink :: String -> String
  shortenLink str | Str.length str < 20 = str
                  | otherwise  = short
    where
      short = Str.take 7 str <> "..." <> Str.drop (Str.length str - 5) str
