module App.Component.AssetTransfer where

import Prelude

import App.Component.Image (ImageQuery, image)
import App.Model (AssetTransfer, initialImage)
import Control.Monad.Aff.Class (class MonadAff)
import Data.Maybe (Maybe(..))
import Data.String as Str
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Network.Ethereum.Web3.Types (Address)

{-
  This component is for the AssetCard, which is comprised of the image and
  the asset metadata with links to etherscan.
-}

data AssetTransferQuery a
  = SelectUserAddress Address a

assetTransfer
  :: forall eff m.
     MonadAff eff m
  => AssetTransfer
  -> H.Component HH.HTML AssetTransferQuery Unit Void m
assetTransfer initialState =
    H.parentComponent
       { initialState: const initialState
       , render
       , eval
       , receiver: const Nothing
       }
  where
    render
      :: AssetTransfer
      -> H.ParentHTML AssetTransferQuery ImageQuery Unit m
    render at = HH.li [HP.class_ (HH.ClassName "sr-tile")]
      [ HH.div [HP.class_ (HH.ClassName "sr-pic")]
        [renderImage at.imageURL]
      , HH.div [HP.class_ (HH.ClassName "sr-info")]
        [ HH.div [HP.class_ (HH.ClassName "sr-info-headings")]
          [ HH.h6_ [HH.text "to: "]
          , HH.h6_ [HH.text "from: "]
          , HH.h6_ [HH.text "tokenId: "]
          , HH.h6_ [HH.text "transactionHash: "]
          , HH.h6_ [HH.text "blockNumber: "]
          ]
        , HH.div [HP.class_ (HH.ClassName "sr-info-details")]
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
      ]

    renderImage
      :: String
      -> H.ParentHTML AssetTransferQuery ImageQuery Unit m
    renderImage url =
      HH.slot
        unit
        (image $ initialImage url)
        unit
        (HE.input absurd)

    eval
      :: AssetTransferQuery ~> H.ParentDSL AssetTransfer AssetTransferQuery ImageQuery Unit Void m
    eval (SelectUserAddress _ next) = pure next

    addressLink address =
      HH.a [ HP.href $ "https://etherscan.io/address/" <> show address, HP.target "_blank" ]
           [ HH.text $ show address ]

    txLink txHash =
      HH.a [ HP.href $ "https://etherscan.io/tx/" <> show txHash, HP.target "_blank" ]
           [ HH.text $ shortenLink $ show txHash ]

    shortenLink
      :: String
      -> String
    shortenLink str | Str.length str < 20 = str
                    | otherwise  = short
      where
        short = Str.take 7 str <> "..." <> Str.drop (Str.length str - 5) str
