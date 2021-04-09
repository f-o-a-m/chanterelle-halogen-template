module App.Component.AssetTransfer where

import Prelude

import App.Component.Image (ImageQuery, image, Input)
import App.Model (AssetTransfer, initialImage)
import Data.Maybe (Maybe(..))
import Data.String as Str
import Data.Symbol (SProxy(..))
import Effect (Effect)
import Effect.Aff.Class (class MonadAff)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Network.Ethereum.Web3.Types (Address, HexString)
import Unsafe.Coerce (unsafeCoerce)

{-
  This component is for the AssetCard, which is comprised of the image and
  the asset metadata with links to etherscan.
-}

data AssetTransferQuery a
  = SelectUserAddress Address a

_header :: SProxy "header"
_header = SProxy

assetTransfer
  :: forall m.
     MonadAff m
  => AssetTransfer
  -> H.Component HH.HTML AssetTransferQuery Input Void m
assetTransfer initialState =
    H.mkComponent
       { initialState: const initialState
       , render
       , eval
       }
  where
    -- render
    --   :: AssetTransfer
    --   -> _ AssetTransferQuery ImageQuery Input m
    render at = HH.li [HP.class_ (HH.ClassName "sr-tile")]
      [ HH.div [HP.class_ (HH.ClassName "sr-pic")]
        [ ] -- [ renderImage at.imageURL ]
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
                  --, HE.onClick \_ -> Just (SelectUserAddress at.to)
                  ] [] -- [ addressLink at.to ]
          , HH.h5 [ HP.class_ (HH.ClassName "user-address-link")
                  --, HE.onClick \_ -> Just (SelectUserAddress at.from)
                  ] [] -- [ addressLink at.from ]
          -- , HH.h5_ [HH.text $ show at.tokenId]
          -- , HH.h5_ [txLink at.transactionHash]
          -- , HH.h5_ [HH.text $ show $ at.blockNumber]
          ]
        ]
      ]

    -- renderImage
    --   :: String
    --   -> _ AssetTransferQuery ImageQuery Unit m
    -- renderImage url = --unsafeCoerce unit
    --   HH.slot
    --     _header
    --     unit
    --     (image $ initialImage url)
    --     (unsafeCoerce unit)
    --     (unsafeCoerce absurd)

    -- eval :: AssetTransferQuery ~> _ AssetTransfer AssetTransferQuery ImageQuery Unit Void m
    eval = H.mkEval H.defaultEval
      { handleAction = handleAction
      , initialize = Just (unsafeCoerce absurd) -- FIXME
      }
      
    handleAction (SelectUserAddress _ next) = pure next

    addressLink :: Address -> _
    addressLink address =
      HH.a [ HP.href $ "https://etherscan.io/address/" <> show address, HP.target "_blank" ]
           [ HH.text $ show address ]

    txLink :: HexString -> _
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
