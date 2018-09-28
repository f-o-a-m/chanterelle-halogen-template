module App.Component.SRList where

import Prelude

import App.Component.AssetTransfer (AssetTransferQuery, assetTransfer)
import App.Model (SRList, AssetTransfer, initialList)
import Control.Monad.Aff.Class (class MonadAff)
import Data.Array (cons, take)
import Data.Maybe (Maybe(..))
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP

{-
  This component contains the list of all of the AssetCards, it is accumlated
  as a fold over the entire event history of the SuperRare contracts.
-}

data SRListQuery a
  = AddAssetTransfer AssetTransfer a

newtype AssetTransferSlot = AssetTransferSlot Int
derive instance eqAssetTransferSlot :: Eq AssetTransferSlot
derive instance ordAssetTransferSlot :: Ord AssetTransferSlot

srList
  :: forall eff m.
     MonadAff eff m
  => H.Component HH.HTML SRListQuery Unit Void m
srList =
    H.parentComponent
      { initialState: const initialList
      , render
      , eval
      , receiver: const Nothing
      }
  where
    render
      :: SRList
      -> H.ParentHTML SRListQuery AssetTransferQuery AssetTransferSlot m
    render st =
      HH.div [HP.class_ (HH.ClassName "sr-container")]
        [ HH.div [HP.class_ (HH.ClassName "sr-list")]
          [HH.ul_ (map renderTransfer st.transfers)]
        ]

    renderTransfer
      :: { transferId :: Int
         , transfer :: AssetTransfer
         }
      -> H.ParentHTML SRListQuery AssetTransferQuery AssetTransferSlot m
    renderTransfer t =
      HH.slot
        (AssetTransferSlot t.transferId)
        (assetTransfer t.transfer)
        unit
        (HE.input absurd)

    eval
      :: SRListQuery ~> H.ParentDSL SRList SRListQuery AssetTransferQuery AssetTransferSlot Void m
    eval (AddAssetTransfer at next) = do
      H.modify (addTransfer at)
      pure next


-- | Adds a transfer to the current state.
addTransfer
  :: AssetTransfer
  -> SRList
  -> SRList
addTransfer at st =
  let newTransfer = {transferId: st.nextId, transfer: at}
  in st { nextId = st.nextId + 1
        , transfers = take 20 $ newTransfer `cons` st.transfers
        }
