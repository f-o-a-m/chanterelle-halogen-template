module App.Component.List where

import Prelude

import App.Component.AssetTransfer (AssetTransferQuery, assetTransfer)
import App.Model (List, AssetTransfer, initialList)
import Data.Array (snoc)
import Data.Maybe (Maybe(..))
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP

-- | The list component query algebra.
data ListQuery a
  = AddAssetTransfer AssetTransfer a

-- | The slot value that is filled by tasks during the install process.
newtype AssetTransferSlot = AssetTransferSlot Int
derive instance eqAssetTransferSlot :: Eq AssetTransferSlot
derive instance ordAssetTransferSlot :: Ord AssetTransferSlot

-- | The list component definition.
list :: forall m. Applicative m => H.Component HH.HTML ListQuery Unit Void m
list =
  H.parentComponent
    { initialState: const initialList
    , render
    , eval
    , receiver: const Nothing
    }
  where

  render :: List -> H.ParentHTML ListQuery AssetTransferQuery AssetTransferSlot m
  render st =
    HH.div [HP.class_ (HH.ClassName "kitty-container")]
      [ HH.div [HP.class_ (HH.ClassName "kitty-list")]
        [HH.ul_ (map renderTask st.transfers)]
      ]

  renderTask :: {transferId :: Int, transfer :: AssetTransfer} -> H.ParentHTML ListQuery AssetTransferQuery AssetTransferSlot m
  renderTask t =
    HH.slot
      (AssetTransferSlot t.transferId)
      (assetTransfer t.transfer)
      unit
      (HE.input absurd)

  eval :: ListQuery ~> H.ParentDSL List ListQuery AssetTransferQuery AssetTransferSlot Void m
  eval (AddAssetTransfer at next) = do
    H.modify (addTask at)
    pure next


-- | Adds a task to the current state.
addTask :: AssetTransfer -> List -> List
addTask at st =
  let newTransfer = {transferId: st.nextId, transfer: at}
  in st { nextId = st.nextId + 1, transfers = st.transfers `snoc` newTransfer }
