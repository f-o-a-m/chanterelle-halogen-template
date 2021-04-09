module App.Component.SRList where

import Prelude

import App.Component.AssetTransfer (AssetTransferQuery, assetTransfer)
import App.Model (SRList, AssetTransfer, initialList)
import Chanterelle.Internal.Utils (assertDirectory)
import Data.Array (cons, take)
import Data.Maybe (Maybe(..))
import Data.Symbol (SProxy(..))
import Effect.Aff.Class (class MonadAff)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Unsafe.Coerce (unsafeCoerce)
import Partial.Unsafe (unsafePartial)

{-
  This component contains the list of all of the AssetCards, it is accumlated
  as a fold over the entire event history of the SuperRare contracts.
-}

_header :: SProxy "srList header"
_header = SProxy

data SRListQuery a
  = AddAssetTransfer AssetTransfer a

newtype AssetTransferSlot = AssetTransferSlot Int
derive instance eqAssetTransferSlot :: Eq AssetTransferSlot
derive instance ordAssetTransferSlot :: Ord AssetTransferSlot

srList
  :: forall m.
     MonadAff m
  => H.Component HH.HTML SRListQuery Unit Void m
srList =
    H.mkComponent
      { initialState: const initialList
      , render
      , eval
      }
  where
    -- render
    --   :: SRList
    --   -> H.ParentHTML SRListQuery AssetTransferQuery AssetTransferSlot m
    render st =
      HH.div [HP.class_ (HH.ClassName "sr-container")]
        [ HH.div [HP.class_ (HH.ClassName "sr-list")]
          [HH.ul_ (map renderTransfer st.transfers)]
        ]

    renderTransfer
      :: { transferId :: Int
         , transfer :: AssetTransfer
         }
      -> _ -- H.Component SRListQuery AssetTransferQuery AssetTransferSlot m
    renderTransfer t =
      HH.slot
        _header
        (AssetTransferSlot t.transferId)
        (assetTransfer t.transfer)
        (unsafeCoerce unit)
        absurd

    -- eval
    --   :: SRListQuery ~> H.ParentDSL SRList SRListQuery AssetTransferQuery AssetTransferSlot Void m
    eval = H.mkEval H.defaultEval
      { handleAction = handleAction
      , initialize = Just (unsafeCoerce absurd) --FIXME
      }
       
    handleAction (AddAssetTransfer at next) = do
      _ <- H.modify (addTransfer at)
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
