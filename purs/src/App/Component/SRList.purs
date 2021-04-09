module App.Component.SRList where

import Prelude
import App.Component.AssetTransfer as AssetTransfer
import App.Model (SRList, AssetTransfer, initialList)
import Data.Array (cons, take)
import Data.Maybe (Maybe(..))
import Data.Symbol (SProxy(..))
import Effect.Aff.Class (class MonadAff)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Properties as HP

{-
  This component contains the list of all of the AssetCards, it is accumlated
  as a fold over the entire event history of the SuperRare contracts.
-}
_header :: SProxy "header"
_header = SProxy

data Query a
  = AddAssetTransfer AssetTransfer a

data Action
  = InitialAction

newtype Slots'
  = AssetTransferSlot Int

derive instance eqAssetTransferSlot :: Eq Slots'

derive instance ordAssetTransferSlot :: Ord Slots'

type Input
  = Unit

type Message
  = Void

srList ::
  forall m.
  MonadAff m =>
  H.Component HH.HTML Query Input Message m
srList =
  H.mkComponent
    { initialState: const initialList
    , render
    , eval
    }
  where
  render :: SRList -> H.ComponentHTML Action _ m
  render st =
    HH.div [ HP.class_ (HH.ClassName "sr-container") ]
      [ HH.div [ HP.class_ (HH.ClassName "sr-list") ]
          [ HH.ul_ (map renderTransfer st.transfers) ]
      ]

  renderTransfer ::
    { transferId :: Int
    , transfer :: AssetTransfer
    } ->
    HH.ComponentHTML Action _ m
  renderTransfer t =
    HH.slot
      _header
      (AssetTransferSlot t.transferId)
      (AssetTransfer.assetTransfer t.transfer)
      unit
      absurd

  eval ::
    forall i.
    H.HalogenQ Query Action i
      ~> H.HalogenM SRList Action _ Message m
  eval =
    H.mkEval
      H.defaultEval
        { handleQuery = handleQuery
        }

  handleQuery :: forall a. Query a -> H.HalogenM SRList Action _ Message m (Maybe a)
  handleQuery (AddAssetTransfer at next) = do
    _ <- H.modify (addTransfer at)
    pure $ Just next

-- | Adds a transfer to the current state.
addTransfer ::
  AssetTransfer ->
  SRList ->
  SRList
addTransfer at st =
  let
    newTransfer = { transferId: st.nextId, transfer: at }
  in
    st
      { nextId = st.nextId + 1
      , transfers = take 20 $ newTransfer `cons` st.transfers
      }