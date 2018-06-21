module App.Component.List where

import Prelude

import App.Component.Task (AssetTransferQuery, assetTransfer)
import App.Model (List, TaskId, AssetTransfer, initialList, initialTask)
import Data.Array (snoc, filter, length)
import Data.Map as M
import Data.Maybe (Maybe(..), fromMaybe)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE

-- | The list component query algebra.
data ListQuery a
  = AddAssetTransfer AssetTransfer a

-- | The slot value that is filled by tasks during the install process.
newtype AssetTransferSlot = AssetTransferSlot TransferId
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
    HH.div_
      [ HH.h1_ [ HH.text "Todo list" ]
      , HH.p_
          [ HH.button
              [ HE.onClick (HE.input_ NewTask) ]
              [ HH.text "New Task" ]
          ]
      , HH.ul_ (map renderTask st.tasks)
      , HH.p_ [ HH.text $ show st.numCompleted <> " / " <> show (length st.tasks) <> " complete" ]
      , HH.button
          [ HE.onClick (HE.input_ AllDone) ]
          [ HH.text "All Done" ]
      ]

  renderTask :: TransferId -> H.ParentHTML ListQuery TaskQuery AssetTransferSlot m
  renderTask transferId =
    HH.slot
      (AssetTransferSlot transferId)
      (transfer initialTransfer)
      unit
      (HE.input (HandleTransferMessage transferId))

  eval :: ListQuery ~> H.ParentDSL List ListQuery AssetTransferQuery TransferSlot Void m
  eval (NewTask next) = do
    H.modify addTask
    pure next
  eval (AllDone next) = do
    toggled <- H.queryAll (H.action (ToggleCompleted true))
    H.modify $ updateNumCompleted (const (M.size toggled))
    pure next
  eval (HandleTaskMessage p msg next) = do
    case msg of
      NotifyRemove -> do
        wasComplete <- H.query (TaskSlot p) (H.request IsCompleted)
        when (fromMaybe false wasComplete) $ H.modify $ updateNumCompleted (_ `sub` 1)
        H.modify (removeTask p)
      Toggled b ->
        H.modify $ updateNumCompleted (if b then (_ + 1) else (_ `sub` 1))
    pure next

-- | Adds a task to the current state.
addTask :: List -> List
addTask st = st { nextId = st.nextId + 1, tasks = st.tasks `snoc` st.nextId }

-- | Removes a task from the current state.
removeTask :: TaskId -> List -> List
removeTask taskId st = st { tasks = filter (_ /= taskId) st.tasks }

-- | Updates the number of completed tasks.
updateNumCompleted :: (Int -> Int) -> List -> List
updateNumCompleted f st = st { numCompleted = f st.numCompleted }

