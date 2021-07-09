module App.Component.Image where

import Prelude
import App.Model (Image, ImageLoadState(..), initialImage)
import Effect.Aff (Milliseconds(..), delay)
import Effect.Aff.Class (class MonadAff)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP

{-
  This component is responsible for rendering the image state. If the image is
  still loading, we indicate this. If the image failed to load, we increment a retry count
  and try again.
-}
data Query :: forall k. k -> Type
data Query a

data Action
  = LoadFailed
  | LoadSucceeded
  | RetryLoading

type Slots :: forall k. Row k
type Slots
  = ()

type Input
  = Unit

type Message
  = Void

imageComponent ::
  ∀ m.
  MonadAff m =>
  Image ->
  H.Component Query Input Message m
imageComponent initialState =
  H.mkComponent
    { initialState: const initialState
    , render
    , eval
    }
  where
  render :: forall s. Image -> H.ComponentHTML Action s m
  render img =
    let
      classNameModifiers = case img.loadStatus of
        Loading -> "Image--loading"
        Loaded -> "Image--loaded"
        Failed -> "Image--failed"

      renderImage i =
        HH.img
          [ HP.class_ (HH.ClassName "Image-img")
          , HP.src i.baseURL
          , HE.onError $ const $ LoadFailed
          , HE.onLoad $ const $ LoadSucceeded
          ]
    in
      HH.div [ HP.class_ (HH.ClassName $ "Image " <> classNameModifiers) ]
        $ case img.loadStatus of
            Loading -> [ renderImage img ]
            Loaded -> [ renderImage img ]
            Failed ->
              [ HH.div
                  [ HP.class_ (HH.ClassName "Image-error")
                  , HE.onClick $ const $ RetryLoading
                  ]
                  []
              ]

  eval ::
    forall i.
    H.HalogenQ Query Action i
      ~> H.HalogenM Image Action Slots Message m
  eval =
    H.mkEval
      $ H.defaultEval
          { handleAction = handleAction
          }
    where
    handleAction :: Action -> H.HalogenM Image Action Slots Message m Unit
    handleAction (LoadFailed) = do
      st <- H.get
      if st.loadTryCount > 0 then do
        H.liftAff $ delay (Milliseconds 1000.0)
        H.put st { loadTryCount = st.loadTryCount - 1 }
      else
        H.put st { loadStatus = Failed }

    handleAction (LoadSucceeded) = H.modify_ (_ { loadStatus = Loaded })

    handleAction (RetryLoading) = do
      st <- H.get
      H.put $ initialImage st.baseURL
