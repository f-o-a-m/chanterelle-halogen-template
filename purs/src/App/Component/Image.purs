module App.Component.Image where

import Prelude

import App.Model (Image, ImageLoadState(..), initialImage)
import Control.Monad.Aff (Milliseconds(..), delay)
import Control.Monad.Aff.Class (class MonadAff)
import Data.Maybe (Maybe(..))
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP

-- | The transfer component query algebra.
data ImageQuery a
  = LoadFailed a
  | LoadSucceeded a
  | RetryLoading a

-- | The transfer component definition.
image :: forall eff m. MonadAff eff m => Image -> H.Component HH.HTML ImageQuery Unit Void m
image initialState =
  H.component
    { initialState: const initialState
    , render
    , eval
    , receiver: const Nothing
    }
  where

  render :: Image -> H.ComponentHTML ImageQuery
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
           , HE.onError (HE.input_ LoadFailed)
           , HE.onLoad (HE.input_ LoadSucceeded)
           ]

    in HH.div [ HP.class_ (HH.ClassName $ "Image " <> classNameModifiers) ]
        $ case img.loadStatus of
            Loading ->
              [renderImage img]
            Loaded ->
              [renderImage img]
            Failed ->
              [ HH.div
                  [ HP.class_ (HH.ClassName "Image-error")
                  , HE.onClick (HE.input_ RetryLoading)
                  ] []
              ]

  eval :: ImageQuery ~> H.ComponentDSL Image ImageQuery Void m
  eval (LoadFailed next) = do
    st <- H.get
    if st.loadTryCount > 0
       then do
         H.liftAff $ delay (Milliseconds 1000.0)
         H.put st {loadTryCount = st.loadTryCount - 1}
         pure next
       else H.put st {loadStatus = Failed} *> pure next
  eval (LoadSucceeded next) = H.modify (_{loadStatus = Loaded}) *> pure next
  eval (RetryLoading next) = do
    st <- H.get
    H.put $ initialImage st.baseURL
    pure next
