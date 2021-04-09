module App.Component.Image where

import Prelude

import App.Model (Image, ImageLoadState(..), initialImage)
import Data.Maybe (Maybe(..))
import Effect.Aff (Milliseconds(..), delay)
import Effect.Aff.Class (class MonadAff)
import Halogen (HalogenQ(..))
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Unsafe.Coerce (unsafeCoerce)
import Data.Maybe
import Prim.RowList

{-

  This component is responsible for rendering the image state. If the image is
  still loading, we indicate this. If the image failed to load, we increment a retry count
  and try again.

-}

data Query a
  = LoadFailed a
  | LoadSucceeded a
  | RetryLoading a

data Action = ImageInitialize

type Input = Unit
type Message = Void

type EmptyRow = ()

image
  :: ∀ m.
     MonadAff m
  => Image -> H.Component HH.HTML Query Input Message m
image initialState =
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
         --  , HE.onError \_ -> Just LoadFailed -- (HE.input_ LoadFailed)
         --  , HE.onLoad \_ -> Just LoadSucceeded -- (HE.input_ LoadSucceeded)
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
               --   , HE.onClick \_ -> Just RetryLoading -- (HE.input_ RetryLoading)
                  ] []
              ]

  eval :: forall i. 
          H.HalogenQ Query Action i
       ~> H.HalogenM Image Action EmptyRow Message m
  eval = H.mkEval $ H.defaultEval
      { handleQuery = handleQuery
      , handleAction = handleAction
     -- , initialize = Just (ImageInitialize :: Action)
      }
    where
      handleQuery :: forall a. Query a -> H.HalogenM Image Action EmptyRow Message m (Maybe a)
      handleQuery (LoadFailed next) = do
        st <- H.get
        if st.loadTryCount > 0
          then do
            H.liftAff $ delay (Milliseconds 1000.0)
            H.put st {loadTryCount = st.loadTryCount - 1}
            pure (Just next)
          else H.put st {loadStatus = Failed} *> pure (Just next)
      handleQuery (LoadSucceeded next) = H.modify (_{loadStatus = Loaded}) *> pure (Just next)
      handleQuery (RetryLoading next) = do
        st <- H.get
        H.put $ initialImage st.baseURL
        pure (Just next)

      handleAction :: Action -> H.HalogenM Image Action () Message m Unit
      handleAction = pure (unsafeCoerce unit)