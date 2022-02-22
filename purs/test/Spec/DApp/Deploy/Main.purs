module Spec.DApp.Deploy.Main (main) where

import Prelude
import Chanterelle.Test (buildTestConfig)
import DApp.Deploy.Script (deploy) as Deploy
import Data.Maybe (Maybe(..), fromMaybe)
import Effect (Effect)
import Effect.Aff (Milliseconds(..), launchAff)
import Effect.Class (liftEffect)
import Node.Process as NP
import Spec.DApp.Deploy.SimpleStorageSpec (simpleStorageSpec)
import Test.Spec.Reporter.Console (consoleReporter)
import Test.Spec.Runner (defaultConfig, runSpec')

main :: Effect Unit
main =
  void
    $ launchAff do
        nodeUrl <- liftEffect $ fromMaybe "http://localhost:8545" <$> NP.lookupEnv "NODE_URL"
        testConfig <- buildTestConfig nodeUrl 60 Deploy.deploy
        runSpec' defaultConfig { timeout = Just $ Milliseconds (120.0 * 1000.0) } [ consoleReporter ] do
              simpleStorageSpec $ testConfig
