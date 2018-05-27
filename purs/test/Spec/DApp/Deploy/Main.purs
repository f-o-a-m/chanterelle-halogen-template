module Spec.DApp.Deploy.Main (main) where

import Prelude

import Chanterelle.Test (buildTestConfig)
import Control.Monad.Aff (launchAff)
import Control.Monad.Aff.AVar (AVAR)
import Control.Monad.Aff.Console (CONSOLE)
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Class (liftEff)
import Control.Monad.Eff.Unsafe (unsafeCoerceEff)
import Data.Maybe (Maybe(..), fromMaybe)
import DApp.Deploy.Script (deployScript) as Deploy
import Spec.DApp.Deploy.TokenSpec (tokenSpec) as Deploy
import Network.Ethereum.Web3 (ETH)
import Node.FS.Aff (FS)
import Node.Process as NP
import Test.Spec.Reporter.Console (consoleReporter)
import Test.Spec.Runner (PROCESS, run', defaultConfig)

-- | TODO: make the options for deploy config env vars
main
  :: forall e.
     Eff ( console :: CONSOLE
         , eth :: ETH
         , avar :: AVAR
         , fs :: FS
         , spec_process :: PROCESS
         , process :: NP.PROCESS
         | e
         ) Unit
main = void $ launchAff do
  nodeUrl <- liftEff $ fromMaybe "http://localhost:8545" <$> NP.lookupEnv "NODE_URL"
  testConfig <- buildTestConfig nodeUrl 60 Deploy.deployScript
  liftEff $ unsafeCoerceEff $ run' defaultConfig {timeout = Just (120 * 1000)} [consoleReporter] do
    Deploy.tokenSpec    $ testConfig
