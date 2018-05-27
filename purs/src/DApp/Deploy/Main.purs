module DApp.Deploy.Main (main) where

import Prelude

import Chanterelle.Deploy (deploy)
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (CONSOLE)
import Control.Monad.Eff.Exception (EXCEPTION)
import DApp.Deploy.Script (deployScript)
import Data.Maybe (fromMaybe)
import Network.Ethereum.Web3 (ETH)
import Node.FS.Aff (FS)
import Node.Process (PROCESS)
import Node.Process as NP

main :: forall e.
        Eff ( console :: CONSOLE
            , eth :: ETH
            , fs :: FS
            , process :: PROCESS
            , exception :: EXCEPTION
            | e) Unit
main = do
  nodeUrl <- fromMaybe "http://localhost:8545" <$> NP.lookupEnv "NODE_URL"
  deploy nodeUrl 60 deployScript
